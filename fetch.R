library(qs)
library(tidyverse)

source("api.R")
source("constants.R")
source("cost.R")

future::plan(future::multicore, workers = 2)

devices <- fetch_devices()

all_points <- fetch_all_points(devices)

all_data <- fetch_point(
    all_points$device_type,
    all_points$point_id,
    lubridate::now() - lubridate::ddays(13),
    lubridate::now())

named <-
    all_data %>%
    dplyr::left_join(
               y = all_points %>%
                   select(c(point_id_p, device_type, point_name)),
               by = join_by(point_id == "point_id_p"))
tabular <-
    named %>%
    dplyr::select(-c(key, point_id, device_type)) %>%
    tidyr::pivot_wider(
               names_from = point_name,
               values_from = value) %>%
    dplyr::mutate(`Net Load` = `Meter Active Power` + `Total Active Power`)
days <-
    tabular %>%
    dplyr::pull(timestamp) %>%
    lubridate::round_date(unit = "day") %>%
    unique()

color_mappings <-
    c("Meter Usage" = "red",
      "Generation" = "green",
      "Load" = "blue")
tabular %>%
    # dplyr::filter(timestamp > lubridate::now() - lubridate::ddays(1)) %>%
    ggplot(aes(x = timestamp)) +
    geom_ribbon(
        aes(ymin = pmin(0, `Meter Active Power`),
            ymax = pmax(0, `Meter Active Power`),
            fill = "Meter Usage"),
        alpha = 0.3) +
    geom_area(
        aes(y = `Total Active Power`,
            fill = "Generation"),
        alpha = 0.3) +
    geom_area(
        aes(y = `Net Load`,
            fill = "Load"),
        alpha = 0.3) +
    scale_color_manual(values = color_mappings) +
    scale_fill_manual(values = color_mappings) +
    scale_x_datetime(breaks = days) +
    labs(
        x = "Date",
        y = "Power [W]",
        fill = "Energy Flow",
        title = "Household power consumption",
        subtitle = "Negative power indicates net power export")

### Cost calculations
costed <-
    tabular %>%
    dplyr::filter(!is.na(timestamp)) %>%
    dplyr::arrange(timestamp) %>%
    dplyr::mutate(
               previous_time = lag(timestamp),
               period = timestamp - lag(timestamp),
               peak = is_peak(timestamp),
               kWh = period_hours(period) * `Meter Active Power` / 1e3,
               fixed_base_rate = constant_rate(`Meter Active Power`, fixed_base_rate),
               demand_base_rate = constant_rate(`Meter Active Power`, demand_base_rate),
               fixed_base_cost = fixed_base_rate * kWh,
               demand_base_cost = demand_base_rate * kWh
           )
peak <-
    costed %>%
    dplyr::filter(peak) %>%
    dplyr::mutate(date = lubridate::date(timestamp)) %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(
               max_half_hour = max(
                   slider::slide_dbl(
                               kWh,
                               ~sum(. * demand_rate, na.rm = TRUE),
                               .before = 5,
                               .complete = TRUE), na.rm = TRUE)
           )
n_days <- nrow(peak)
fixed_cost <-
    sum(costed$fixed_base_cost, na.rm = TRUE) +
    n_days * fixed_daily_cost
demand_cost <-
    sum(costed$demand_base_cost, na.rm = TRUE) +
    sum(peak$max_half_hour, na.rm = TRUE) +
    n_days * demand_daily_cost
daily_difference <- (fixed_cost - demand_cost) / n_days