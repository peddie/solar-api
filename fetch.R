library(qs)
library(tidyverse)

source("api.R")
source("constants.R")
source("cost.R")
source("bom.R")

future::plan(future::multicore, workers = 2)

devices <- fetch_devices()

all_points <- fetch_all_points(devices)

all_data <- fetch_point(
    all_points$device_type,
    all_points$point_id,
    lubridate::now() - lubridate::ddays(22),
    lubridate::now())

named <- label_points(all_data, all_points)
tabular <-
    named %>% pivot_for_plotting %>% compute_net_load

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

### BOM fetching

set_user_agent()
forecast <-
    bom_web_detailed_forecast() %>%
    tidy_forecast_tables() %>%
    dplyr::mutate(fetch_time = lubridate::round_date(
                                              lubridate::now(),
                                              unit = "hour"))

### Data update
solar_data_filename <- "solar_data.qs"
past_solar <- qs::qread(solar_data_filename)
updated_solar_data <-
    past_solar %>%
    tibble::add_row(tabular) %>%
    dplyr::distinct(
               timestamp,
               .keep_all = TRUE)
qs::qsave(
        updated_solar_data,
        solar_data_filename)

forecast_data_filename <- "forecast_data.qs"
past_forecasts <- qs::qread(forecast_data_filename)
updated_forecast_data <-
    past_forecasts %>%
    tibble::add_row(forecast) %>%
    dplyr::distinct(
               timestamp,
               .keep_all = TRUE)
qs::qsave(
        updated_forecast_data,
        forecast_data_filename)

### Plot generation

tabular %>% plot_power()
