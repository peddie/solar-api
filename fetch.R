library(qs)
library(tidyverse)

format_timestamp <- function (datetime, round_unit = "5 minutes") {
    if (is.null(round_unit)) {
        round_unit <- "second"
    }
    realdate <- lubridate::round_date(datetime, round_unit)
    format(realdate, "%Y%m%d%H%M%S")
}


client <- "GoSungrow"
ps_id <- 1212467

device_type_to_key <- function(device_type, ps = ps_id){
    paste0(ps, "_", device_type, "_1_1")
}

inverter_device_type <- "1"
inverter_key <- device_type_to_key(inverter_device_type, ps = ps_id)
meter_device_type <- "7"
meter_key <- device_type_to_key(meter_device_type, ps = ps_id)

sungrow_csv_data_args <- function(key,
                                  point,
                                  start_date = lubridate::now() - lubridate::ddays(1),
                                  end_date = lubridate::now()) {
    colon <- function(x, y) {
        paste0(x, ":", y)
    }
    c("data", "csv", "queryMutiPointDataList",
      colon("PsId", ps_id),
      colon("Points", paste0(key, ".", point)),
      colon("MinuteInterval", 5),
      colon("StartTimeStamp", format_timestamp(start_date)),
      colon("EndTimeStamp", format_timestamp(end_date)))
}

parse_response <- function(lines) {
    readr::read_csv(I(lines),
                    skip = 3,
                    show_col_types = FALSE,
                    na = c("--", ""))
}

fetch <- function(key,
                  point,
                  start_date = lubridate::now() - lubridate::ddays(1),
                  end_date = lubridate::now()) {
    args <- sungrow_csv_data_args(
        key,
        point,
        start_date,
        end_date)
    res <- system2(client,
            args,
            stdout = TRUE,
            stderr = FALSE) %>%
        parse_response

    renames <- c(value = paste0(key, ".", point),
                 key = "Ps Key",
                 timestamp = "Timestamp")

    # print(paste(args, "-->", nrow(res)))
    if (nrow(res) > 0) {
        res %>%
        dplyr::rename(dplyr::any_of(renames)) %>%
        dplyr::mutate(point_id = point) %>%
            dplyr::filter(!is.na(value))
    } else {
        res
    }
}

fetch_devices <- function(id = ps_id) {
    system2(
        client,
        c("data",
          "csv",
          "getDeviceList"),
        stdout = TRUE) %>%
        parse_response
}

fetch_all_points <- function(id = ps_id) {
    renames <- c(
        point_id = "Point Id",
        point_name = "Point Name",
        point_cal_type = "Point Cal Type")

    fetch_point <- function(device_type) {
        fetch_points(device_type) %>%
            parse_response %>%
            dplyr::mutate(
                       device_type = device_type) %>%
            dplyr::rename(dplyr::any_of(renames))
    }

    fetch_devices(id = ps_id) %>%
        parse_response %>%
        .$`Device Type` %>%
        purrr::map(fetch_point) %>%
        purrr::list_rbind() %>%
        dplyr::mutate(point_id_p = paste0("p", point_id))
}

fetch_point <- function(dtype, point_id, start_date, end_date) {
    purrr::map2(
               .x = dtype,
               .y = point_id,
               .f = \(dt, pid) fetch(
                                   device_type_to_key(dt),
                                   paste0("p", pid),
                                   start_date,
                                   end_date),
               .progress = TRUE) %>%
        purrr::list_rbind()
}

devices <- fetch_devices()
all_points <- fetch_all_points()
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

color_mappings <-
    c("Meter Usage" = "red",
      "Generation" = "green",
      "Load" = "blue")

days <- tabular %>% dplyr::pull(timestamp) %>% lubridate::round_date(unit = "day") %>% unique()

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

fixed_base_rate <- 0.2697
demand_base_rate <- 0.2497
fixed_daily_cost <- 1.0395
demand_daily_cost <- 1.0857
demand_rate <- 0.13
export_rate <- 0.10

is_peak <- function(timestamp) {
    hms::as_hms(timestamp) > lubridate::hms("16:00:00") &
        hms::as_hms(timestamp) < lubridate::hms("21:00:00")
}
constant_rate <- function(power, base_rate) {
    if_else(power < 0, -export_rate, base_rate)
}
period_hours <- function(dt) {
    (dt %>% as.period %>% period_to_seconds) / 3600
}

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