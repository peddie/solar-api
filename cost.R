### Cost calculations

solarmax_plan <- c(
    base_rate = 0.2697,
    daily_cost = 1.0395,
    peak_rate = 0,
    export_rate = 0.1)

flex_plan <- c(
    base_rate = 0.2697,
    daily_cost = 1.0395,
    peak_rate = 0,
    export_rate = 0.066)

demand_plan <- c(
    base_rate = 0.2497,
    daily_cost = 1.0857,
    peak_rate = 0.13,
    export_rate = 0.066)

append_delta_time <- function(tabular) {
    tabular %>%
        dplyr::filter(!is.na(timestamp)) %>%
        dplyr::arrange(timestamp) %>%
        dplyr::mutate(
                   dt_hours = lubridate::time_length(
                                       timestamp - dplyr::lag(timestamp),
                                       unit = "hour")) %>%
        dplyr::filter(!is.na(dt_hours))
}

compute_sold <- function(tabular) {
    tabular %>%
        dplyr::filter(`Meter Active Power` < 0) %>%
        dplyr::arrange(timestamp) %>%
        append_delta_time() %>%
        dplyr::mutate(day = to_day(timestamp)) %>%
        dplyr::group_by(day) %>%
        dplyr::summarise(
                   sold_kWh = -sum(dt_hours * `Meter Active Power` * 1e-3))
}

is_peak <- function(timestamp) {
    hms::as_hms(timestamp) > lubridate::hms("16:00:00") &
        hms::as_hms(timestamp) < lubridate::hms("21:00:00")
}

calc_off_peak <- function(power) {
    dplyr::if_else(power < 0, 0, 1.0)
}

calc_export <- function(power) {
    dplyr::if_else(power < 0, 1.0, 0)
}

calc_peak_cost <- function(power, plan) {
    slider::slide_dbl(
                power,
                ~sum(. * plan[["peak_rate"]], na.rm = TRUE),
                .before = 5,
                .complete = TRUE) %>%
        max(na.rm = TRUE)
}

calculate_cost <- function(tabular, plan) {
    tabular %>%
        append_delta_time() %>%
        dplyr::mutate(
               day = to_day(timestamp),
               peak = is_peak(timestamp),
               kWh = dt_hours * `Meter Active Power` / 1e3,
               off_peak_cost =
                   kWh * calc_off_peak(`Meter Active Power`) * plan[["base_rate"]],
               export_cost =
                   kWh * calc_export(`Meter Active Power`) * plan[["export_rate"]]
               ) %>%
        dplyr::group_by(day) %>%
        dplyr::summarise(
                   peak_cost =
                       calc_peak_cost(kWh, plan),
                   daily_cost = plan[["daily_cost"]],
                   across(c(
                       off_peak_cost,
                       export_cost),
                       ~sum(., na.rm = TRUE)),
                   net_cost =
                       peak_cost + daily_cost + off_peak_cost + export_cost) %>%
        dplyr::relocate(net_cost, .before = dplyr::everything())
}

calculate_costs <- function(tabular) {
    tibble::tribble(
                ~plan_name, ~plan,
                "demand", demand_plan,
                "solarmax", solarmax_plan,
                "flex", flex_plan) %>%
        dplyr::mutate(
                   cost =
                       purrr::map(plan, \(p) calculate_cost(tabular, p))) %>%
        tidyr::unnest(cost) %>%
        dplyr::group_by(plan_name) %>%
        dplyr::summarise(across(-c(day, plan), ~mean(., na.rm = TRUE)))
}


