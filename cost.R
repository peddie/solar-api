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

