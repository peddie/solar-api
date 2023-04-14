library(qs)
library(tidyverse)

source("api.R")
source("constants.R")

future::plan(future::multicore, workers = 2)
devices <- fetch_devices()
all_points <- fetch_all_points(devices)

solar_data_filename <- "solar_data.qs"
past_solar <- qs::qread(solar_data_filename)

last_time <-
    past_solar %>%
    dplyr::pull(timestamp) %>%
    max()

tabular <- fetch_point(
    all_points$device_type,
    all_points$point_id,
    last_time,
    lubridate::now()) %>%
    label_points(all_points) %>%
    pivot_for_plotting %>%
    compute_net_load
updated_solar_data <-
    past_solar %>%
    tibble::add_row(tabular) %>%
    dplyr::distinct(
               timestamp,
               .keep_all = TRUE)
qs::qsave(
        updated_solar_data,
        solar_data_filename)
