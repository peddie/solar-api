library(qs)
library(magrittr)
library(tidyverse)

source("bom.R")

### BOM fetching

set_user_agent()
forecast <-
    bom_web_detailed_forecast() %>%
    tidy_forecast_tables() %>%
    dplyr::mutate(fetch_time = lubridate::round_date(
                                              lubridate::now(),
                                              unit = "3 hour"))
forecast_data_filename <- "forecast_data.qs"
past_forecasts <- qs::qread(forecast_data_filename)
updated_forecast_data <-
    past_forecasts %>%
    tibble::add_row(forecast) %>%
    dplyr::distinct(
               timestamp, fetch_time,
               .keep_all = TRUE)
qs::qsave(
        updated_forecast_data,
        forecast_data_filename)
