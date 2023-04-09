set_user_agent <- function() {
    httr::set_config(httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36"))
}

bom_web_detailed_forecast <- function(place_name = "kenmore") {
    rvest::read_html(
              paste0("http://www.bom.gov.au/places/qld/",
                     place_name,
                     "/forecast/detailed/"))
}

bom_web_forecast <- function(place_name = "kenmore") {
    rvest::read_html(
              paste0("http://www.bom.gov.au/places/qld/",
                     place_name))
}

tidy_forecast_table <- function(table, date) {
    parsed_date <-
        date %>%
        stringr::str_sub(start = 2) %>%
        lubridate::ymd(tz = "Australia/Brisbane")

    renames <- c(
        `mm_50th_quantile` = "50% chance of more than (mm)",
        `mm_25th_quantile` = "25% chance of more than (mm)",
        `mm_10th_quantile` = "10% chance of more than (mm)",
        `rain_probability` = "Chance of any rain",
        `air_temp_C` = "Air temperature (°C)",
        `feels_like_temp_C` = "Feels like (°C)",
        `dew_point_temp_C` = "Dew point temperature (°C)",
        `uv_index` = "UV Index",
        `wind_speed_combined` = "Wind speed  km/hknots",
        `wind_direction` = "Wind direction",
        `relative_humidity` = "Relative humidity (%)",
        `forest_fuel_dryness_factor` = "Forest fuel dryness factor",
        `mixing_height_m` = "Mixing height (m)"
    )
    numeric_cols <- c(
        "mm_50th_quantile",
        "mm_25th_quantile",
        "mm_10th_quantile",
        "relative_humidity",
        "uv_index",
        "forest_fuel_dryness_factor",
        "mixing_height_m",
        "air_temp_C",
        "feels_like_temp_C",
        "dew_point_temp_C",
        "wind_speed_kmh",
        "wind_speed_knots"
    )
    factor_cols <- c(
        "Rain",
        "Thunderstorms",
        "Snow",
        "Fog",
        "Frost",
        "wind_direction"
    )
    parse_time_of_day <- function(str) {
        str %>% lubridate::parse_date_time("%I:%M %p") %>% hms::as_hms()
    }
    parse_numeric <- function(str) {
        str %>% readr::parse_number(na = c("", "–"))
    }
    parse_percent <- function(str) {
        str %>% parse_numeric %>% `/`(100)
    }
    parse_kmhknots <- function(str) {
        str %>%
            stringr::str_match("([:digit:]{2})([:digit:]+)") %>%
            .[,-1]
    }
    table %>%
        dplyr::distinct() %>%
        tidyr::pivot_longer(cols = -c(X1), names_to = "original_names") %>%
        tidyr::pivot_wider(names_from = c(X1)) %>%
        dplyr::select(-original_names) %>%
        dplyr::mutate(timestamp = parse_time_of_day(From) + parsed_date,
                      .before = `From`) %>%
        dplyr::rename(dplyr::all_of(renames)) %>%
        dplyr::mutate(
                   rain_probability = parse_percent(rain_probability),
                   wind_speed_combined = parse_kmhknots(wind_speed_combined),
                   wind_speed_kmh = wind_speed_combined[,1],
                   wind_speed_knots = wind_speed_combined[,2]) %>%
        dplyr::select(-c(wind_speed_combined, At, From)) %>%
        dplyr::mutate(across(all_of(numeric_cols), parse_numeric),
                      across(all_of(factor_cols), factor),
                      relative_humidity = relative_humidity / 100) %>%
        dplyr::filter(if_any(numeric_cols, ~ !is.na(.)))
}

tidy_forecast_tables <- function(detailed_html) {
    forecast_table_elements <-
        detailed_html %>%
        rvest::html_element("body div#pageheight div#pagewrap div#columns div#container div#content div#main-content") %>%
        rvest::html_elements("div.forecast-day")
    dates <-
        forecast_table_elements %>%
        rvest::html_attr("id")
    forecast_tables <-
        forecast_table_elements %>%
        rvest::html_table(header = FALSE)
    purrr::map2(forecast_tables, dates, tidy_forecast_table) %>%
        purrr::list_rbind()
}

plot_rain_forecast <- function(forecast) {
    colors <- c(
        "10%" = grDevices::rgb(0, 0.1, 1, 0.1),
        "25%" = grDevices::rgb(0, 0.25, 1, 0.3),
        "50%" = grDevices::rgb(0, 0.5, 1, 0.6)
    )
    forecast %>%
        dplyr::filter(!is.na(mm_10th_quantile)) %>%
        ggplot2::ggplot(ggplot2::aes(x = timestamp)) +
        ggplot2::geom_area(
                     ggplot2::aes(y = mm_10th_quantile,
                                  fill = "10%"),
                                  color = "blue") +
        ggplot2::geom_area(
                     ggplot2::aes(y = mm_25th_quantile,
                                  fill = "25%")) +
        ggplot2::geom_area(
                     ggplot2::aes(y = mm_50th_quantile,
                                  fill = "50%")) +
        ggplot2::scale_x_datetime(
                     labels = scales::date_format("%a %b %d %H:%M")) +
        ggplot2::scale_fill_manual(values = colors) +
        ggplot2::scale_color_manual(values = colors) +
        ggplot2::coord_cartesian(ylim = c(0, NA), expand = TRUE) +
        ggplot2::labs(
                     x = "Time",
                     y = "Rain amount [mm]",
                     fill = "Probability of rain",
                     title = "BOM rain forecast")
}