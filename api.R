format_timestamp <- function (datetime, round_unit = "5 minutes") {
    if (is.null(round_unit)) {
        round_unit <- "second"
    }
    realdate <- lubridate::round_date(datetime, round_unit)
    format(realdate, "%Y%m%d%H%M%S")
}


device_type_to_key <- function(device_type, ps = ps_id) {
    paste0(ps, "_", device_type, "_1_1")
}

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

fetch_points <- function(device_type, id = ps_id) {
    system2(
        client,
        c("data",
          "csv",
          "getPowerDevicePointNames",
          paste0("DeviceType:", device_type)),
        stdout = TRUE) %>%
        parse_response
}

fetch_all_points <- function(device_types, id = ps_id) {
    renames <- c(
        point_id = "Point Id",
        point_name = "Point Name",
        point_cal_type = "Point Cal Type")

    fetch_point <- function(device_type) {
        fetch_points(device_type) %>%
            dplyr::mutate(
                       device_type = device_type) %>%
            dplyr::rename(dplyr::any_of(renames))
    }

    device_types %>%
        .$`Device Type` %>%
        purrr::map(fetch_point) %>%
        purrr::list_rbind() %>%
        dplyr::mutate(point_id_p = paste0("p", point_id))
}

fetch_point <- function(dtype, point_id, start_date, end_date) {
    furrr::future_map2(
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

label_points <- function(data, all_points) {
    data %>%
        dplyr::left_join(
                   y = all_points %>%
                       dplyr::select(c(point_id_p, device_type, point_name)),
                   by = dplyr::join_by(point_id == "point_id_p"))
}

pivot_for_plotting <- function(named) {
    named %>%
        dplyr::select(-c(key, point_id, device_type)) %>%
        tidyr::pivot_wider(
                   names_from = point_name,
                   values_from = value)

}

compute_net_load <- function(tabular) {
    tabular %>%
        dplyr::mutate(
                   `Net Load` = `Meter Active Power` + `Total Active Power`)
}

to_day <- function(timestamp) {
    lubridate::as_date(lubridate::floor_date(timestamp, unit = "day"))
}

get_days <- function(tabular) {
    tabular %>%
        dplyr::pull(timestamp) %>%
        to_day() %>%
        unique()
}

plot_power <- function(tabular) {
    color_mappings <-
        c("Meter Usage" = "red",
          "Generation" = "green",
          "Load" = "blue")
    days <- get_days(tabular)
    tabular %>%
        ggplot2::ggplot(aes(x = timestamp)) +
        ggplot2::geom_ribbon(
                     ggplot2::aes(ymin = pmin(0, `Meter Active Power`),
                                  ymax = pmax(0, `Meter Active Power`),
                                  fill = "Meter Usage"),
                     alpha = 0.3) +
        ggplot2::geom_area(
                     ggplot2::aes(y = `Total Active Power`,
                                  fill = "Generation"),
                     alpha = 0.3) +
        ggplot2::geom_area(
                     ggplot2::aes(y = `Net Load`,
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
}

plot_power_compared_to_yesterday <- function(tabular) {
    today <-
        lubridate::floor_date(
                       lubridate::now(),
                       unit = "day")
    today_data <-
        tabular %>%
        dplyr::filter(lubridate::date(timestamp) == today)
    old_data <-
        tabular %>%
        dplyr::filter(lubridate::date(timestamp) != today) %>%
        dplyr::mutate(days_offset =
                          lubridate::date(today) - lubridate::date(timestamp),
                      timestamp_today = timestamp + days_offset)
    days_offsets <-
        old_data %>%
        dplyr::pull(days_offset) %>%
        unique()
    color_mappings <-
        c("Meter Usage" = "red",
          "Generation" = "green",
          "Load" = "blue",
          "Previous generation" = "black")
    total_days_offsets <- sum(days_offsets)
    max_days_offsets <- max(days_offsets)
    today_data %>%
        ggplot2::ggplot(ggplot2::aes(x = timestamp)) +
        ggplot2::geom_ribbon(
                     ggplot2::aes(ymin = pmin(0, `Meter Active Power`),
                                  ymax = pmax(0, `Meter Active Power`),
                                  fill = "Meter Usage"),
                     alpha = 0.3) +
        ggplot2::geom_area(
                     ggplot2::aes(y = `Total Active Power`,
                                  fill = "Generation"),
                     alpha = 0.3) +
        ggplot2::geom_area(
                     ggplot2::aes(y = `Net Load`,
                                  fill = "Load"),
                     alpha = 0.3) +
        ggplot2::geom_area(
                     data = old_data %>% dplyr::mutate(date = lubridate::date(timestamp)),
                     ggplot2::aes(x = timestamp_today,
                                  y = `Total Active Power`,
                                  group = date),
                     position = "identity",
                     alpha = 0.5 / max_days_offsets) +
        ggplot2::scale_color_manual(values = color_mappings) +
        ggplot2::scale_fill_manual(values = color_mappings) +
        ggplot2::scale_x_datetime(
                     breaks = scales::date_breaks("6 hours"),
                     labels = scales::date_format("%a %H:%M")) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(
                                                  angle = 90,
                                                  vjust = 0.5)) +
        ggplot2::labs(
                     x = "Date",
                     y = "Power [W]",
                     fill = "Energy Flow",
                     colour = paste("Past", max_days_offsets, "Days"),
                     group = "Past generation",
                     title = "Household power consumption",
                     subtitle = paste0(
                         "Negative power indicates net power export",
                         "\n",
                         "Shadows indicate past ", max_days_offsets, " days of generation"))
}

plot_past_power <- function(tabular) {
    today <-
        lubridate::floor_date(
                       lubridate::now(),
                       unit = "day")
    days <-
        tabular %>%
        dplyr::pull(timestamp) %>%
        lubridate::date() %>%
        Filter(function(d) d != lubridate::date(today), .) %>%
        unique()
    n_days <- length(days)
    tabular %>%
        dplyr::filter(lubridate::date(timestamp) != lubridate::date(today)) %>%
        dplyr::mutate(
                   time = hms::as_hms(timestamp),
                   date = lubridate::date(timestamp)) %>%
        dplyr::group_by(date) %>%
        ggplot(aes(x = time)) +
        geom_area(
            aes(y = `Total Active Power`,
                fill = date,
                group = date),
            alpha = 0.2,
            position = "identity") +
        coord_cartesian(
            xlim = c(
                lubridate::hms("05:00:00"),
                lubridate::hms("19:00:00"))) +
        scale_fill_viridis_c(trans = "date") +
        ggplot2::scale_x_time(
                     breaks = scales::date_breaks("3 hours"),
                     labels = scales::time_format("%H:%M")) +
        ggplot2::theme(
                     aspect.ratio = 0.2,
                     axis.text.x = ggplot2::element_text(
                                                angle = 90,
                                                vjust = 0.5)) +
        labs(
            x = "Time of day",
            y = "Power [W]",
            fill = "Date",
            title = paste("Past", n_days, "days of power generation"))
}