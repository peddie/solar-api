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
