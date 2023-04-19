library(shiny)
library(magrittr)
library(plotly)

source("api.R", local = TRUE)
source("constants.R", local = TRUE)
source("bom.R", local = TRUE)
source("cost.R", local = TRUE)

options(browser = "x-www-browser")
options(shiny.host = "192.168.1.157")
options(shiny.port = 22222)
set_user_agent()

ui <- fluidPage(
    titlePanel("House dashboard"),

    sidebarLayout(
        sidebarPanel(
            helpText("Weather"),
            plotlyOutput("forecast_plot"),
            plotlyOutput("probability_plot"),
            plotlyOutput("temperature_plot"),
        ),

        mainPanel(
            helpText("Solar power info"),
            textOutput("current_time"),
            textOutput("current_gen"),
            textOutput("current_load"),
            textOutput("current_meter"),
            plotlyOutput("power_plot"),
            dateInput(
                "power_history_date",
                "Power history window",
                value = lubridate::today() - lubridate::ddays(7),
                format = "yyyy-mm-dd",
                min = "2023-03-23"),
            textOutput("power_sold"),
            plotlyOutput("past_power_plot"),
            h2("Power plan analysis"),
            shiny::dataTableOutput("plans"),
        )
    )
)

to_fetch <- tibble::tribble(
    ~device_type, ~point_id,
    7, 8018,
    1, 24
)

all_points <- fetch_devices() %>% fetch_all_points()

as_plotly <- function(ggobject) {
    ggobject %>%
        ggplotly() %>%
        plotly::config(
                    displayModeBar = FALSE) %>%
        plotly::layout(
                    legend = list(
                        orientation = "h",
                        xanchor = "center",
                        x = 0.5,
                        y = -0.3),
                    yaxis = list(
                        fixedrange = TRUE))
}

server <- function(input, output) {
    global <- shiny::reactiveValues(current_data = NULL)

    latest_data <- reactive({
        shiny::invalidateLater(millis = 300 * 1000)
        ## We don't want to keep fetching days of API data every time
        ## if the only thing that changed was the 5-minute timer.  So
        ## we save the current data in the `global` reactive store and
        ## compute the necessary bounds based on what we already have.
        start_time <- input$power_history_date
        end_time <- lubridate::now()
        isolate(current_data <- global$current_data)
        if (!is.null(current_data)) {
            earliest_cached <-
                current_data %>% dplyr::pull(timestamp) %>% min
            if (start_time >= earliest_cached) {
                print("Update of latest time")
                start_time <-
                    current_data %>% dplyr::pull(timestamp) %>% max
            } else {
                ## input$power_history_date changed, so we assume the
                ## timer did not trigger.
                print("Update of earliest time")
                end_time <- earliest_cached
            }
        } else {
            print("Initial fetch.")
        }
        tictoc::tic("iSolarCloud API call:")
        new_data <- fetch_point(
            to_fetch$device_type,
            to_fetch$point_id,
            start_time,
            end_time
        )
        toc_msg <- function(tic, toc, msg, info) {
            paste(msg,
                  "fetched",
                  nrow(new_data),
                  "points in",
                  round(toc - tic, 3),
                  "seconds")
        }
        tictoc::toc(func.toc = toc_msg)
        tictoc::tic("Formed updated frame:")
        update <- new_data %>%
            label_points(all_points) %>%
            pivot_for_plotting() %>%
            compute_net_load()
        merged <- dplyr::bind_rows(
                              current_data,
                             update) %>%
            dplyr::arrange(timestamp)
        global$current_data <- merged
        toc_msg <- function(tic, toc, msg, info) {
            paste(msg,
                  "formed new frame of",
                  nrow(merged),
                  "points in",
                  round(toc - tic, 3),
                  "seconds")
        }
        tictoc::toc(func.toc = toc_msg)
        return(merged)
    })
    historical_data <- reactive({
        print("Update of historical data.")
        latest_data() %>%
            dplyr::filter(
                       lubridate::date(timestamp) !=
                       lubridate::date(lubridate::today()) &
                       lubridate::date(timestamp) >= input$power_history_date)
    }) %>% shiny::bindCache(
                      lubridate::date(lubridate::today()),
                      input$power_history_date)
    
    latest_forecast <- reactive({
        shiny::invalidateLater(millis = 60 * 60 * 1000)
        tictoc::tic("BOM forecast fetch")
        latest <- bom_web_detailed_forecast(place_name = "kenmore") %>%
            tidy_forecast_tables()
        tictoc::toc()
        return(latest)
    })
    output$current_time <- renderText({
        latest_time <-
            dplyr::slice(latest_data(), which.max(timestamp))$`timestamp`
        paste("Updated at: ", latest_time)
    })
    output$current_gen <- renderText({
        latest_gen <-
            dplyr::slice(latest_data(), which.max(timestamp))$`Total Active Power`
        paste("Generation: ", latest_gen, "W")
    })
    output$current_load <- renderText({
        latest_load <-
            dplyr::slice(latest_data(), which.max(timestamp))$`Net Load`
        paste("Load: ", latest_load, "W")
    })
    output$current_meter <- renderText({
        latest_meter <-
            dplyr::slice(latest_data(), which.max(timestamp))$`Meter Active Power`
        direction <- if (latest_meter < 0) "exporting" else "buying"
        paste("Meter import: ", latest_meter, "W", paste0("(", direction, " power)"))
    })

    ## https://community.rstudio.com/t/plotly-have-both-geom-line-and-geom-smooth-in-the-legend-and-remove-the-trailing-text-1-at-the-end-of-the-legend-keys/105030
    output$power_plot <- renderPlotly({
        latest_data() %>%
            plot_power_compared_to_yesterday() %>%
            as_plotly()
    })
    output$past_power_plot <- renderPlotly({
        historical_data() %>%
            plot_past_power() %>%
            as_plotly()
    })
    output$forecast_plot <- renderPlotly({
        latest_forecast() %>%
            plot_rain_forecast() %>%
            as_plotly()
    })
    output$temperature_plot <- renderPlotly({
        latest_forecast() %>%
            plot_temperatures() %>%
            as_plotly()
    })
    output$probability_plot <- renderPlotly({
        latest_forecast() %>%
            plot_rain_probabilities() %>%
            as_plotly()
    })
    output$power_sold <- renderText({
        print("used historical data.")
        historical <- historical_data()
        mean_sold <-
            historical %>%
            compute_sold() %>%
            dplyr::pull(sold_kWh) %>%
            mean(na.rm = TRUE)
        days <- get_days(historical)
        paste("Mean power export over past", length(days), " days was ", sprintf("%.2f", mean_sold), "kWh / day.")
    })
    output$plans <- shiny::renderDataTable({
        calculate_costs(historical_data())
    })
}


shinyApp(ui = ui, server = server)
