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
    latest_data <- reactive({
        shiny::invalidateLater(millis = 300 * 1000)
        fetch_point(
            to_fetch$device_type,
            to_fetch$point_id,
            input$power_history_date,
            lubridate::now()
        ) %>%
            label_points(all_points) %>%
            pivot_for_plotting() %>%
            compute_net_load()
    })
    latest_forecast <- reactive({
        shiny::invalidateLater(millis = 60 * 60 * 1000)
        bom_web_detailed_forecast(place_name = "kenmore") %>%
            tidy_forecast_tables()
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
        latest_data() %>%
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
        latest <- latest_data()
        mean_sold <-
            latest %>%
            compute_sold() %>%
            dplyr::pull(sold_kWh) %>%
            mean(na.rm = TRUE)
        days <- get_days(latest)
        paste("Mean power export over past", length(days), " days was ", sprintf("%.2f", mean_sold), "kWh / day.")
    })
    output$plans <- shiny::renderDataTable({
        calculate_costs(latest_data())
    })
}


shinyApp(ui = ui, server = server)
