library(shiny)
library(magrittr)
library(plotly)

source("api.R", local = TRUE)
source("constants.R", local = TRUE)
source("bom.R", local = TRUE)

options(browser = "x-www-browser")
options(shiny.host = "192.168.1.157")
options(shiny.port = 22222)
set_user_agent()

ui <- fluidPage(
    titlePanel("House dashboard"),

    sidebarLayout(
        sidebarPanel(
            helpText("Power stats"),
            textOutput("current_time"),
            textOutput("current_gen"),
            textOutput("current_load"),
            textOutput("current_meter")
        ),

        mainPanel(
            plotlyOutput("plot"),
            plotlyOutput("past_power_plot"),
            plotlyOutput("forecast_plot")
        )
    )
)

to_fetch <- tibble::tribble(
    ~device_type, ~point_id,
    7, 8018,
    1, 24
)

all_points <- fetch_devices() %>% fetch_all_points()

server <- function(input, output) {
    latest_data <- reactive({
        shiny::invalidateLater(millis = 300 * 1000)
        fetch_point(
            to_fetch$device_type,
            to_fetch$point_id,
            lubridate::floor_date(
                           lubridate::now() - lubridate::ddays(7),
                           unit="day"),
            lubridate::now()) %>%
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
    output$plot <- renderPlotly({
        latest_data() %>% plot_power_compared_to_yesterday() %>% ggplotly()
    })
    output$past_power_plot <- renderPlotly({
        latest_data() %>% plot_past_power() %>% ggplotly()
    })
    output$forecast_plot <- renderPlotly({
        latest_forecast() %>% plot_rain_forecast() %>% ggplotly()
    })
}


shinyApp(ui = ui, server = server)
