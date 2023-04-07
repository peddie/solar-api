library(shiny)
library(magrittr)

source("api.R", local = TRUE)
source("constants.R", local = TRUE)

options(browser = "x-www-browser")
options(shiny.host = "192.168.1.157")
options(shiny.port = 22222)

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
            plotOutput("plot")
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
                           lubridate::now() - lubridate::ddays(2),
                           unit="day"),
            lubridate::now()) %>%
            label_points(all_points) %>%
            pivot_for_plotting() %>%
            compute_net_load()
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
        paste("Meter import: ", latest_meter, "W")
    })
    output$plot <- renderPlot({
        latest_data() %>% plot_power_compared_to_yesterday()
    })
}


shinyApp(ui = ui, server = server)
