library(shiny)
library(tidyverse)
library(readr)
library(viridis)
library(flexdashboard)
library(plotly)
library(leaflet)
library(htmlwidgets)

#Load in data 

taxi_data = read_csv("./data/transport_final.csv")

tp_data = 
    taxi_data %>% 
    separate(pickup_time, into = c("pickup_hr", "pickup_min", "pickup_sec"), sep = ":") %>% 
    separate(dropoff_time, into = c("dropoff_hr", "dropoff_min", "dropoff_sec"), sep = ":") %>% 
    select(-pickup_sec, -dropoff_min, -pickup_min, -dropoff_sec) %>% 
    mutate(
        pickup_hr = as.numeric(pickup_hr),
        dropoff_hr = as.numeric(dropoff_hr)
    )

boros = tp_data %>% distinct(pu_boro) %>% drop_na(pu_boro) %>% pull()

tp_type = tp_data %>% distinct(type) %>% drop_na(type) %>% pull()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Taxi Distribution Explorer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "boro_choice", 
                label = h3("Select pick-up borough"),
                choices = boros, selected = "Manhattan"),
            
            selectInput(
                "day",
                label = h3("Choose day"),
                choices = c("2019-02-14","2019-02-15"), selected = "2019-02-14"),
            
            sliderInput(
                "time_range", 
                label = h3("Choose hour range"), 
                min = 0, max = 23, value = c(18, 21)),
            radioButtons(
                "car_type", 
                label = h3("Select transportation type"),
                choices = tp_type, selected = "yellow")),
        
        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(type = "tab",
                              tabPanel("Map drop-offs", leafletOutput("mapPlot")),
                              tabPanel("Summary drop-offs", 
                                       fluidRow(
                                           column(10, plotlyOutput("summary_do")),
                                           column(10, plotlyOutput("avg_dist_do")),
                                           column(10, plotlyOutput("avg_fare_do")))),
                              tabPanel("Map pick-ups", leafletOutput("mapPlot2")),
                              tabPanel("Summary pick-ups", 
                                       fluidRow(
                                           column(10, plotlyOutput("summary_pu")),
                                           column(10, plotlyOutput("avg_dist_pu")),
                                           column(10, plotlyOutput("avg_fare_pu"))))
))))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mapPlot <- renderLeaflet({
        tp_data %>%
            filter(pu_boro == input[["boro_choice"]],
                   pickup_date == input[["day"]],
                   pickup_hr %in% input$time_range[1]:input$time_range[2],
                   type == input$car_type) %>% 
            group_by(do_neiborhood, x.y, y.y, type) %>% 
            summarize(num = n(),
                      avg_distance = round(mean(trip_distance),2),
                      avg_fare = round(mean(fare_amount),2)) %>% 
            mutate(avg_fare = ifelse(is.na(avg_fare), 0, avg_fare),
                   avg_distance = ifelse(is.na(avg_distance), 0, avg_distance)) %>% 
            mutate(label = str_c("Drop-off neighborhood:", do_neiborhood, 
                                 "</b><br>", "Count: ", num, " drop-offs",
                                 "</b><br>", "Mean distance: ", avg_distance, " miles",
                                 "</b><br>", "Mean fare: $", avg_fare)) %>% 
            leaflet() %>%
            addTiles() %>% 
            addProviderTiles(providers$CartoDB.Positron) %>% 
            addCircleMarkers(~x.y, ~y.y, 
                             radius = ~num/15,
                             popup = ~label,
                             color = ~type)
        })
    
    
    output$summary_do <- renderPlotly({
        ggplotly(tp_data %>%
            filter(pu_boro == input[["boro_choice"]],
                   pickup_date == input[["day"]],
                   pickup_hr %in% input$time_range[1]:input$time_range[2],
                   type == input$car_type) %>% 
            group_by(do_neiborhood) %>%
            summarize(freq = n()) %>% 
            top_n(n = 15) %>% 
            mutate(do_neiborhood = fct_reorder(do_neiborhood, freq)) %>% 
            ggplot(aes(x = do_neiborhood, 
                       y = freq, 
                       fill = freq)) +
            geom_bar(stat = "identity") +
            viridis::scale_fill_viridis(
                begin = 1, end = 0) + 
            coord_flip() +
            labs(
                title = "Top 10 drop-off neighborhoods in Manhattan from chosen borough",
                x = "",
                y = "Number of drop-offs") + 
            theme_bw() +   
            theme(legend.position = "none",    
                  plot.title = element_text(hjust = 0.5, size=12, face='bold')))
    })
    
    output$avg_fare_do <- renderPlotly({
        ggplotly(tp_data %>%
                     filter(pu_boro == input[["boro_choice"]],
                            pickup_date == input[["day"]],
                            pickup_hr %in% input$time_range[1]:input$time_range[2],
                            type == input$car_type) %>% 
                     group_by(do_neiborhood) %>%
                     summarize(avg_fare = mean(fare_amount)) %>% 
                     top_n(n = 15) %>% 
                     mutate(do_neiborhood = fct_reorder(do_neiborhood, avg_fare)) %>% 
                     ggplot(aes(x = do_neiborhood, 
                                y = avg_fare, 
                                fill = avg_fare)) +
                     geom_bar(stat = "identity") +
                     viridis::scale_fill_viridis(
                         begin = 1, end = 0) + 
                     coord_flip() +
                     labs(
                         title = "Top 10 Manhattan Neighborhoods with Highest Average Fare Amount",
                         x = "",
                         y = "Average fare amount ($)") + 
                     theme_bw() +   
                     theme(legend.position = "none",    
                           plot.title = element_text(hjust = 0.5, size=11, face='bold')))
    })
    
    output$avg_dist_do <- renderPlotly({
        ggplotly(tp_data %>%
                     filter(pu_boro == input[["boro_choice"]],
                            pickup_date == input[["day"]],
                            pickup_hr %in% input$time_range[1]:input$time_range[2],
                            type == input$car_type) %>% 
                     group_by(do_neiborhood) %>%
                     summarize(avg_distance = mean(trip_distance)) %>% 
                     top_n(n = 15) %>% 
                     mutate(do_neiborhood = fct_reorder(do_neiborhood, avg_distance)) %>% 
                     ggplot(aes(x = do_neiborhood, 
                                y = avg_distance, 
                                fill = avg_distance)) +
                     geom_bar(stat = "identity") +
                     viridis::scale_fill_viridis(
                         begin = 1, end = 0) + 
                     coord_flip() +
                     labs(
                         title = "Top 10 Manhattan Neighborhoods with Highest Average Travel Distances",
                         x = "",
                         y = "Average distance travelled ($)") + 
                     theme_bw() +   
                     theme(legend.position = "none",    
                           plot.title = element_text(hjust = 0.5, size=11)))
    })
        
        
        output$mapPlot2 <- renderLeaflet({
            tp_data %>%
                filter(pu_boro == input[["boro_choice"]],
                       pickup_date == input[["day"]],
                       pickup_hr %in% input$time_range[1]:input$time_range[2],
                       type == input$car_type) %>% 
                group_by(pu_boro, pu_neiborhood, x.x, y.x, type) %>% 
                summarize(numpu = n(),
                          avg_distance = round(mean(trip_distance),2),
                          avg_fare = round(mean(fare_amount),2)) %>% 
                mutate(avg_fare = ifelse(is.na(avg_fare), 0, avg_fare),
                       avg_distance = ifelse(is.na(avg_distance), 0, avg_distance)) %>% 
                mutate(text_label = str_c("Pick-up neighborhood:", pu_neiborhood, 
                                          "</b><br>", "Count: ", numpu, " pick-ups",
                                          "</b><br>", "Mean distance: ", avg_distance, " miles",
                                          "</b><br>", "Mean fare: $", avg_fare)) %>% 
                leaflet() %>%
                addTiles() %>% 
                addProviderTiles(providers$CartoDB.Positron) %>% 
                addCircleMarkers(~x.x, ~y.x, 
                                 radius = ~numpu/10,
                                 popup = ~text_label,
                                 color = ~type)
    })
        
        output$summary_pu <- renderPlotly({
            ggplotly(tp_data %>%
                         filter(pu_boro == input[["boro_choice"]],
                                pickup_date == input[["day"]],
                                pickup_hr %in% input$time_range[1]:input$time_range[2],
                                type == input$car_type) %>% 
                         group_by(pu_neiborhood) %>%
                         summarize(freq = n()) %>% 
                         top_n(n = 15) %>% 
                         mutate(pu_neiborhood = fct_reorder(pu_neiborhood, freq)) %>% 
                         ggplot(aes(x = pu_neiborhood, 
                                    y = freq, 
                                    fill = freq)) +
                         geom_bar(stat = "identity") +
                         viridis::scale_fill_viridis(
                             begin = 1, end = 0) + 
                         coord_flip() +
                         labs(
                             title = "Top 10 pick-up neighborhoods in chosen borough",
                             x = "",
                             y = "Number of pick-ups") + 
                         theme_bw() +   
                         theme(legend.position = "none",    
                               plot.title = element_text(hjust = 0.5, size=12, face='bold')))
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
