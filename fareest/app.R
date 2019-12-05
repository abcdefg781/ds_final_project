library(shiny)
library(readr)
library(tidyverse)
library(plotly)

taxi_data = read_csv("./data/transport_final.csv")

impute_data = read_csv("./data/impute_final.csv")

fare_data = 
    taxi_data %>% 
    separate(pickup_time, into = c("pickup_hr", "pickup_min", "pickup_sec"), sep = ":") %>% 
    separate(dropoff_time, into = c("dropoff_hr", "dropoff_min", "dropoff_sec"), sep = ":") %>% 
    select(-pickup_sec, -dropoff_min, -pickup_min, -dropoff_sec) %>% 
    mutate(
        pickup_hr = as.numeric(pickup_hr),
        dropoff_hr = as.numeric(dropoff_hr)) %>% 
    filter(type != "for hire",
           pu_boro != "Staten Island") %>% 
    drop_na(pu_boro)

fare_data_base =
    fare_data %>% 
    mutate(time_of_day=
               cut(pickup_hr, 
                   breaks = c(0,2,5,9,11,13,16,18,21,23),
                   labels = c("night","early morning","morning rush","others","lunch","others","evening rush","dinner time","night")),
           congestion = (fare_amount - trip_distance*2.5 -2.5)/.5) %>% 
                              filter(congestion>=0) 

fare_data = fare_data_base[-c(123, 16214),]
                              

pu_neiborhood = impute_final %>% distinct(pu_neiborhood) %>% pull()

do_neiborhood = impute_final %>% distinct(do_neiborhood) %>% pull()

time = fare_data %>% 
    distinct(time_of_day) %>% 
    drop_na(time_of_day) %>% 
    pull()

moderate_lm = lm(fare_amount ~ trip_distance + duration + as.factor(time_of_day), data = fare_data)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Taxi Fare Estimator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("pu_choice", 
                        label = h3("Select pick-up neighborhood"),
                        choices = pu_neiborhood, selected = "Alphabet City"),
                    
            selectInput("do_choice", 
                        label = h3("Select drop-off neighborhood"),
                        choices = do_neiborhood, selected = "Clinton East"),

            selectInput("time_of_day", 
                        label = h3("Select current time window"),
                        choices = time, selected = "dinner")),

        # Show a plot of the generated distribution
        mainPanel(
           dataTableOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderDataTable({
        impute_data %>% 
            filter(do_neiborhood = input[["do_choice"]],
                   pu_neiborhood = input[["pu_choice"]]) %>%
            pull(avg_dist, avg_duration) %>% 
            tibble()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
