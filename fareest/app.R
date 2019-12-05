library(shiny)
library(readr)
library(tidyverse)
library(plotly)
library(htmlwidgets)

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

fare_data =
    fare_data %>% 
    mutate(time_of_day=
               cut(pickup_hr, 
                   breaks = c(0,2,5,9,11,13,16,18,21,23),
                   labels = c("night","early morning","morning rush","others","lunch","others","evening rush","dinner time","night")),
           congestion = (fare_amount - trip_distance*2.5 -2.5)/.5) %>% 
                              filter(congestion>=0) %>% 
    drop_na(fare_amount)

fare_data = 
    fare_data %>% filter(fare_amount <= 60)

fare_data = fare_data[-c(123, 16214),]
                              
pu_neighborhood = impute_data %>% distinct(pu_neiborhood) %>% pull()

do_neighborhood = impute_data %>% distinct(do_neiborhood) %>% pull()

time_of_day = fare_data %>% 
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
                        choices = pu_neighborhood, selected = "Alphabet City"),
                    
            selectInput("do_choice", 
                        label = h3("Select drop-off neighborhood"),
                        choices = do_neighborhood, selected = "SoHo"),

            selectInput("time", 
                        label = h3("Select current time window"),
                        choices = time_of_day, 
                        selected = "evening rush")),

        # Show a plot of the generated distribution
        mainPanel(tabsetPanel(type = "tab",
                              tabPanel("Disclaimer", textOutput("disclaimer")),
                              tabPanel("Fare prediction", textOutput("predict"))
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$predict <- renderText({  
            abc = impute_data %>% 
                filter(do_neiborhood == input[["do_choice"]],
                       pu_neiborhood == input[["pu_choice"]]) %>%
                pull(avg_dist) 
            
            cdf = impute_data %>% 
                filter(do_neiborhood == input[["do_choice"]],
                       pu_neiborhood == input[["pu_choice"]]) %>%
                pull(avg_duration) 
            
            newData = data.frame(trip_distance = abc, duration = cdf, time_of_day = input[["time"]])
            predict_df = predict(moderate_lm, newdata = newData, interval = "prediction", level = .95)
            paste("Your estimated fare is: $", round(predict_df[1],2), 
                  "The prediction interval for your estimated fare is: $", round(min(predict_df),2), "to $", round(max(predict_df),2),".")
    })
  
  output$disclaimer <- renderText({  
      paste("This fare estimator is based off a subset of the original dataset that does not contain distance and duration values for all the pick-up 
      and drop-off neighborhood pairs. Therefore, if you run into 'Error', it is because there is no underlying data for your neighborhood
      pair choice. We are sorry for any inconvenience this may cause. However, this should at least give you the estimated fare 
      for the popular neighborhoods (which we hope is where you're travelling to!) We hope you enjoy using this interactive app!")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
