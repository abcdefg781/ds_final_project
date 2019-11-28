library(shiny)
library(flexdashboard)
library(viridis)
library(p8105.datasets)
library(tidyverse)
library(plotly)

data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(rating = review_scores_location / 2) %>%
  rename(latitude = lat, longitude = long) %>%
  select(
    boro = neighbourhood_group, neighbourhood, rating, price, room_type,
    latitude, longitude) %>%
  filter(!is.na(rating))

boros = nyc_airbnb %>% distinct(boro) %>% pull()

max_price = 1000

min_price = nyc_airbnb %>% distinct(price) %>% min()

room_choice = nyc_airbnb %>% distinct(room_type) %>% pull()

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Airbnb Explorer beta 1.0"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
    
      selectInput(
        "boro_choice", 
        label = h3("Select boro"),
        choices = boros, selected = "Manhattan"),
      
      sliderInput(
        "price_range", 
        label = h3("Choose price range"), 
        min = min_price, max = max_price, value = c(100, 400)),
      
      radioButtons(
        "room_choice", 
        label = h3("Choose room type"),
        choices = room_choice, selected = "Entire home/apt")
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("mapPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$mapPlot <- renderPlotly({ 
    nyc_airbnb %>%
      filter(boro == input[["boro_choice"]],
             price %in% input$price_range[1]:input$price_range[2],
             room_type == input$room_choice) %>% 
      mutate(text_label = str_c("Price: $", price, '\nRating: ', rating)) %>% 
      plot_ly(
        x = ~longitude, y = ~latitude, type = "scatter", mode = "markers",
        alpha = 0.5, color = ~price, text = ~text_label)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

