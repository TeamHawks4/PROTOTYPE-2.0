### Personalized Carbon Footprint Guide ###
### Aditya Kumar Roy ###

## Loading Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinythemes)
library(shinyjs)

## Loading the Datasets
data <- read_csv("smart_city_citizen_activity.csv")
head(data)

## Data Pre-Processing
colSums(is.na(data))
unique(data)

## Exploratory Data Analysis
str(data)
summary(data)

table(data$Gender)
data$Gender <- ifelse(data$Gender == "Male", 1,
                      ifelse(data$Gender == "Female", 2, 3))
data$Gender <- as.numeric(data$Gender)
table(data$Mode_of_Transport)

data$Mode_of_Transport <- case_when(
  data$Mode_of_Transport == "Bicycle" ~ 1,
  data$Mode_of_Transport == "Bike" ~ 2,
  data$Mode_of_Transport == "Car" ~ 3,
  data$Mode_of_Transport == "EV" ~ 4,
  data$Mode_of_Transport == "Public Transport" ~ 5,
  data$Mode_of_Transport == "Walking" ~ 6,
  TRUE ~ NA_real_
)
data$Mode_of_Transport <- as.numeric(data$Mode_of_Transport)

data <- data[, c(setdiff(names(data), "Carbon_Footprint_kgCO2"), "Carbon_Footprint_kgCO2")]

corr <- cor(data)

data <- data %>% select(-Citizen_ID, -Sleep_Hours, -Public_Events_Hours, -Calories_Burned,
                        -Social_Media_Hours, -Work_Hours)

# Define UI for the app
ui <- fluidPage(
  # Custom CSS for styling
  tags$style(HTML("
    .container-fluid {
      background-color: #F0F8FF;
    }
    .input-box {
      background-color: #3CB371;
      color: white;
      border: 1px solid white;
      border-radius: 5px;
      padding: 10px;
      margin-bottom: 10px;
    }
    .output-box {
      background-color: #3CB371;
      color: white;
      border: 1px solid white;
      border-radius: 5px;
      padding: 10px;
      margin-top: 10px;
    }
    .checkbox {
      font-size: 18px;
    }
  ")),
  
  # Title
  titlePanel(
    HTML("<div style='text-align: center; font-weight: bold; font-size: 30px;'>🌍 GreenMeter: Personal Carbon Footprint Guide 🌱</div>")
  ),
  
  # Input Section
  fluidRow(
    column(6,
           helpText("👤 Please enter your age:", style = "font-weight: bold; font-size: 18px; color: #333333;"),
           tags$div(class = "input-box", numericInput("age", "Age:", value = 18, min = 0)),
           
           helpText("🧑‍🤝‍🧑 Please select your gender:", style = "font-weight: bold; font-size: 18px; color: #333333;"),
           tags$div(class = "input-box", selectInput("gender", "Gender:", 
                                                     choices = c("Male", "Female", "Other"))),
           
           helpText("🚗 Enter your travel distance in kilometers:", style = "font-weight: bold; font-size: 18px; color: #333333;"),
           tags$div(class = "input-box", numericInput("travel_distance", "Travel Distance (km):", value = 0)),
           
           helpText("🚌 Select your mode of transport:", style = "font-weight: bold; font-size: 18px; color: #333333;"),
           tags$div(class = "input-box", selectInput("transport_mode", "Mode of Transport:", 
                                                     choices = c("Car", "Public Transport", "Bicycle", "Walking"))),
           
           helpText("🥗 Select the type of food you consume:", style = "font-weight: bold; font-size: 18px; color: #333333;"),
           tags$div(class = "input-box", selectInput("food_type", "Type of Food:", 
                                                     choices = c("Meat", "Dairy", "Plant-based")))
    ),
    column(6,
           helpText("💡 Enter your electricity usage in kWh:", style = "font-weight: bold; font-size: 18px; color: #333333;"),
           tags$div(class = "input-box", numericInput("electricity_usage", "Electricity Usage (kWh):", value = 0)),
           
           helpText("📱 Enter the number of hours spent on social media:", style = "font-weight: bold; font-size: 18px; color: #333333;"),
           tags$div(class = "input-box", numericInput("social_media_hours", "Social Media Hours:", value = 0)),
           
           helpText("🚶‍♂️ Enter the number of steps walked:", style = "font-weight: bold; font-size: 18px; color: #333333;"),
           tags$div(class = "input-box", numericInput("steps_walked", "Steps Walked:", value = 0)),
           
           helpText("🔌 Enter the number of times charging stations are used:", style = "font-weight: bold; font-size: 18px; color: #333333;"),
           tags$div(class = "input-box", numericInput("charging_station_usage", "Charging Station Usage (times):", value = 0)),
           
           tags$div(class = "input-box", actionButton("calculate", "Calculate Carbon Footprint"))
    )
  ),
  
  # Output Section
  fluidRow(
    column(12,
           h3("🔢 Results"),
           div(class = "output-box", textOutput("footprint_result"))
    ),
    column(12,
           h3("💡 Suggestions for You"),
           div(class = "output-box", textOutput("suggestions"))
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Function to calculate carbon footprint
  calculate_footprint <- function() {
    # Define emission factors (example values)
    transport_emission_factors <- c(Car = 0.2, PublicTransport = 0.05, Bicycle = 0, Walking = 0)
    food_emission_factors <- c(Meat = 7.0, Dairy = 3.0, Plant_based = 1.0)
    social_media_emission_factor <- 0.01  # Example factor for social media usage
    charging_station_emission_factor <- 0.5  # Example factor for charging station usage
    
    # Calculate carbon footprint
    travel_footprint <- input$travel_distance * transport_emission_factors[input$transport_mode]
    food_footprint <- food_emission_factors[input$food_type] * 1  # Ensure food type is numeric
    electricity_footprint <- input$electricity_usage * 0.5  # Example factor for electricity
    social_media_footprint <- input$social_media_hours * social_media_emission_factor
    charging_station_footprint <- input$charging_station_usage * charging_station_emission_factor
    
    total_footprint <- travel_footprint + food_footprint + electricity_footprint + social_media_footprint + charging_station_footprint
    return(total_footprint)
  }
  
  # Function to provide suggestions
  provide_suggestions <- function(footprint) {
    suggestions <- ""
    
    # Mode of Transport Suggestions
    if (input$travel_distance > 10 && input$transport_mode == "Car") {
      suggestions <- paste(suggestions, "✅ Consider using public transport for long commutes to reduce your carbon footprint.If you are working professional try switching to EV.\n", sep = "")
    } else if (input$travel_distance <= 5 && input$transport_mode == "Car") {
      suggestions <- paste(suggestions, "✅ Try walking for nearby distances to reduce emissions and improve health.\n", sep = "")
    }
    
    # Social Media & Electricity Usage Suggestions
    if (input$social_media_hours > 2) {
      suggestions <- paste(suggestions, "✅ Consider reducing social media usage to save electricity and reduce your carbon footprint.Instead why not play.\n", sep = "")
    }
    
    # Walking Encouragement
    if (input$age < 40 && input$steps_walked < 5000) {
      suggestions <- paste(suggestions, "✅ Try walking more often; it's a great way to reduce emissions and stay healthy.\n", sep = "")
    }
    
    # Charging Station Usage
    if (input$charging_station_usage > 2) {
      suggestions <- paste(suggestions, "✅ Try to minimize the usage of charging stations to save electricity.\n", sep = "")
    }
    
    # Food Consumption Suggestions
    if (input$food_type == "Meat") {
      suggestions <- paste(suggestions, "✅ Consider reducing meat consumption, as it has a high carbon footprint.\n", sep = "")
    } else if (input$food_type == "Dairy") {
      suggestions <- paste(suggestions, "✅ Consider switching to plant-based food options to reduce your carbon footprint.\n", sep = "")
    }
    
    # Final Recommendations
    if (footprint > 10) {
      suggestions <- paste(suggestions, "✅ Overall, try to adopt more sustainable habits to lower your carbon footprint.\n", sep = "")
    } else if (footprint > 5) {
      suggestions <- paste(suggestions, "✅ You're on the right track, but there is still room for improvement.\n", sep = "")
    } else {
      suggestions <- paste(suggestions, "✅ Great job! Keep maintaining your low carbon footprint.\n", sep = "")
    }
    
    return(suggestions)
  }
  
  # Observe button click
  observeEvent(input$calculate, {
    footprint <- calculate_footprint()
    
    output$footprint_result <- renderText({
      paste("Your estimated carbon footprint is:", round(footprint, 2), "kg CO2.")
    })
    
    output$suggestions <- renderText({
      provide_suggestions(footprint)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
