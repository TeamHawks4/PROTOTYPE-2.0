library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)

# Load the dataset
df <- read_csv("New_data set.csv")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "EcoPulseX Dashboard 🌿"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Energy vs Footprint", tabName = "energy", icon = icon("bolt")),
      menuItem("Transport Mode Impact", tabName = "transport", icon = icon("bus")),
      menuItem("Work Hours vs Footprint", tabName = "work", icon = icon("briefcase")),
      menuItem("Steps vs Calories", tabName = "steps", icon = icon("walking")),
      menuItem("Sleep Patterns", tabName = "sleep", icon = icon("bed")),
      menuItem("Social vs Events", tabName = "social", icon = icon("users"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "energy",
              h3("Home Energy Consumption vs Carbon Footprint"),
              plotlyOutput("plot1", height = "500px")
      ),
      
      tabItem(tabName = "transport",
              h3("Mode of Transport Distribution"),
              plotlyOutput("plot2", height = "500px")
      ),
      
      tabItem(tabName = "work",
              h3("Work Hours vs Carbon Footprint"),
              plotlyOutput("plot3", height = "500px")
      ),
      
      tabItem(tabName = "steps",
              h3("Steps Walked vs Calories Burned"),
              plotlyOutput("plot4", height = "500px")
      ),
      
      tabItem(tabName = "sleep",
              h3("Sleep Hours Distribution"),
              plotlyOutput("plot5", height = "500px")
      ),
      
      tabItem(tabName = "social",
              h3("Social Media vs Public Events Hours"),
              plotlyOutput("plot6", height = "500px")
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  output$plot1 <- renderPlotly({
    plot_ly(df, x = ~Home_Energy_Consumption_kWh, y = ~Carbon_Footprint_kgCO2,
            type = 'bar', marker = list(color = 'forestgreen')) %>%
      layout(xaxis = list(title = "Home Energy Consumption (kWh)"),
             yaxis = list(title = "Carbon Footprint (kg CO2)"))
  })
  
  output$plot2 <- renderPlotly({
    transport_dist <- df %>% count(Mode_of_Transport)
    plot_ly(transport_dist, labels = ~Mode_of_Transport, values = ~n, type = 'pie') %>%
      layout(title = "Mode of Transport Usage")
  })
  
  output$plot3 <- renderPlotly({
    plot_ly(df, x = ~Work_Hours, y = ~Carbon_Footprint_kgCO2,
            type = 'scatter', mode = 'markers',
            marker = list(size = 10, color = 'orange')) %>%
      layout(xaxis = list(title = "Work Hours per Day"),
             yaxis = list(title = "Carbon Footprint (kg CO2)"))
  })
  
  output$plot4 <- renderPlotly({
    plot_ly(df, x = ~Steps_Walked, y = ~Calories_Burned,
            type = 'scatter', mode = 'markers',
            marker = list(color = 'dodgerblue')) %>%
      layout(xaxis = list(title = "Steps Walked"),
             yaxis = list(title = "Calories Burned"))
  })
  
  output$plot5 <- renderPlotly({
    plot_ly(df, x = ~Sleep_Hours, type = "histogram", marker = list(color = 'plum')) %>%
      layout(xaxis = list(title = "Sleep Hours"),
             yaxis = list(title = "Frequency"))
  })
  
  output$plot6 <- renderPlotly({
    plot_ly(df, x = ~Social_Media_Hours, y = ~Public_Events_Hours,
            type = 'scatter', mode = 'markers',
            marker = list(color = 'mediumseagreen')) %>%
      layout(xaxis = list(title = "Social Media Hours"),
             yaxis = list(title = "Public Events Hours"))
  })
}

shinyApp(ui, server)
