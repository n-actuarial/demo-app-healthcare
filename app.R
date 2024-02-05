library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(plotly)
library(shinyWidgets)
library(shinyjs)

healthcare_data <- read.csv("dummy_healthcare_data_malaysia.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Malaysia Healthcare Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Trends", tabName = "trends", icon = icon("line-chart")),
      selectInput("state", "State", c("All", unique(healthcare_data$State)), selected = "All"),
      selectInput("condition", "Condition", c("All", unique(healthcare_data$Condition)), selected = "All"),
      selectInput("ageGroup", "Age Group", c("All", unique(healthcare_data$AgeGroup)), selected = "All"),
      dateRangeInput("dateRange", "Date Range", start = min(healthcare_data$DateReported), end = max(healthcare_data$DateReported))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("totalCases"),
                valueBoxOutput("averageCases")
              ),
              fluidRow(
                box(plotlyOutput("trendPlot"), status = "primary", solidHeader = TRUE, width = 12)
              )),
      tabItem(tabName = "trends",
              h2("Trends Analysis")
      )
    ),
    theme = shinytheme("flatly")
  )
)

server <- function(input, output) {
  filteredData <- reactive({
    data <- healthcare_data
    if(input$state != "All") {
      data <- data %>% filter(State == input$state)
    }
    if(input$condition != "All") {
      data <- data %>% filter(Condition == input$condition)
    }
    if(input$ageGroup != "All") {
      data <- data %>% filter(AgeGroup == input$ageGroup)
    }
    data <- data %>% filter(DateReported >= input$dateRange[1], DateReported <= input$dateRange[2])
    return(data)
  })
  
  # Generate key metrics
  output$totalCases <- renderValueBox({
    data <- filteredData()
    valueBox(
      formatC(sum(data$CasesReported), format = "d", big.mark = ","),
      "Total Cases",
      icon = icon("hospital-o"),
      color = "red"
    )
  })
  
  output$averageCases <- renderValueBox({
    data <- filteredData()
    valueBox(
      round(mean(data$CasesReported), 2),
      "Average Cases per Report",
      icon = icon("medkit"),
      color = "blue"
    )
  })
  
  # Generate trend plot with plotly for interactive zoom and pan
  output$trendPlot <- renderPlotly({
    gg <- ggplot(filteredData(), aes(x = as.Date(DateReported), y = CasesReported, group = 1)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = paste("Trend of", ifelse(input$condition == "All", "All Conditions", input$condition), 
                         "Cases in", ifelse(input$state == "All", "All States", input$state)),
           x = "Date",
           y = "Cases Reported")
    ggplotly(gg)
  })
}

shinyApp(ui = ui, server = server)