library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(plotly)
library(shinyWidgets)
library(shinyjs)

ui <- dashboardPage(
  dashboardHeader(title = "Malaysia Healthcare Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      fileInput("fileUpload", "Choose CSV File", accept = ".csv"),
      uiOutput("stateInput"),
      uiOutput("conditionInput"),
      uiOutput("ageGroupInput"),
      uiOutput("startDateInput"),  # Updated UI output for start date
      uiOutput("endDateInput")     # Updated UI output for end date
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
              )
      )
    ),
    theme = shinytheme("flatly")
  )
)

server <- function(input, output, session) {
  healthcareData <- reactive({
    req(input$fileUpload) # Ensure a file is uploaded
    inFile <- input$fileUpload
    data <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
    data$DateReported <- as.Date(data$DateReported, format="%m/%d/%Y") # Correctly parsing the date
    return(data)
  })
  
  output$stateInput <- renderUI({
    selectInput("state", "State", choices = c("All", unique(healthcareData()$State)), selected = "All")
  })
  
  output$conditionInput <- renderUI({
    selectInput("condition", "Condition", choices = c("All", unique(healthcareData()$Condition)), selected = "All")
  })
  
  output$ageGroupInput <- renderUI({
    selectInput("ageGroup", "Age Group", choices = c("All", unique(healthcareData()$AgeGroup)), selected = "All")
  })
  
  # Separate UI outputs for start and end date inputs
  output$startDateInput <- renderUI({
    data <- healthcareData()
    dateInput("startDate", "Start Date", value = min(data$DateReported, na.rm = TRUE))
  })
  
  output$endDateInput <- renderUI({
    data <- healthcareData()
    dateInput("endDate", "End Date", value = max(data$DateReported, na.rm = TRUE))
  })
  
  filteredData <- reactive({
    data <- healthcareData()
    if(input$state != "All") {
      data <- data %>% filter(State == input$state)
    }
    if(input$condition != "All") {
      data <- data %>% filter(Condition == input$condition)
    }
    if(input$ageGroup != "All") {
      data <- data %>% filter(AgeGroup == input$ageGroup)
    }
    if (!is.null(input$startDate) && !is.null(input$endDate)) {
      data <- data %>% filter(DateReported >= input$startDate & DateReported <= input$endDate)
    }
    return(data)
  })
  
  output$totalCases <- renderValueBox({
    data <- filteredData()
    if(nrow(data) > 0) {
      sum_cases <- sum(data$CasesReported, na.rm = TRUE)
    } else {
      sum_cases <- 0
    }
    valueBox(
      formatC(sum_cases, format = "d", big.mark = ","),
      "Total Cases",
      icon = icon("hospital"),
      color = "red"
    )
  })
  
  output$averageCases <- renderValueBox({
    data <- filteredData()
    if(nrow(data) > 0 && sum(data$CasesReported, na.rm = TRUE) > 0) {
      avg_cases <- round(mean(data$CasesReported, na.rm = TRUE), 2)
    } else {
      avg_cases <- NA
    }
    valueBox(
      avg_cases,
      "Average Cases per Report",
      icon = icon("medkit"),
      color = "blue"
    )
  })
  
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
