library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(shinythemes)

# Define a function to fetch and process data from GitHub
fetch_healthcare_data <- function() {
  url <- "https://raw.githubusercontent.com/elvingggg/demo-app-healthcare/main/dummy_healthcare_data_malaysia.csv"
  data <- read.csv(url, stringsAsFactors = FALSE)
  data$DateReported <- as.Date(data$DateReported, format="%m/%d/%Y")
  return(data)
}

# Define your color scheme
colors <- list(
  skyBlue     = "#69D2E6",
  lightCyan   = "#A5DCD7",
  lightYellow = "#E1E6CD",
  orange      = "#F58732",
  redOrange   = "#FA6900",
  appleGreen  = "#64C864"
)

# Custom CSS for color scheme
custom_css <- sprintf("
.skin-blue .main-header .logo {
  background-color: %s; /* skyBlue */
  color: #fff;
}
.skin-blue .main-header .navbar {
  background-color: %s; /* orange */
}
.skin-blue .sidebar-menu a {
  color: %s; /* lightCyan */
}
.skin-blue .sidebar-menu li.active > a {
  background-color: %s; /* lightYellow */
  color: #000; /* Black text color for active menu item */
}
", colors$skyBlue, colors$orange, colors$lightCyan, colors$lightYellow)

# UI definition
ui <- dashboardPage(
  dashboardHeader(
    title = "Healthcare Analytics",
    # Custom logo with a link
    tags$li(
      class = "dropdown",
      tags$a(href = "https://www.n-actuarial.com/", 
             "Powered by : ", tags$img(src = "https://github.com/elvingggg/demo-app-healthcare/raw/main/NASlogo.png", height = 20, width = 164))
    )
  ),
  dashboardSidebar(
    tags$head(tags$style(HTML(custom_css))),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Statistics", tabName = "statistics", icon = icon("bar-chart")),
      uiOutput("stateInput"),
      uiOutput("conditionInput"),
      uiOutput("ageGroupInput"),
      uiOutput("startDateInput"),
      uiOutput("endDateInput")
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(custom_css))),
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
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(DT::dataTableOutput("data_table"), status = "primary", solidHeader = TRUE, width = 12)
              )
      ),
      tabItem(tabName = "statistics",
              fluidRow(
                box(plotlyOutput("topStatesPlot"), status = "primary", solidHeader = TRUE, width = 6),
                box(plotlyOutput("topConditionsPlot"), status = "primary", solidHeader = TRUE, width = 6)
              ),
              fluidRow(
                box(plotlyOutput("topAgeGroupsPlot"), status = "primary", solidHeader = TRUE, width = 6),
                box(plotlyOutput("newChart"), status = "primary", solidHeader = TRUE, width = 6)
              )
      )
    ),
    theme = shinytheme("flatly")
  )
)

# Server logic
server <- function(input, output, session) {
  # Fetch data from GitHub
  healthcareData <- reactive(fetch_healthcare_data())
  
  # UI output logic for stateInput, conditionInput, ageGroupInput, startDateInput, endDateInput
  output$stateInput <- renderUI({
    selectInput("state", "State", choices = c("All", unique(healthcareData()$State)), selected = "All")
  })
  
  output$conditionInput <- renderUI({
    selectInput("condition", "Condition", choices = c("All", unique(healthcareData()$Condition)), selected = "All")
  })
  
  output$ageGroupInput <- renderUI({
    selectInput("ageGroup", "Age Group", choices = c("All", unique(healthcareData()$AgeGroup)), selected = "All")
  })
  
  output$startDateInput <- renderUI({
    dateInput("startDate", "Start Date", value = min(healthcareData()$DateReported, na.rm = TRUE))
  })
  
  output$endDateInput <- renderUI({
    dateInput("endDate", "End Date", value = max(healthcareData()$DateReported, na.rm = TRUE))
  })
  
  # Filter the data based on user-selected filters
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
    data
  })
  
  # Display total cases and average cases
  output$totalCases <- renderUI({
    data <- filteredData()
    sum_cases <- sum(data$CasesReported, na.rm = TRUE)
    div(
      style = sprintf("background-color: %s; color: black; padding: 20px; border-radius: 15px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);", colors$lightYellow),
      h3(style = "margin-bottom: 10px; font-weight: bold; font-size: 18px; color: black;", "Total Cases"),
      h2(style = "margin: 0; font-size: 32px; color: black;", icon("hospital"), formatC(sum_cases, format = "d", big.mark = ","))
    )
  })
  
  output$averageCases <- renderUI({
    data <- filteredData()
    avg_cases <- ifelse(nrow(data) > 0 && sum(data$CasesReported, na.rm = TRUE) > 0,
                        round(mean(data$CasesReported, na.rm = TRUE), 2),
                        NA)
    div(
      style = sprintf("background-color: %s; color: black; padding: 20px; border-radius: 15px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);", colors$lightCyan),
      h3(style = "margin-bottom: 10px; font-weight: bold; font-size: 18px; color: black;", "Cases per Report"),
      h2(style = "margin: 0; font-size: 32px; color: black;", icon("medkit"), avg_cases)
    )
  })
  
  # Render the trend plot
  output$trendPlot <- renderPlotly({
    req(filteredData())
    gg <- ggplot(filteredData(), aes(x = DateReported, y = CasesReported, group = 1)) +
      geom_line() +
      geom_point() +
      theme_minimal() +
      labs(title = paste("Trend of", ifelse(input$condition == "All", "All Conditions", input$condition),
                         "Cases in", ifelse(input$state == "All", "All States", input$state)),
           x = "Date",
           y = "Cases Reported")
    ggplotly(gg)
  })
  
  # Render the data table
  output$data_table <- DT::renderDataTable({
    req(healthcareData())
    DT::datatable(healthcareData(), options = list(pageLength = 10))
  })
  
  # Render the plots for the "Statistics" tab
  output$topStatesPlot <- renderPlotly({
    req(filteredData())
    top_states <- filteredData() %>%
      group_by(State) %>%
      summarise(total_cases = sum(CasesReported, na.rm = TRUE)) %>%
      top_n(5, total_cases)
    
    gg <- ggplot(top_states, aes(x = State, y = total_cases)) +
      geom_bar(stat = "identity", fill = colors$skyBlue) +
      labs(title = "Top States with Most Cases", x = "State", y = "Total Cases") +
      theme_minimal()
    
    ggplotly(gg)
  })
  
  output$topConditionsPlot <- renderPlotly({
    req(filteredData())
    top_conditions <- filteredData() %>%
      group_by(Condition) %>%
      summarise(total_cases = sum(CasesReported, na.rm = TRUE)) %>%
      top_n(5, total_cases)
    
    gg <- ggplot(top_conditions, aes(x = Condition, y = total_cases)) +
      geom_bar(stat = "identity", fill = colors$lightCyan) +
      labs(title = "Top Conditions", x = "Condition", y = "Total Cases") +
      theme_minimal()
    
    ggplotly(gg)
  })
  
  output$topAgeGroupsPlot <- renderPlotly({
    data <- filteredData()
    top_age_groups <- data %>%
      group_by(AgeGroup) %>%
      summarise(total_cases = sum(CasesReported, na.rm = TRUE)) %>%
      arrange(desc(total_cases))
    
    gg <- ggplot(top_age_groups, aes(x = reorder(AgeGroup, total_cases), y = total_cases)) +
      geom_bar(stat = "identity", fill = colors$lightYellow) +
      labs(title = "Top Age Groups with Most Cases", x = "Age Group", y = "Total Cases") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(gg)
  })
  
  output$newChart <- renderPlotly({
    req(filteredData())
    avg_cases_per_day <- filteredData() %>%
      group_by(DateReported) %>%
      summarise(avg_cases = mean(CasesReported, na.rm = TRUE))
    
    gg <- ggplot(avg_cases_per_day, aes(x = DateReported, y = avg_cases)) +
      geom_line(color = colors$orange) +
      labs(title = "Average Cases per Day", x = "Date", y = "Average Cases") +
      theme_minimal()
    
    ggplotly(gg)
  })
}

# Run the Shiny app
shinyApp(ui, server)