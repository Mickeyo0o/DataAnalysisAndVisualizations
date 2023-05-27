library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)

# Define server logic required to draw a histogram
function(input, 
         output,
         session) { 
  df <- read.csv('reviews.csv')
  filters <- reactive({
    list(input$appName, input$dateRange)
  })
  updateSelectizeInput(session, inputId = 'appName', choices = df$appId, server = TRUE)
  reactiveVals <- reactiveValues()
  reactiveVals$filteredData <- df
  reactiveVals$currMinDate <- NA
  reactiveVals$currMaxDate <- NA
  observeEvent(input$appName, {
    reactiveVals$filteredData <- df %>% filter(appId == input$appName)
    if(reactiveVals$filteredData %>% count() > 0){
      minDate = (reactiveVals$filteredData %>% summarise(min(at)))[[1]]
      maxDate = (reactiveVals$filteredData %>% summarise(max(at)))[[1]]
      updateDateRangeInput(session, inputId = 'dateRange', start = minDate, end = maxDate,
                           min = minDate, max = maxDate) 
      reactiveVals$currMinDate <- minDate
      reactiveVals$currMaxDate <- maxDate
    }
  })
  observeEvent(input$dateRange, {
    reactiveVals$filteredData <- df %>% filter(appId == input$appName & at > min(input$dateRange) & at < max(input$dateRange))
  })
  observeEvent(input$allDates, {
    updateDateRangeInput(session, inputId = 'dateRange', start = reactiveVals$currMinDate, end = reactiveVals$currMaxDate) 
  })
  output$comments <- renderDT(
    reactiveVals$filteredData %>% select(userName, content, score)
  )
  output$scoreHist <- renderPlot({
    data <- reactiveVals$filteredData %>% select(score)
    ggplot(data, aes(x = score)) +
      geom_bar() +
      theme_minimal()
  })
  
}
