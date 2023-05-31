library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(flexdashboard)
library(plotly)
library(stringr)

# Define server logic required to draw a histogram
function(input, 
         output,
         session) { 
  df <- read.csv('reviews.csv')
  filters <- reactive({
    list(input$appName, input$dateRange, input$scoreSlider)
  })
  updateSelectizeInput(session, inputId = 'appName', choices = c( "Show all", df$appId), server = TRUE)
  updateSelectInput(session, inputId="scoresSelect", choices = unique(df$score), selected = unique(df$score))
  reactiveVals <- reactiveValues()
  reactiveVals$filteredData <- df
  reactiveVals$currMinDate <- NA
  reactiveVals$currMaxDate <- NA
  observeEvent(input$appName, {
    if(input$appName == "Show all") { reactiveVals$filteredData <- df } 
    else { reactiveVals$filteredData <- df %>% filter(appId == input$appName) }
    if(reactiveVals$filteredData %>% count() > 0){
      temp = reactiveVals$filteredData %>% filter(thumbsUpCount > 0) %>%
        filter(thumbsUpCount >= max(.$thumbsUpCount) * (1-0.95))
      updateSelectInput(session, inputId = "scoresSelect", 
                        choices = unique(temp$score),
                        selected = unique(temp$score))
      minDate = (reactiveVals$filteredData %>% summarise(min(at)))[[1]]
      maxDate = (reactiveVals$filteredData %>% summarise(max(at)))[[1]]
      updateDateRangeInput(session, inputId = 'dateRange', start = minDate, end = maxDate,
                           min = minDate, max = maxDate) 
      reactiveVals$currMinDate <- minDate
      reactiveVals$currMaxDate <- maxDate
    }
  })
  observeEvent(input$dateRange, {
    if(input$appName == "Show all") {
      reactiveVals$filteredData <- df %>% filter(at > min(input$dateRange) & at < max(input$dateRange))
    } else {
      reactiveVals$filteredData <- df %>% filter(appId == input$appName & at > min(input$dateRange) & at < max(input$dateRange))
    }
    temp = reactiveVals$filteredData %>% filter(thumbsUpCount > 0) %>%
      filter(thumbsUpCount >= max(.$thumbsUpCount) * (1-0.95))
    updateSelectInput(session, inputId = "scoresSelect", 
                      choices = unique(temp$score),
                      selected = unique(temp$score))
  })
  
  observeEvent(input$scoresSelect, {
    reactiveVals$selectedScores = input$scoresSelect
  })
  
  observeEvent(input$methodReviews, {
    reactiveVals$methodStr = input$methodReviews
  })
  
  output$comments <- renderDT(
    reactiveVals$filteredData %>% select(userName, content, score),
    options = list(lengthMenu = c(5, 10, 25, 50, 100))
  )
  
  output$reviewsPie <- renderPlotly({
    data <- data.frame(grade = as.character(reactiveVals$filteredData$score)) %>% 
      dplyr::group_by(grade) %>% 
      summarise(total = n()) %>%
      mutate(percentage = total / sum(total) * 100) %>%
      mutate(color = ifelse(grade == "1", "#ff0000", 
                            ifelse(grade == "2", "#ffa700",
                                   ifelse(grade == "3", "#fff400",
                                          ifelse(grade == "4", "#a3ff00", "#2cba00")))))
    
    plot_ly(data, labels = ~grade, values = ~percentage, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            hoverinfo = 'text',
            text = ~paste(total, 'Number of occurrences'),
            marker = list(colors = ~color,
                          line = list(color = '#FFFFFF', width = 1)),
            sort = FALSE,
            rotation = 180,
            showlegend = FALSE) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$reviewsTime <- renderPlotly({
    data <- data.frame(time = reactiveVals$filteredData$at) %>% 
      mutate(time = str_extract(time, "^[^\\s]+")) %>%
      mutate(time = as.Date(time)) %>%
      group_by(time) %>%
      summarise(total = n())
    
    plotReviewsTime <- ggplot(data, aes(x=time, y=total, group = 1)) +
      geom_point() +
      geom_smooth(method = reactiveVals$methodStr, se =FALSE, color="blue", linewidth=1, linetype=1) +
      theme_minimal() + 
      scale_x_date(date_labels = "%Y-%m-%d")
    
    ggplotly(plotReviewsTime)
    
  })
  
  output$thumbs <- renderPlotly({
    data <- data.frame(name = reactiveVals$filteredData$userName, 
                       time = reactiveVals$filteredData$at, 
                       content = reactiveVals$filteredData$content, 
                       score = reactiveVals$filteredData$score, 
                       thumbs = reactiveVals$filteredData$thumbsUpCount) %>% 
      filter(thumbs > 0) %>%
      filter(thumbs >= max(.$thumbs) * (1-0.95)) %>%
      mutate(time = str_extract(time, "^[^\\s]+")) %>%
      mutate(time = as.Date(time)) %>%
      mutate(color = ifelse(score == "1", "#ff0000", 
                            ifelse(score == "2", "#ffa700",
                                   ifelse(score == "3", "#fff400",
                                          ifelse(score == "4", "#a3ff00", "#2cba00"))))) %>%
    filter(score %in% reactiveVals$selectedScores)
    
    p1 <- ggplot(data, aes(x = time, y = thumbs, text = paste("Grade:", score))) +
      geom_point(alpha = 0.7, size = 3, color = data$color) +
      theme_minimal() +
      scale_x_date(date_labels = "%Y-%m-%d")
    
    ggplotly(p1)
  })
  
  output$repliesPerGrade <- renderPlotly({
    data <- data.frame(score = reactiveVals$filteredData$score, 
                       time = reactiveVals$filteredData$at, 
                       reply = reactiveVals$filteredData$repliedAt) %>% 
      group_by(score) %>%
      summarize(percentage = sum(!is.null(reply)) / n() * 100) %>%
      mutate(color = ifelse(score == "1", "#ff0000", 
                            ifelse(score == "2", "#ffa700",
                                   ifelse(score == "3", "#fff400",
                                          ifelse(score == "4", "#a3ff00", "#2cba00")))))
    
    p1 <- ggplot(data, aes(x = score, y = percentage)) +
      geom_bar(stat = "identity", fill = df1$color) +
      labs(x = "Grade", y = "Percentage of Reviews with Reply") +
      theme_minimal() 
    
    ggplotly(p1)
  })
  
  
}
