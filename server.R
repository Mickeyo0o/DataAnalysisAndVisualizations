library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(plotly)
library(stringr)

# Define server logic required to draw a histogram
function(input, output, session) {
  
  df <- read.csv('reviews.csv')
  
  filters <- reactive({list(input$appName, input$dateRange)})
  reactiveVals <- reactiveValues()
  reactiveVals$filteredData <- df
  reactiveVals$currMinDate <- NA
  reactiveVals$currMaxDate <- NA
  
  updateSelectizeInput(session, inputId = 'appName', 
                       choices = c( "Show all", df$appId), 
                       selected = "Show all",
                       server = TRUE)
  
  minDate = (df %>% summarise(min(at)))[[1]]
  maxDate = (df %>% summarise(max(at)))[[1]]
  
  updateDateRangeInput(session, inputId = 'dateRange',
                       start = minDate, end = maxDate,
                       min = minDate, max = maxDate) 
  
  updateSelectInput(session, inputId="scoresSelect", 
                    choices = unique(df$score), 
                    selected = unique(df$score))
  
  updateSliderInput(session, inputId = "thumbsInterval",
                    min = min(df$thumbsUpCount),
                    max = max(df$thumbsUpCount), 
                    value = c(min(df$thumbsUpCount), max(df$thumbsUpCount)))
  
  temp = data.frame(score = df$score, time = df$at, reply = df$repliedAt) %>%
    filter(str_length(reply) > 0) %>%
    mutate(between = as.Date(reply) - as.Date(time)) %>%
    filter(between >= 0) %>%
    group_by(score, between) %>%
    summarize(amount = n()) %>%
    arrange(desc(amount))
  
  updateSliderInput(session, inputId = "dataAmount", 
                    min = min(temp$amount),
                    max = temp$amount[5],
                    value = min(temp$amount))
  
  
  
  
  observeEvent(input$appName, {
    if(input$appName == "Show all") { reactiveVals$filteredData <- df } 
    
    else { reactiveVals$filteredData <- df %>% filter(appId == input$appName) }
    
    if(reactiveVals$filteredData %>% count() > 0) {
      
      minDate = (reactiveVals$filteredData %>% summarise(min(at)))[[1]]
      maxDate = (reactiveVals$filteredData %>% summarise(max(at)))[[1]]
      
      updateDateRangeInput(session, inputId = 'dateRange',
                           start = minDate, end = maxDate,
                           min = minDate, max = maxDate) 
      
      reactiveVals$currMinDate <- minDate
      reactiveVals$currMaxDate <- maxDate
    }
  })
  
  
  observeEvent(input$dateRange, {
    if(input$appName == "Show all") {
      
      reactiveVals$filteredData <- df %>%
        filter(at > min(input$dateRange) & at < max(input$dateRange))
      
    } else {
      
      reactiveVals$filteredData <- df %>% 
        filter(appId == input$appName & 
                 at > min(input$dateRange) & 
                 at < max(input$dateRange))
      
    }
    if (reactiveVals$filteredData %>% count() > 0) {
      temp = reactiveVals$filteredData %>% 
        filter(thumbsUpCount > 0) %>%
        filter(thumbsUpCount >= max(.$thumbsUpCount) * (1-0.95))
      
      updateSelectInput(session, inputId = "scoresSelect", 
                        choices = unique(temp$score),
                        selected = unique(temp$score))
      
      updateSliderInput(session, inputId = "thumbsInterval", 
                        min = min(reactiveVals$filteredData$thumbsUpCount),
                        max = max(reactiveVals$filteredData$thumbsUpCount), 
                        value = c(min(reactiveVals$filteredData$thumbsUpCount),
                                  max(reactiveVals$filteredData$thumbsUpCount)))
      
      temp = reactiveVals$filteredData %>% 
        data.frame(score = .$score, time = .$at, reply = .$repliedAt) %>%
        filter(str_length(reply) > 0) %>%
        mutate(between = as.Date(reply) - as.Date(time)) %>%
        filter(between >= 0) %>%
        group_by(score, between) %>%
        summarize(amount = n())  %>%
        arrange(desc(amount))
      
      updateSliderInput(session, inputId = "dataAmount", 
                        min = min(temp$amount),
                        max = temp$amount[5],
                        value = min(temp$amount))
    }
  })
  
  
  observeEvent(input$scoresSelect, {
    reactiveVals$selectedScores = input$scoresSelect
  })
  
  
  observeEvent(input$methodReviews, {
    reactiveVals$methodStr = input$methodReviews
  })
  
  
  observeEvent(input$thumbsInterval, {
    reactiveVals$thumbsInterval = input$thumbsInterval
  })
  
  
  observeEvent(input$dataAmount, {
    reactiveVals$dataAmount = input$dataAmount
  })
  
  
  
  
  output$comments <- renderDT(
    reactiveVals$filteredData %>% select(userName, content, score),
    options = list(lengthMenu = c(5, 10, 25, 50, 100))
  )
  
  
  output$reviewsPie <- renderPlotly({
    if (reactiveVals$filteredData %>% count() > 0) {
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
              text = ~paste(total, 'occurrences'),
              marker = list(colors = ~color,
                            line = list(color = '#2d2d30', width = 1)),
              sort = FALSE,
              rotation = 180,
              showlegend = FALSE)%>%
        layout(plot_bgcolor='#2d2d30', paper_bgcolor = '#2d2d30')
    }
  })
  
  
  output$reviewsTime <- renderPlotly({
    if (reactiveVals$filteredData %>% count() > 0) {
      data <- data.frame(Date = reactiveVals$filteredData$at, 
                         Score = reactiveVals$filteredData$score) %>% 
        mutate(Date = str_extract(Date, "^[^\\s]+")) %>%
        mutate(Date = as.Date(Date)) %>%
        group_by(Date) %>%
        summarise(Total = n(), Score = round(mean(Score))) %>%
        mutate(color = ifelse(Score == "1", "#ff0000", 
                              ifelse(Score == "2", "#ffa700",
                                     ifelse(Score == "3", "#fff400",
                                            ifelse(Score == "4", "#a3ff00", "#2cba00")))))
      
      plot <- ggplot(data, aes(x=Date, y=Total)) +
        geom_point(alpha = 0.7, color = data$color) +
        geom_smooth(method = reactiveVals$methodStr, se =FALSE, alpha = 0.95, color="#d600ff") +
        theme_minimal() + 
        scale_x_date(date_labels = "%Y-%m-%d")
        
      
      plot <- ggplotly(plot)
      
      plot %>% layout(plot_bgcolor='#2d2d30', 
                      paper_bgcolor = '#2d2d30',
                      xaxis = list(
                        title = list(text = 'Time', font = list(color = "#ffffff")),
                        zerolinecolor = '#646467',  
                        zerolinewidth = 2,  
                        gridcolor = '#515154'),  
                      yaxis = list(
                        title = list(text = 'Total', font = list(color = "#ffffff")),
                        zerolinecolor = '#646467',  
                        zerolinewidth = 2,  
                        gridcolor = '#515154'))
    }
  })
  
  
  output$thumbs <- renderPlotly({
    if (reactiveVals$filteredData %>% count() > 0) {
      data <- data.frame(name = reactiveVals$filteredData$userName, 
                          Date = reactiveVals$filteredData$at, 
                          content = reactiveVals$filteredData$content, 
                          score = reactiveVals$filteredData$score, 
                          Likes = reactiveVals$filteredData$thumbsUpCount) %>% 
        filter(Likes > 0) %>%
        filter(Likes >= max(Likes) * (1-0.95)) %>%
        mutate(Date = str_extract(Date, "^[^\\s]+")) %>%
        mutate(Date = as.Date(Date)) %>%
        mutate(color = ifelse(score == "1", "#ff0000", 
                              ifelse(score == "2", "#ffa700",
                                      ifelse(score == "3", "#fff400",
                                            ifelse(score == "4", "#a3ff00", "#2cba00"))))) %>%
        filter(score %in% reactiveVals$selectedScores)
        
      plot <- ggplot(data, aes(x = Date, y = Likes, text = paste("Score:", score))) +
        geom_point(alpha = 0.7, size = 3, color = data$color) +
        theme_minimal() +
        scale_x_date(date_labels = "%Y-%m-%d")
        
      plot <- ggplotly(plot)
      
      plot %>% layout(plot_bgcolor='#2d2d30', 
                      paper_bgcolor = '#2d2d30',
                      xaxis = list(
                        title = list(text = 'Time', font = list(color = "#ffffff")),
                        zerolinecolor = '#646467',  
                        zerolinewidth = 2,  
                        gridcolor = '#515154'),  
                      yaxis = list(
                        title = list(text = 'Likes', font = list(color = "#ffffff")),
                        zerolinecolor = '#646467',  
                        zerolinewidth = 2,  
                        gridcolor = '#515154'))
    }
  })
  
  
  output$repliesPerGrade <- renderPlotly({
    if (reactiveVals$filteredData %>% count() > 0) {
      data <- data.frame(Score = reactiveVals$filteredData$score, 
                         time = reactiveVals$filteredData$at, 
                         reply = reactiveVals$filteredData$repliedAt,
                         thumbs = reactiveVals$filteredData$thumbsUpCount) %>% 
        filter(thumbs >= reactiveVals$thumbsInterval[1]) %>%
        filter(thumbs <= reactiveVals$thumbsInterval[2]) %>%
        group_by(Score) %>%
        summarize(Percentage = sum(str_length(reply) > 0) / n() * 100) %>%
        mutate(color = ifelse(Score == "1", "#ff0000", 
                              ifelse(Score == "2", "#ffa700",
                                     ifelse(Score == "3", "#fff400",
                                            ifelse(Score == "4", "#a3ff00", "#2cba00")))))
      
      plot <- ggplot(data, aes(x = Score, y = Percentage)) +
        geom_bar(stat = "identity", fill = data$color) +
        labs(x = "Score", y = "Percentage of Reviews with Reply") +
        theme_minimal() 
      
      plot <- ggplotly(plot)
      
      plot %>% layout(plot_bgcolor='#2d2d30', 
                      paper_bgcolor = '#2d2d30',
                      xaxis = list(
                        title = list(text = 'Score', font = list(color = "#ffffff")),
                        zerolinecolor = '#646467',  
                        zerolinewidth = 2,  
                        gridcolor = '#515154'),  
                      yaxis = list(
                        title = list(text = 'Percentage of reviews with replies', 
                                     font = list(color = "#ffffff")),
                        zerolinecolor = '#646467',  
                        zerolinewidth = 2,  
                        gridcolor = '#515154'))
    }
  })
  
  
  output$timeReply <- renderPlotly({
    if (reactiveVals$filteredData %>% count() > 0) {
      data <- data.frame(score = reactiveVals$filteredData$score, 
                         time = reactiveVals$filteredData$at, 
                         reply = reactiveVals$filteredData$repliedAt) %>%
        filter(str_length(reply) > 0) %>%
        mutate(between = as.Date(reply) - as.Date(time)) %>%
        filter(between >= 0) %>%
        group_by(score, between) %>%
        summarize(amount = n()) %>%
        filter(amount >= reactiveVals$dataAmount) %>%
        ungroup() %>%
        mutate(color = ifelse(score == "1", "#ff0000", 
                              ifelse(score == "2", "#ffa700",
                                     ifelse(score == "3", "#fff400",
                                            ifelse(score == "4", "#a3ff00", "#2cba00"))))) %>%
        mutate(score = as.factor(score))
      if (data %>% count() > 5) {
        plot <- ggplot(data, aes(x = score, y = between, fill = score)) +
          geom_violin() +
          scale_fill_manual(values = c("1" = "#ff0000", "2" = "#ffa700", "3" = "#fff400", 
                                       "4" = "#a3ff00", "5" = "#2cba00"),
                            breaks = c("1", "2", "3", "4", "5")) +
          labs(x = "Score", y = "Time difference (days)") +
          theme_minimal()
            
        ggplotly(plot) %>% layout(showlegend = FALSE)
      }
    }
  })
}
