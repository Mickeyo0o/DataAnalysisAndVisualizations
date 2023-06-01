#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(shinyalert)

dashboardPage(
  dashboardHeader(),
  
  dashboardSidebar(
    tags$div(
      style = "text-align: center;",
      img(src = "image1.png", height = 150, width = 150)
    ),
    tags$div(
      style = "position: fixed; top: 300; width: 100%; text-align: center;",
      selectizeInput('appName', 'App ID', choices = NULL, 
                     options = list(maxOptions = 100000, 
                                    placeholder = 'Select app name...'), width = 225),
      dateRangeInput('dateRange', 'Date', format = 'yyyy-mm-dd', width = 225),
      actionButton('allDates', 'Show all data'),
    ),
    
    tags$div(
      style = "position: fixed; bottom: 0; width: 100%; text-align: center;",
      actionButton("Help", "Help section")
    )
  ),
  dashboardBody(
    tags$style(HTML(".body { background-color: #1e1e1e; color: #888 }
                    .box { background-color: #2d2d30; border-top-color: #2d2d30;}
                    .custom-box { background-color: #383838;}
                    .box-body { border-top-left-radius: 15px;
                    border-top-right-radius: 15px; border-bottom-right-radius: 15px;
                    border-bottom-left-radius: 15px;  padding: 5px;}
                    .box-header { background-color: #252526; color: #FFFFFF; }
                    .box-title {color: #FFFFFF; }
                    .control-label { color: #FFFFFF; }
                    .radio {color: #FFFFFF; } 
                    .irs--shiny .irs-bar {border-top-color: #252526; 
                              border-bottom-color: #252526;
                              background-color: #d600ff; }
                    .irs--shiny .irs-handle {background-color: #252526;
                                              border-color: #252526;}
                    .irs--shiny .irs-handle.state_hover {background-color: #252526;
                                              border-color: #252526;}
                    .irs--shiny .irs-handle:hover {background-color: #252526;
                                              border-color: #252526;}
                    .irs--shiny .irs-from {background-color: #d600ff;}
                    .irs--shiny .irs-to {background-color: #d600ff;}
                    .irs--shiny .irs-line {background: 
                    linear-gradient(rgb(0, 0, 0) -50%, rgb(119, 119, 119) 150%)}
                    .irs--shiny .irs-min {background-color: rgba(0,0,0,0.4);}
                    .irs--shiny .irs-max {background-color: rgba(0,0,0,0.4);}
                    .skin-blue .main-header .navbar {background-color: #181818;}
                    .skin-blue .main-header .logo {background-color: #181818;}
                    .skin-blue .main-header .logo:hover {background-color: #181818;}
                    .skin-blue .main-sidebar {background-color: #181818;}
                    .skin-blue .main-header .navbar .sidebar-toggle:hover {background-color: #2d2d2d;}
                    .selectize-control.multi .selectize-input > div.active {background-color: #d600ff;} 
                    input[type='radio'] { accent-color: #d600ff;}
                    .irs--shiny .irs-single {background-color: #d600ff;}
                    div.datatables {color: #888}
                    .rating-box-1 {background-color: #ff0000; color: #ffffff;}
                    .rating-box-2 {background-color: #ffa700; color: #000000;}
                    .rating-box-3 {background-color: #fff400; color: #000000;}
                    .rating-box-4 {background-color: #a3ff00; color: #000000;}
                    .rating-box-5 {background-color: #2cba00; color: #ffffff;}
                    .sweet-alert {background-color: #373737;}")),
    
                    
    class = "body",
    fluidPage(
      fluidRow(
          box(
            width = 4,
            title = "Score distribution",
            plotlyOutput("reviewsPie"),
          ),
          box(
            width = 8,
            title = "Number of reviews over time",
            fluidRow(
              column(width = 10, plotlyOutput("reviewsTime")),
              column(width = 2, radioButtons(
                inputId = "methodReviews",
                label = "Choose a smoothing method:",
                choices = c("lm", "glm", "gam", "loess", "auto"),
                selected = "auto"
                )
              )
          )
        ),
      ),
      fluidRow(
        box(
          width = 6,
          title = "Most liked reviews",
          fluidRow(
            column(width = 10, plotlyOutput("thumbs")),
            column(width = 2, selectInput(inputId = "scoresSelect", 
                                          label = "Select score values", 
                                          choices = c(1), 
                                          multiple = TRUE))
          )
        ),
        box(
          width = 6,
          title = "Selected review",
          uiOutput("selectedReview")
        )
      ),
      fluidRow(
        box(
          width = 6,
          title = "Percentage of reviews that received a reply",
          plotlyOutput("repliesPerGrade"),
          sliderInput(inputId = "thumbsInterval",
                      label = "Select an interval of thumbs up for a comment:",
                      min = 0,
                      max = 0,
                      value = c(0,0),
                      step = 1),
        ),
        box(
          width = 6,
          title = "Time for a review to receive a reply",
          plotlyOutput("timeReply"),
          sliderInput("dataAmount", "Choose a number of minimum data amount at given point: ", min = 1, max = 1, value = 1, step = 1)
        )
      ),
      fluidRow(
        box(
          width = 12,
          title = "Reviews data table",
          DTOutput("comments")
        )
      )
    )
  )
)
