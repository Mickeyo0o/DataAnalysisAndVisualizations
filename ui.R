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

dashboardPage(
  dashboardHeader(),
  
  dashboardSidebar(
    selectizeInput('appName', 'App ID', choices = NULL, 
                   options = list(maxOptions = 100000, 
                                  placeholder = 'Select app name...')),
    dateRangeInput('dateRange', 'Date', format = 'yyyy-mm-dd'),
    actionButton('allDates', 'Show all data')
  ),
  dashboardBody(
    tags$style(HTML(".body { background-color: #1e1e1e; }
                    .box { background-color: #2d2d30; border-top-color: #2d2d30;}
                    .box-header { background-color: #252526; color: #FFFFFF; }
                    .box-title {color: #FFFFFF; }
                    .control-label { color: #FFFFFF; }
                    .radio {color: #FFFFFF; } 
                    .irs--shiny .irs-bar {border-top-color: #252526; 
                              border-bottom-color: #252526;
                              background-color: #d600ff; }
                    .irs--shiny .irs-handle {background-color: #252526;
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
                    .skin-blue .main-header .navbar .sidebar-toggle:hover {background-color: #2d2d2d;}")),
                    
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
          title = "Selected review"
        )
      ),
      fluidRow(
        box(
          width = 6,
          title = "Replies per Grade",
          plotlyOutput("repliesPerGrade"),
          sliderInput(inputId = "thumbsInterval",
                      label = "Select an interval of thumbs up for a comment:",
                      min = 0,
                      max = 0,
                      value = c(0,0),
                      step = 1),
        )
      )
      
    )
  )
)
