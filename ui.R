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
  dashboardSidebar(selectizeInput('appName', 'App ID', choices = NULL,options = list(maxOptions=100000, placeholder = 'Select app name...')),
                   dateRangeInput('dateRange', 'Date', format = 'yyyy-mm-dd'),
                   actionButton('allDates', 'Show all data')),
  dashboardBody(DTOutput('comments'), plotOutput('scoreHist'), plotlyOutput("reviewsPie"), plotlyOutput("reviewsTime"), plotlyOutput("thumbs"))
)
