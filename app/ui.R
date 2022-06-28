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
library(DBI)
library(tidyverse)
library(lubridate)
library(magrittr)
library(reshape2)
library(zoo)




dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    
    plotOutput(outputId = "sales")
    
  )
)