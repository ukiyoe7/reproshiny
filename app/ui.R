# DAHBOARD APP
# SANDRO JAKOSKA 12.10.2022

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