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
library(scales)


dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(
    
    fluidRow(
      
        box(title = "EMISSÃO", status = "primary",
            fluidRow(
              
                plotOutput(outputId = "sales")
              
            )
        )
      ,
    
        width = 6,
        offset = 0,
        box(title = "FATURADO", status = "primary",
            fluidRow(
             
                plotOutput(outputId = "sales_fat")
              
            )
        )
      )
    )
    
  )

