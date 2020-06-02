###################
# app.R
# 
# Main controller. 
# Used to import your ui and server components; initializes the app.
###################
library(shiny)
library(shinydashboard)
library(DT)

library(gbm)
library(readr)
library(DALEX)
library(ingredients)
library(iBreakDown)
library(ggplot2)
library(scales)
library(randomForest)
library(imputeTS)
library(dplyr)
library(fields)

library(ggthemes)

source('./ui.R')
source('./server.R')


shinyApp(ui, 
         server)
