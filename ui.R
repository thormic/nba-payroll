###################
# ui.R
# 
# Initializes the ui. 
# Used to load in header, sidebar, and body components.
###################
source('./components/header.R')
source('./components/sidebar.R')
source('./components/body.R')


ui <- dashboardPage(
  title = "NBA Players Payroll 2017/18",
  header = header,
  sidebar =  sidebar,
  body = body)
