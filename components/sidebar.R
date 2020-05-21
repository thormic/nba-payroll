###################
# sidebar.R
# 
# Sidebar menu options for the ui.
###################
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Data summary", tabName = "dataset", icon = icon("database")),
    menuItem("Model comparison", tabName = "models", icon = icon("chart-area")),
    menuItem("Team", tabName = "team", icon = icon("users")),
    menuItem("Player", tabName = "player", icon = icon("user")),
    menuItem("Create a player", tabName = "create", icon = icon("user-plus"))
    
  )
)
