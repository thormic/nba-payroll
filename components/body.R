###################
# body.R
# 
# Body for the ui. 
# When multiple tabs, split those into their own
# components as well.
###################
body <- dashboardBody(
    shinyDashboardThemes(
      theme = "blue_gradient"
    ),
    tabItems(
      
      ########################
      # Data summary
      ########################
      tabItem(
        tabName = "dataset",
        fluidRow(
          box(
            title = "Data controler",
            # Choose a column
            selectInput(
              "columnChoice",
              "Choose a column:",
              choices = colnames(nba_sel),
              selected = "n"),
            sliderInput("slider", "Percentage of observations:", 0, 1.0, 0.1),
            # Create an eventReactive element
            actionButton(
              inputId = "submit",
              label = "Submit column"),
            width = 4
          ),
          box(plotOutput("histPlot"),
              width = 8)
        ),
  
        fluidRow(
          box(
            title = "Correlation matrix",
            plotOutput("corrPlot"),
            width = 12
          )
        ),
        
        fluidRow(
          box(
            title = "Observations",
            DT::dataTableOutput("obsTable"),
            width = 12
          )
        )
      ),
      
      ########################
      # Models
      ########################
      tabItem(
        tabName = "models"
      ),
      
      ########################
      # Team
      ########################
      tabItem(
        tabName = "team"
      ),
      
      ########################
      # Compare/ show players
      ########################
      tabItem(
        tabName = "player"
      ),
      
      ########################
      # Create a player
      ########################
      tabItem(
        tabName = "create"
      )
      
    )
)
