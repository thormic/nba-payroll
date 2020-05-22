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
            title = "Description",
            includeMarkdown("include.md"),
            width = 12
          )
        ),
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
        tabName = "models",
          fluidRow(
           box(
             plotOutput("resBoxPlot"),
             width = 12
           ) 
          ),
        fluidRow(
          box(
            plotOutput("dropLossPlot"),
            width = 12
          ) 
        ),
        fluidRow(
          box(
            plotOutput("modelScatterPlot"),
            width = 12
          ) 
        )
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
        tabName = "player",
          fluidRow(
            box(
              selectInput(
                "playerChoice",
                "Choose a player:",
                choices = nba_sel[,1],
                selected = "n"),
              actionButton(
                inputId = "submit_player",
                label = "Submit player"),
              width = 6
            )
          ),
          fluidRow(
            box(
              textOutput("playerName"),
              width = 4
            ),
            box(
              textOutput("playerAge"),
              width = 4
            ),
            box(
              textOutput("playerPayroll"),
              width = 4
            )
          ),
          fluidRow(
            box(
              plotOutput("simulateVariable"),
              width = 6
            ),
            box(
              plotOutput("simulateVariable2"),
              width = 6
            ) 
          ),
          fluidRow(
            box(
              plotOutput("playerBreakdown"),
              width = 6
            ),
            box(
              plotOutput("playerShap"),
              width = 6
            )
          )
      ),
      
      ########################
      # Create a player
      ########################
      tabItem(
        tabName = "create"
      )
      
    )
)
