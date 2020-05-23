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
        tabName = "team",
        
        fluidRow(
          box(
            selectInput(
              "teamChoice",
              "Choose team:",
              # choices = nba_sel[,5],
              choices = team_names[,2],
              selected = "n"),
            actionButton(
              inputId = "submit_team",
              label = "Submit team"),
            width = 6
          ),
          
          valueBoxOutput("teamName", width=6),
          
        ),
        fluidRow(
          valueBoxOutput("ageInfoBox", width=3),
          valueBoxOutput("teamMaxPayrollBox", width=3),
          valueBoxOutput("teamNumPlayersBox", width=3),
          valueBoxOutput("teamMeanPPGBox", width=3)
        ),
        uiOutput("showTeam")
      
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
