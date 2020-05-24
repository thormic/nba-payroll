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
          column(width = 1),
          valueBox(
            h3("GBM", align = "center"),
            subtitle = NULL,
            color = "orange",
            width = 3
          ),
          column(width = 4),
          valueBox(
            h3("Random Forest", align = "center"),
            subtitle = NULL,
            color = "orange",
            width = 3
          ),
          column(width = 1),
        ),
        
        fluidRow(
          column(width = 1),
          valueBoxOutput("gbmRMSE", width = 3),
          valueBox(
            h3("RMSE", align = "center"),
            subtitle = NULL,
            color = "blue",
            width = 4
          ),
          valueBoxOutput("rfRMSE", width = 3),
          column(width = 1)
          ),
        
        fluidRow(
          column(width = 1),
          valueBoxOutput("gbmMAD", width = 3),
          valueBox(
            h3("Mean Absolute Deviance", align = "center"),
            subtitle = NULL,
            color = "blue",
            width = 4
          ),
          valueBoxOutput("rfMAD", width = 3),
          column(width = 1),
        ),
        
        fluidRow(
          column(width = 1),
          valueBoxOutput("gbmRsq", width = 3),
          valueBox(
            h3(HTML(paste0("R",tags$sup("2"))), align = "center"),
            subtitle = NULL,
            color = "blue",
            width = 4
          ),
          valueBoxOutput("rfRsq", width = 3),
          column(width = 1),
        ),
      
        fluidRow(
          valueBox(
            h3("Comparison", align = "center"),
            subtitle = NULL,
            width = 12
          ) 
        ),
        
        fluidRow(
         box(
           plotOutput("resBoxPlot"),
           width = 6
         ),
         box(
           plotOutput("dropLossPlot"),
           width = 6
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
