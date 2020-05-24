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
            p("Goal of this project is to understand how certain variables affected payroll of NBA Players in 2017/18 season.
            Firstly, we used GBM (Generalized Boosted Model) and RF (Random Forest) models to predict the salaries of the players.
            Then, using ",
              em("DALEX"),
              " package, we measured the impact of each variable on the predicted salary. We also performed ",
              em("Ceteris Paribus"), 
              " analysis - you look at it as 'what if some variable had different value' analysis."), 
            p("Sidebar on the left shows different parts of the analysis:",
              strong("Data summary, "),
              strong("Model comparison, "),
              strong("Team, "),
              strong("Player "),
              " - each part responsible for different part of the analysis."),
            width = 6
          ),
          box(
            DT::dataTableOutput("varsTable"),
            width = 4
          ),
          box(
            h5("Made by:"),
            h4("Bartlomiej Kowalczuk & Michal Thor"),
            background = "orange",
            width = 2
          )
        ),
        
        fluidRow(
          box(
            title = "Data controler",
            selectInput(
              "firstVarChoice",
              "Choose #1 variable:",
              choices = colnames(Filter(is.numeric, nba_sel))
            ),
            selectInput(
              "secondVarChoice",
              "Choose #2 variable:",
              choices = colnames(Filter(is.numeric, nba_sel)),
              selected = "Age"
            ),
            selectInput(
              "thirdVarChoice",
              "Choose #3 variable:",
              choices = colnames(Filter(is.numeric, nba_sel)),
              selected = "PPG"
            ),
            width = 3,
          ),
          box(plotOutput("firstHistogram"),
              width = 3),
          box(plotOutput("secondHistogram"),
              width = 3),
          box(plotOutput("thirdHistogram"),
              width = 3)
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
