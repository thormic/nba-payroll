###################
# body.R
# 
# Body for the ui. 
# When multiple tabs, split those into their own
# components as well.
###################
body <- dashboardBody(
  tags$head(tags$style(HTML(".small-box {height: 100%}"))),
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
            title = strong("Description"),
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
            title = strong("Data controler"),
            selectInput(
              "firstVarChoice",
              p("Choose #1 variable:", style = "font-weight: lighter; margin: 0px"),
              choices = colnames(Filter(is.numeric, nba_sel))
            ),
            selectInput(
              "secondVarChoice",
              p("Choose #2 variable:", style = "font-weight: lighter; margin: 0px"),
              choices = colnames(Filter(is.numeric, nba_sel)),
              selected = "Age"
            ),
            selectInput(
              "thirdVarChoice",
              p("Choose #3 variable:", style = "font-weight: lighter; margin: 0px"),
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
            title = strong("Observations"),
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
          column(width = 4),
          valueBox(
            h3("GBM", align = "center"),
            subtitle = NULL,
            color = "orange",
            width = 4
          ),
          valueBox(
            h3("Random Forest", align = "center"),
            subtitle = NULL,
            color = "orange",
            width = 4
          )
        ),
        
        fluidRow(
          box(
            h2("RMSE", align = "center"),
            width = 4
          ),
          infoBoxOutput("gbmRMSE", width = 4),
          infoBoxOutput("rfRMSE", width = 4)
          ),
        
        fluidRow(
          box(
            h2("Mean Absolute Deviance", align = "center"),
            width = 4
          ),
          infoBoxOutput("gbmMAD", width = 4),
          infoBoxOutput("rfMAD", width = 4)
        ),
        
        fluidRow(
          box(
            h2(HTML(paste0("R",tags$sup("2"))), align = "center"),
            width = 4
          ),
          infoBoxOutput("gbmRsq", width = 4),
          infoBoxOutput("rfRsq", width = 4)
        ),
        fluidRow(
          column(12,div(style = "height:75px"))
        ),
      
        fluidRow(
          column(width = 3),
          valueBox(
            h3("Comparison", align = "center"),
            subtitle = NULL,
            color = "orange",
            width = 6
          ),
          column(width = 3),
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
        tabName = "player",
        fluidPage(
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
              width = 4
            ),
            valueBoxOutput("playerName",
                          width = 8
              )
          ),
          fluidRow(
            valueBoxOutput("playerAge",
                          width = 4
            ),
            valueBoxOutput("playerGames",
                          width = 4
            ),
            valueBoxOutput("playerPayroll",
                          width = 4
            )
          ),
          uiOutput("showPlayer")
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
