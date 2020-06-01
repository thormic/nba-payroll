###################
# server.R
# For all server needs 
###################

server <- function(input, output, session) {
  
  ########################
  # Data summary
  ########################  

  output$varsTable <- DT::renderDataTable(
    variables_df,
    rownames = FALSE,
    options = list(info = FALSE,
                   autoWidth = TRUE,
                   ordering = TRUE,
                   searching = FALSE,
                   paging = FALSE,
                   scrollX = TRUE,
                   scrollY = 150,
                   scrollColapse = TRUE
    )
  )
  
  output$obsTable <- DT::renderDataTable(
    nba_sel,
    extensions = "FixedColumns",
    rownames = FALSE,
    options = list(info = FALSE,
                   autoWidth = TRUE,
                   ordering = TRUE,
                   searching = FALSE,
                   paging = FALSE,
                   scrollX = TRUE,
                   scrollY = 300,
                   scrollColapse = TRUE,
                   fixedColumns = list(leftColumns = 1)
    )
  )
  
  firstChoiceDf <- reactive({
      nba_sel[[ input$firstVarChoice ]]
  })
  
  secondChoiceDf <- reactive({
    nba_sel[[ input$secondVarChoice ]]
  })
  
  thirdChoiceDf <- reactive({
    nba_sel[[ input$thirdVarChoice ]]
  })

  output$firstHistogram <- renderPlot({
    first_hist <- firstChoiceDf()
    hist(first_hist,
         main = paste("Histogram of", input$firstVarChoice),
         xlab=input$firstVarChoice)
  })
  
  output$secondHistogram <- renderPlot({
    second_hist <- secondChoiceDf()
    hist(second_hist, 
         main = paste("Histogram of", input$secondVarChoice),
         xlab=input$secondVarChoice)
  })
  
  output$thirdHistogram <- renderPlot({
    third_hist <- thirdChoiceDf()
    hist(third_hist, 
         main = paste("Histogram of", input$thirdVarChoice),
         xlab=input$thirdVarChoice)
  })
  
  
  ########################
  # Models
  ########################
  
  output$gbmRMSE <- renderInfoBox({
    infoBox(
      h3(round(nba_gbm_prf$measures$rmse, 2), align = "center"), 
      subtitle = NULL,
      color = "green"
    )
  })
  output$gbmMAD <- renderInfoBox({
    infoBox(
      h3(round(nba_gbm_prf$measures$mad, 2), align = "center"),
      subtitle = NULL,
      color = "red"
    )
  })
  output$gbmRsq <- renderInfoBox({
    infoBox(
      h3(round(nba_gbm_prf$measures$r2, 2), align = "center"),
      subtitle = NULL,
      color = "green"
    )
  })
  
  output$rfRMSE <- renderInfoBox({
    infoBox(
      h3(round(nba_rf_prf$measures$rmse, 2), align = "center"), 
      subtitle = NULL,
      color = "red"
    )
  })
  output$rfMAD <- renderInfoBox({
    infoBox(
      h3(round(nba_rf_prf$measures$mad, 2), align = "center"),
      subtitle = NULL,
      color = "green"
    )
  })
  output$rfRsq <- renderInfoBox({
    infoBox(
      h3(round(nba_rf_prf$measures$r2, 2), align = "center"),
      subtitle = NULL,
      color = "green"
    )
  })
  
  output$resBoxPlot <- renderPlot({
    plot(nba_gbm_prf, nba_rf_prf, geom = "boxplot") +
      scale_y_continuous("Absolute residuals in Dollars", labels = dollar_format(suffix = "$", prefix = "")) + 
      ggtitle("Distributions of model absolute residuals")
  })
  
  
  output$dropLossPlot <- renderPlot({
    plot(nba_gbm_part, nba_rf_part,
         max_vars = 12, bar_width = 4, show_boxplots = FALSE) 
  })
  
  
  output$modelScatterPlot <- renderPlot({
    plot(nba_gbm_md, nba_rf_md,
         variable = "y", yvariable = "y_hat") +
      scale_x_continuous("Value in Dollars", labels = dollar_format(suffix = "$", prefix = "")) + 
      scale_y_continuous("Estimated value in Dollars", labels = dollar_format(suffix = "$", prefix = "")) + 
      facet_wrap(~label) +
      geom_abline(slope = 1) + theme(legend.position = "none") +
      ggtitle("Diagnostics plot Predicted vs True target values", "")
  })
  
  ########################
  # Team
  ########################
  
  team_df <- eventReactive(
    input$submit_team,
    {
      Tm_selected <- filter(team_names, TeamFull == input$teamChoice)[[1]]
      filter(nba_sel, Tm == Tm_selected)
    })

  output$teamName <- renderValueBox({
    valueBox(
      value = tags$p(input$teamChoice, style = "font-size: 150%;"),
      "Team",
      icon = icon("basketball-ball"),
      color = "light-blue"
    )
  })
  
  output$ageInfoBox <- renderValueBox({
    valueBox(
      round(mean(team_df()$Age),2), "Mean of Age", icon = icon("birthday-cake"), color = "teal", width=3
    )
  })
  output$teamNumPlayersBox <- renderValueBox({
    valueBox(
      dim(team_df())[1], "Players",  icon = icon("users"), color = "blue", width=3
    )
  })
  output$teamMeanPPGBox <- renderValueBox({
    valueBox(
      round(mean(team_df()$PPG),2), "Mean PPG", round(mean(team_df()$PPG),2), icon = icon("trophy"), color = "purple", width=3
    )
  })
  output$teamMaxPayrollBox <- renderValueBox({
    valueBox(
      paste("$", format(max(team_df()$Salary), big.mark = ","), sep = ""), "Max Salary",icon = icon("dollar-sign"), color = "green", width=3
    )
  })
  
  output$teamPayrollByPosition <- renderPlot({
    ggplot(team_df(), aes(Pos,Salary))+
      # geom_bar(stat = "identity")+
      geom_point(size = 4) +
      scale_y_continuous("Salaries in Dollars", labels = dollar_format(suffix = "$", prefix = ""))+
      theme_hc()+ scale_colour_hc() +
      ggtitle('Salaries by position') +
      theme(plot.title = element_text(size = 13, face = "bold"))
  })
  
  output$teamPayrollHist <- renderPlot({
    ggplot(team_df(), aes(x=Salary)) + 
      geom_histogram(aes(y=..density..), colour="black", bins = dim(team_df())[1])+
      scale_x_continuous("Salaries in Dollars", labels = dollar_format(suffix = "$", prefix = ""))+
      geom_density(alpha=.6, fill="#00ffff")+
      theme_hc()+ scale_colour_hc() +
      ggtitle('Salaries')
  })
  
  output$teamHeightHist <- renderPlot({
    ggplot(team_df(), aes(x=height)) + 
      geom_histogram(aes(y=..density..), colour="black", bins = dim(team_df())[1])+
      geom_density(alpha=.6, fill="#00ffff") +
      theme_hc()+ scale_colour_hc() +
      ggtitle('Height')
  })
  
  output$teamPayrollPPGRegression <- renderPlot({
    ggplot(team_df(), aes(x=PPG, y=Salary)) +
      geom_point(size=4) +    # Use hollow circles
      ylab("Salary") +
      scale_y_continuous("Salaries in Dollars", labels = dollar_format(suffix = "$", prefix = "")) +
      geom_smooth(method=lm, color="#00ffff") +
      theme_hc()+ scale_colour_hc() +
      ggtitle('Points per game')
  })
  
  output$teamTable <- DT::renderDataTable(
    team_df(),
    extensions = "FixedColumns",
    rownames = FALSE,
    options = list(info = FALSE,
                   autoWidth = TRUE,
                   ordering = TRUE,
                   searching = FALSE,
                   paging = FALSE,
                   scrollX = TRUE,
                   scrollY = 300,
                   scrollColapse = TRUE,
                   fixedColumns = list(leftColumns = 1)
    )
  )
  
  output$showTeam <- renderUI({
    if(is.null(team_df()))return()
    fluidPage(
      fluidRow(
        box(
          plotOutput("teamPayrollByPosition"),
          width = 6
        ),
        box(
          plotOutput("teamPayrollPPGRegression"),
          width = 6
        )
      ),
      fluidRow(
        box(
          plotOutput("teamPayrollHist"),
          width = 6
        ),
        box(
          plotOutput("teamHeightHist"),
          width = 6
        )
      ),
      fluidRow(
        box(
          title = strong("Team Data"),
          DT::dataTableOutput("teamTable"),
          width = 12
        )
      )
    )
  })
  
  # library(tidyr)
  # cols_stats = c("Age", 'G', 'PPG', 'MPG', 'PTS','weight')
  # ggplot(gather(df_temp[cols_stats]), aes(value)) + 
  #   geom_histogram() + 
  #   facet_wrap(~key, scales = 'free_x')
  
  ########################
  # Compare/ show players
  ########################
  
  player_df <- eventReactive(
  input$submit_player,
  {
    filter(nba_sel, Player == input$playerChoice)
  })
  
  compare_df <- eventReactive(
    input$compare_player,
    {
      filter(nba_sel, Player == input$compareChoice)
    })
  
  third_df <- eventReactive(
    input$compare_player,
    {
      filter(nba_sel, Player == input$thirdChoice)
    })
  
  output$playerName <- renderValueBox({
    valueBox(
      value = tags$p(player_df()$Player, style = "font-size: 150%;"),
      subtitle = filter(team_names, Tm == player_df()$Tm)[[2]],
      icon = icon("user"),
      color = "light-blue"
    )
  })
  output$playerAge <- renderValueBox({
    valueBox(
      player_df()$Age, 
      "Age",
      icon = icon("birthday-cake"),
      color = "teal"
    )
  })
  output$playerGames <- renderValueBox({
    valueBox(
      player_df()$G,
      "Games played",
      icon = icon("basketball-ball"),
      color = "purple"
    )
  })
  output$playerPayroll <- renderValueBox({
    valueBox(
      paste("$", format((player_df()$Salary + 16427), big.mark = ","), sep=''),
      "Salary", 
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  # simulating case for this player observation with one variable changing
  # Different variables to choose from
  output$simulateVariable <- renderPlot({
    nba_cp_pg <- ingredients::ceteris_paribus(nba_gbm_exp, 
                                              new_observation = player_df(), 
                                              variables = input$firstCPChoice)
    plot(nba_cp_pg) +
      ylab("Salary") + 
      xlab(input$firstCPChoice) +
      ggtitle(paste("Plot for", input$firstCPChoice)) +
      geom_vline(xintercept = as.numeric(player_df()[input$firstCPChoice]), linetype = "dotted", color = "blue") +
      geom_text(mapping = aes(x = as.numeric(player_df()[input$firstCPChoice]), 
                              y = 0, 
                              label = paste("Current", input$firstCPChoice)),
                size=4.5, angle=90, vjust=-0.4, hjust=0)
  })
  

  output$simulateVariable2 <- renderPlot({
    nba_cp2_pg <- ingredients::ceteris_paribus(nba_gbm_exp, 
                                               new_observation = player_df(), 
                                               variables = input$secondCPChoice)
    plot(nba_cp2_pg) + 
      ylab("Salary") + 
      xlab(input$secondCPChoice) +
      ggtitle(paste("Plot for", input$secondCPChoice)) +
      geom_vline(xintercept = as.numeric(player_df()[input$secondCPChoice]), linetype = "dotted", color = "blue") +
      geom_text(mapping = aes(x = as.numeric(player_df()[input$secondCPChoice]), 
                              y = 0, 
                              label = paste("Current", input$secondCPChoice)),
                size=4.5, angle=90, vjust=-0.4, hjust=0)
  })
  
  output$simulateVariable3 <- renderPlot({
    nba_cp3_pg <- ingredients::ceteris_paribus(nba_gbm_exp, 
                                               new_observation = player_df(), 
                                               variables = input$thirdCPChoice)
    plot(nba_cp3_pg) +
      ylab("Salary") + 
      xlab(input$thirdCPChoice) +
      ggtitle(paste("Plot for", input$thirdCPChoice)) +
      geom_vline(xintercept = as.numeric(player_df()[input$thirdCPChoice]), linetype = "dotted", color = "blue") +
      geom_text(mapping = aes(x = as.numeric(player_df()[input$thirdCPChoice]), 
                              y = 0, 
                              label = paste("Current", input$thirdCPChoice)),
                size=4.5, angle=90, vjust=-0.4, hjust=0)
  })
  
  
  # Switch button
  model_chosen <- reactive({
    switch(input$modelChoice,
           gbm = nba_gbm_exp,
           rf = nba_rf_exp,
           nba_gbm_exp)
  })
  
  # Variables and how they affect certain players payroll
  # Box with choosable player
  output$playerBreakdown <- renderPlot({
    nba_plr_bd <- break_down(model_chosen(), new_observation = player_df())
    nba_plr_bd$label = paste("Break Down for ", player_df()[1])
    plot(nba_plr_bd, digits = 0, max_features = 10) +  
      scale_y_continuous(labels = dollar_format(suffix = "$", prefix = ""), name = "Salary", limits = 400000*c(1,100), breaks = 1000000*seq(0,45,8))
    
  })
  
  output$compareBreakdown <- renderPlot({
    nba_comp_bd <- break_down(model_chosen(), new_observation = compare_df())
    nba_comp_bd$label = paste("Break Down for ", compare_df()[1])
    plot(nba_comp_bd, digits = 0, max_features = 10) +  
      scale_y_continuous(labels = dollar_format(suffix = "$", prefix = ""), name = "Salary", limits = 400000*c(1,100), breaks = 1000000*seq(0,45,8))
    
  })
  
  output$thirdBreakdown <- renderPlot({
    nba_third_bd <- break_down(model_chosen(), new_observation = third_df())
    nba_third_bd$label = paste("Break Down for ", third_df()[1])
    plot(nba_third_bd, digits = 0, max_features = 10) +  
      scale_y_continuous(labels = dollar_format(suffix = "$", prefix = ""), name = "Salary", limits = 400000*c(1,100), breaks = 1000000*seq(0,45,8))
    
  })


  output$showPlayer <- renderUI ({
    if(is.null(player_df()))return()
    fluidPage(
      fluidRow(
        box(
          title = strong("Ceteris paribus simulation"),
          selectInput(
            "firstCPChoice",
            p("Choose #1 variable:", style = "font-weight: lighter; margin: 0px"),
            choices = colnames(Filter(is.numeric, nba_sel))[-1]
          ),
          selectInput(
            "secondCPChoice",
            p("Choose #2 variable:", style = "font-weight: lighter; margin: 0px"),
            choices = colnames(Filter(is.numeric, nba_sel))[-1],
            selected = "MP"
          ),
          selectInput(
            "thirdCPChoice",
            p("Choose #3 variable:", style = "font-weight: lighter; margin: 0px"),
            choices = colnames(Filter(is.numeric, nba_sel))[-1],
            selected = "PPG"
          ),
          width = 3
        ),
        box(
          plotOutput("simulateVariable"),
          width = 3
        ),
        box(
          plotOutput("simulateVariable2"),
          width = 3
        ),
        box(
          plotOutput("simulateVariable3"),
          width = 3
        )
      ),
      fluidRow(
        column(12,div(style = "height:60px"))
      ),
      fluidRow(
        column(width = 3),
        valueBox(
          h3("Compare players", align = "center"),
          subtitle = NULL,
          color = "orange",
          width = 6
        ),
        column(width = 3)
      ),
      fluidRow(
        box(
          column(radioButtons("numberChoice", 
                       "How many players to compare?",
                       c("1" = "one",
                         "2" = "two")),
                 width = 2),
          column(radioButtons("modelChoice", 
                       "Model to use:",
                       c("GBM" = "gbm",
                         "Random Forest" = "rf")),
                 width = 2),
          uiOutput("comparePlayers"),
          width = 12
        )
        ),
      uiOutput("showComparePlots")
    )
        
  })
  
  output$comparePlayers <- renderUI ({
    if (input$numberChoice == "one") {
      column(
        selectInput(
        "compareChoice",
        paste("Choose a player to compare with ", player_df()$Player, ":", sep = ""),
        choices = nba_sel[,1],
        selected = "Stephen Curry"),
      actionButton(
        inputId = "compare_player",
        label = "Compare player"),
      width = 4)
    }
    
    else if (input$numberChoice == "two") {
      div(
        column(
        selectInput(
        "compareChoice",
        paste("Choose first player to compare with ", player_df()$Player, ":", sep = ""),
        choices = nba_sel[,1],
        selected = "Stephen Curry"),
        width = 4),
        column(
        selectInput(
          "thirdChoice",
          paste("Choose second player to compare with ", player_df()$Player, ":", sep = ""),
          choices = nba_sel[,1],
          selected = "Russell Westbrook"),
        actionButton(
          inputId = "compare_player",
          label = "Compare players"),
        width = 4),
        width = 8)
    }


  })
  
  output$showComparePlots <- renderUI ({
    if (is.null(input$compare_player)) return()
    if (input$numberChoice == "one") {
      fluidRow(
        box(
          plotOutput("playerBreakdown"),
          width = 6
        ),
        box(
          plotOutput("compareBreakdown"),
          width = 6
        )
      )
    }
    else if (input$numberChoice == "two") {
      fluidRow(
        box(
          plotOutput("playerBreakdown"),
          width = 4
        ),
        box(
          plotOutput("compareBreakdown"),
          width = 4
        ),
        box(
          plotOutput("thirdBreakdown"),
          width = 4
        )
      )
    }
  })

  
}