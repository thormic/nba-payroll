###################
# server.R
# For all server needs 
###################

server <- function(input, output, session) {
  
  ########################
  # Data summary
  ########################  

  output$descText <- renderText({
    
  }
  )
  
  histPlot_df <- eventReactive(
    input$submit,
    {
      nba_sel[[ input$columnChoice ]]
  })

  output$histPlot <- renderPlot({
    hist_data <- histPlot_df()[ seq_len(input$slider*nrow(nba_sel)) ]
    hist(hist_data)
    # FIX for top % of observations
    # data <- histPlot_df() %>% filter(row_number() < nrow(.) * input$slider)
  })
  
  output$corrPlot <- renderPlot({
    corr_data <- cor(nba_sel[,-c(1:3,5)])
    corrplot(corr_data)
  })
  
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
  
  ########################
  # Models
  ########################
  
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
  
  ########################
  # Compare/ show players
  ########################
  
  player_df <- eventReactive(
  input$submit_player,
  {
    filter(nba_sel, Player == input$playerChoice)
  })
  
  # Statistics for player
  output$playerName <- renderValueBox({
    valueBox(
      value = tags$p(player_df()[1], style = "font-size: 150%;"),
      "",
      icon = icon("user"),
      color = "purple"
    )
  })
  output$playerAge <- renderValueBox({
    valueBox(
      player_df()[4], 
      "Age",
      icon = icon("birthday-cake"),
      color = "orange"
    )
  })
  output$playerGames <- renderValueBox({
    valueBox(
      player_df()[6], 
      "Games played",
      icon = icon("basketball-ball"),
      color = "light-blue"
    )
  })
  output$playerPayroll <- renderValueBox({
    valueBox(
      player_df()[2],
      "Payroll", 
      icon = icon("money-bill"),
      color = "green"
    )
  })
  
  # simulating case for this player observation with one variable changing
  # Different variables to choose from
  output$simulateVariable <- renderPlot({
    nba_cp_pg <- ingredients::ceteris_paribus(nba_gbm_exp, new_observation = player_df(), variables = "Age", variable_splits = list(Age = seq(18,45,0.1)))
    plot(nba_cp_pg) + geom_vline(xintercept = as.numeric(player_df()[4]), linetype = "dotted", color = "blue")
  })

  output$simulateVariable2 <- renderPlot({
    nba_cp_pg <- ingredients::ceteris_paribus(nba_gbm_exp, new_observation = player_df(), variables = "PPG", variable_splits = list(PPG = seq(0,36,3)))
    plot(nba_cp_pg) + geom_vline(xintercept = as.numeric(player_df()[32]), linetype = "dotted", color = "blue")
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
      scale_y_continuous(labels = dollar_format(suffix = "$", prefix = ""), name = "Payroll", limits = 400000*c(1,100), breaks = 1000000*seq(0,45,8))
    
  })

  # Variables contribution to payroll for given player
  output$playerShap <- renderPlot({
    nba_shap <- shap(model_chosen(), new_observation = player_df())
    plot(nba_shap, max_features = 10)
  })


  
  ########################
  # Create a player
  ########################
  
}