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
      round(mean(team_df()$Age),2), "Mean of Age", icon = icon("birthday-cake"), color = "orange", width=3
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
      paste("$", format(max(team_df()$Payroll), big.mark = ","), sep = ""), "Max Payroll",icon = icon("dollar-sign"), color = "green", width=3
    )
  })
  
  output$teamPayrollByPosition <- renderPlot({
    ggplot(team_df(), aes(Pos,Payroll))+
      geom_bar(stat = "identity")+
      scale_y_continuous("Payroll in Dollars", labels = dollar_format(suffix = "$", prefix = ""))+
      theme_hc()+ scale_colour_hc() +
      ggtitle('Payroll by position')
  })
  
  output$teamPayrollHist <- renderPlot({
    ggplot(team_df(), aes(x=Payroll)) + 
      geom_histogram(aes(y=..density..), colour="black", bins = dim(team_df())[1])+
      scale_x_continuous("Payroll in Dollars", labels = dollar_format(suffix = "$", prefix = ""))+
      geom_density(alpha=.6, fill="#00ffff")+
      theme_hc()+ scale_colour_hc() +
      ggtitle('Payroll')
  })
  
  output$teamHeightHist <- renderPlot({
    ggplot(team_df(), aes(x=height)) + 
      geom_histogram(aes(y=..density..), colour="black", bins = dim(team_df())[1])+
      geom_density(alpha=.6, fill="#00ffff") +
      theme_hc()+ scale_colour_hc() +
      ggtitle('Height')
  })
  
  output$teamPayrollPPGRegression <- renderPlot({
    ggplot(team_df(), aes(x=PPG, y=Payroll)) +
      geom_point(size=4) +    # Use hollow circles
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
          title = "Team Data",
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
  
  ########################
  # Create a player
  ########################
  
}