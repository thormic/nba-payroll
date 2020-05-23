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
  
  ########################
  # Create a player
  ########################
  
}