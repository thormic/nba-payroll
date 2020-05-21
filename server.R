###################
# server.R
# For all server needs 
###################

server <- function(input, output, session) {
  
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
}