###################
# server.R
# For all server needs 
###################

server <- function(input, output, session) {
  
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
}