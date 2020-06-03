###################
# app.R
# 
# Main controller. 
# Used to import your ui and server components; initializes the app.
###################

source('./ui.R')
source('./server.R')


shinyApp(ui, 
         server)
