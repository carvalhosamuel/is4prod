## server.R ##
##
## Samuel Carvalho
##
## IS4PROD App Server

#Loading Shiny packages
library(shiny)

#Setting right time zone
Sys.setenv(TZ='GMT')
Sys.setenv(LANG= "en")

shinyServer(function(input, output) {

  #Time series plot rendering for "Data Selection" tab
  
  output$timeseries <- renderDygraph({
    
    graph <<- dygraph(ts1, main = "Time Series Data", ylab = "KW") %>%
      dyOptions(drawGrid = input$showgrid) %>%
      dyRangeSelector(dateWindow = c("2016-07-31 00:00:06", "2016-08-16 19:37:27"))  %>%
      dyShading(from = "2016-07-31 00:00:06", to = "2016-08-16 19:37:27", color = "white")
  
    graph
    
    
  })

})
