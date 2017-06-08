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

#Setting max size for uploaded files
options(shiny.maxRequestSize=60*1024^2)

shinyServer(function(input, output) {
  
  #Data File input for "Data Upload" tab
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    inFile <- as.data.frame(read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                           quote=input$quote))
    inFile[1:15,]
  })

  #Time series plot rendering for "Data Selection" tab
  
  output$timeseries <- renderDygraph({
    
    graph <<- dygraph(ts1, main = "Time Series Data", ylab = "KW") %>%
      dyOptions(drawGrid = input$showgrid) %>%
      dyRangeSelector(dateWindow = c("2016-07-31 00:00:06", "2016-08-16 19:37:27"))  %>%
      dyShading(from = "2016-07-31 00:00:06", to = "2016-08-16 19:37:27", color = "white")
  
    graph
    
    
  })

})
