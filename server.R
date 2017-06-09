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
    
    #Transforming input data into AvgKW and Time series
    inputdata <- inFile
    
    names(inputdata) <- c("Time","AIRMS","BIRMS","CIRMS")
  
    inputdata$Time <- strptime(as.character(inputdata$Time), format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
    
    attach(inputdata)
    
    #Gets both limits of timeseries in order to plot it
    begin1 <- inputdata$Time[1]
    end1 <- inputdata$Time[length(inputdata$Time)]
    
    #IF loop to flip it, in case times are decrescent in input csv file
    if((end1 - begin1) < 0){
      end1 <- inputdata$Time[1]
      begin1 <- inputdata$Time[length(inputdata$Time)]
    }
    
    V <- input$voltage
    
    AvgKW <<- V*sqrt(3)*(rowMeans(inputdata[,2:4]))/1000
    
    tseries <<- xts(AvgKW, inputdata$Time, tzone = "GMT")
    
    endt <<- end1
    begint <<- begin1
    
    inFile[1:15,]
  })

  #Time series plot rendering for "Data Selection" tab
  
  output$timeseries <- renderDygraph({
    
    graph <<- dygraph(tseries, main = "Time Series Data", ylab = "KW") %>%
      dyOptions(drawGrid = TRUE) %>%
      dyRangeSelector(dateWindow = c(as.character(begint), as.character(endt)))  %>%
      dyShading(from = as.character(begint), to = as.character(endt), color = "white") %>%
      dyOptions(retainDateWindow = TRUE)
    
    graph
  
  })
  
  output$fromtoBox <- renderInfoBox({
    
    from <- as.POSIXlt(req(input$timeseries_date_window[[1]]), format="%Y-%m-%dT%H:%M:%OSZ",tz ="GMT")
    
    to <- as.POSIXlt(req(input$timeseries_date_window[[2]]), format="%Y-%m-%dT%H:%M:%OSZ",tz ="GMT")
    
    infoBox("Selection from/to:", paste(from,to, sep = ' to '), icon = icon("area-chart"), fill = TRUE)
    
    
  })
  
  output$avgBox <- renderInfoBox({
    
    from <<- as.POSIXlt(req(input$timeseries_date_window[[1]]), format="%Y-%m-%dT%H:%M:%OSZ", tz ="GMT")
    
    to <<- as.POSIXlt(req(input$timeseries_date_window[[2]]), format="%Y-%m-%dT%H:%M:%OSZ", tz ="GMT")
    
    newdata <<- subset(AvgKW, Time >= from & Time <= to)
    
    meanv <- mean(newdata)
    
    selecmedian <- median(newdata)
    
    infoBox("Average KW", paste0(format(round(meanv, 2), nsmall = 2), "KW"), icon = icon("flash"),color = "blue", fill = TRUE)
    
  })

  output$totalBox <- renderInfoBox({
    
    from <- as.POSIXlt(req(input$timeseries_date_window[[1]]), format="%Y-%m-%dT%H:%M:%OSZ",tz ="GMT")
    
    to <- as.POSIXlt(req(input$timeseries_date_window[[2]]), format="%Y-%m-%dT%H:%M:%OSZ",tz ="GMT")
    
    newdata <- subset(AvgKW, Time >= from & Time <= to)
    
    dif <<- abs(as.numeric(difftime(to, from, units = "hours")))
    
    Ts <<- abs(as.numeric(difftime(Time[2], Time[1], units = "hours"))) 
    
    total <- sum(Ts*rollmean(newdata,2))
    
    infoBox("Total KWh", paste0(format(round(total, 2), nsmall = 2), " KWh "), icon = icon("plug"),color = "green", fill = TRUE)
  })
  
  output$histo <- renderPlot({
  
    from <- as.POSIXlt(req(input$timeseries_date_window[[1]]), format="%Y-%m-%dT%H:%M:%OSZ",tz ="GMT")
    
    to <- as.POSIXlt(req(input$timeseries_date_window[[2]]), format="%Y-%m-%dT%H:%M:%OSZ",tz ="GMT")
    
    newdata <- subset(AvgKW, Time >= from & Time <= to)
    
    df <- data.frame(newdata)
    
    ggplot(data=df, aes(df)) + 
      geom_histogram(colour = "darkgreen", fill = "white", binwidth = input$binwidth, aes(y = ..density..)) +
      geom_density()+
      coord_cartesian(xlim = ranges$x2, ylim = ranges$y2)+
      labs(x = "KW" , y="Density")
    
  })
  
  ############## HISTOGRAM PLOT ZOOM HANDLING
  ranges <- reactiveValues(x2 = NULL, y2 = NULL)
  
  observeEvent(input$plot2_dblclick, {
    
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges$x2 <- c(brush$xmin, brush$xmax)
      ranges$y2 <- c(brush$ymin, brush$ymax)
    } 
    else {
      ranges$x2 <- NULL
      ranges$y2 <- NULL
    }
  })
 
  
 

})
