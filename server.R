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
    
    if(!input$demodataset){
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
    }
    else{
      inFile <- as.data.frame(read.csv("input.csv", header=TRUE, sep=',', 
                                       quote='"'))
      
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
    }
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
    
    Ts <- abs(as.numeric(difftime(Time[2], Time[1], units = "hours"))) 
    
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

  candidates <- reactiveValues(st = NULL, 
                               nd = NULL, 
                               rd = NULL,
                               st3c = NULL)
  
  output$correloplot <- renderPlot({
    
    find_peaks <- function (x, m = input$filter){
      shape <- diff(sign(diff(x, na.pad = FALSE)))
      pks <- sapply(which(shape < 0), FUN = function(i){
        z <- i - m + 1
        z <- ifelse(z > 0, z, 1)
        w <- i + m + 1
        w <- ifelse(w < length(x), w, length(x))
        if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
      })
      pks <- unlist(pks)
      pks
    }

    from <- as.POSIXlt(req(input$timeseries_date_window[[1]]), format="%Y-%m-%dT%H:%M:%OSZ",tz ="GMT")
    
    to <- as.POSIXlt(req(input$timeseries_date_window[[2]]), format="%Y-%m-%dT%H:%M:%OSZ",tz ="GMT")
    
    Ts <- abs(as.numeric(difftime(Time[2], Time[1], units = "secs"))) 

    newdata <- subset(AvgKW, Time >= from & Time <= to)

    correlogram <<- acf(newdata, type = "correlation", lag.max = input$lagsnumber, plot = TRUE, main = "Candidate cycle times in seconds (blue labels)", ylab = "Autocorrelation Level")

    text(find_peaks(correlogram$acf), correlogram$acf[find_peaks(correlogram$acf)], as.character(find_peaks(correlogram$acf)*Ts), col = "blue")
    
    svalues <- find_peaks(correlogram$acf)*Ts
    cvalues <- correlogram$acf[find_peaks(correlogram$acf)]
    
    pairs <- as.data.frame(cbind(cvalues,svalues))
    
    pairs <- pairs[order(pairs$cvalues, decreasing = TRUE),]
    
    
    
    candidates$st <-  paste0(pairs$svalues[1])
    candidates$st3c <- paste0(pairs$svalues[1])
    candidates$nd <-  paste0(pairs$svalues[2])
    candidates$rd <-  paste0(pairs$svalues[3])
      
    
  })
 

  output$cycletime2 = renderUI({
    
    numericInput("ctime2", "Cycle time:", min = 0, max = 5000, candidates$st, step = 0.5)
   
    })
  
  output$cycletime3 = renderUI({
    
    numericInput("ctime3", "Cycle time:", min = 0, max = 5000, candidates$st3c, step = 0.5)
    
  })
  
  output$stcandidate <- renderInfoBox({
   
    infoBox("Best Candidate:", candidates$st,  icon = icon("clock-o"),color = "blue", fill = TRUE)
    
  })
  
  output$ndcandidate <- renderInfoBox({
    
    infoBox("2nd Best Candidate:", candidates$nd,  icon = icon("clock-o"),color = "blue", fill = TRUE)
    
  })
  
  output$rdcandidate <- renderInfoBox({
    
    infoBox("3rd Best Candidate:", candidates$rd,  icon = icon("clock-o"),color = "blue", fill = TRUE)
    
  })
  
  #Reactive values to store the KPIs
  kpis2c <- reactiveValues(productionT = NULL, 
                         idleT = NULL, 
                         products = NULL,
                         kwIdle = NULL,
                         kwProd = NULL
                         )
  
 
  
  output$clustering2 <- renderDygraph({
    
    #Garbage Collection: releases memory
    gc()
    
    from <- as.POSIXlt(req(input$timeseries_date_window[[1]]), format="%Y-%m-%dT%H:%M:%OSZ",tz ="GMT")
    
    to <- as.POSIXlt(req(input$timeseries_date_window[[2]]), format="%Y-%m-%dT%H:%M:%OSZ",tz ="GMT")
    
    Ts <- abs(as.numeric(difftime(Time[2], Time[1], units = "secs"))) 
    
    newdata <- subset(AvgKW, Time >= from & Time <= to)
    newdataTime <- subset(Time, Time >= from & Time <= to)
    
    x <- newdata
    
    #Sliding window (Cycle time computed as number of samples)
    CT <- input$ctime2/Ts
    
    #Extracted matrix using sliding window  
    windowsmatrix <- rollapply(x, CT, function(z) c(z))
    
    #Features construction using the fastICA algorithm
    library(fastICA)
    features <- fastICA(windowsmatrix, input$features, fun = "exp", method = "C", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE)
    
    #IF statement to choose between PCA or ICA, depending on input from dashboard
    if(input$components == "pca"){
      #PCA Components
      features <- as.data.frame(features$X %*% features$K) 
    }
    else{    
      #ICA Componets
      features <- as.data.frame(features$S)   
    }
    
    ### CLUSTERING STARTS
    set.seed(42)
    
    #The 'reshape' package provides the 'rescaler' function.
    library(reshape, quietly=TRUE)
    
    #Generate a kmeans cluster of size 2
    ckmeans <- kmeans(sapply(na.omit(features[,  names(features)]), rescaler, "range"), 2)
   
    ### END OF CLUSTERING
    
    #Aligning the cluster indexes array with the data array
    if((CT-1)%%2==1){
      Xnose <- floor((CT-1)/2)
      Xtail <- Xnose + 1 
    }else{
      Xnose <- (CT-1)/2
      Xtail <- Xnose
    }
    
    firstcluster <- ckmeans$cluster[1]
    lastcluster <- last(ckmeans$cluster)
    nosechunk <- rep(firstcluster, Xnose)
    tailchunk <- rep(lastcluster, Xtail)
    clusters_index <- as.matrix(c(nosechunk,ckmeans$cluster,tailchunk))
    
    #Resulting matrix of each data point assigned to one cluster
    vec <- as.matrix(cbind(newdata,seq(1,length(newdata),by = 1),clusters_index))
    
    #Subsetting clusters 1 and 2 and taking the average to tell production/idle apart
    cluster1 <- subset(vec, vec[,3] == 1)
    avg1 <- mean(cluster1[,1])
    total1 <- sum(Ts*rollmean(cluster1[,1],2))
    
    cluster2 <- subset(vec, vec[,3] == 2)
    avg2 <- mean(cluster2[,1])
    total2 <- sum(Ts*rollmean(cluster2[,1],2))
    
    #If statement to make sure production is always plotted in blue by comparing avg1 and avg2
    
    if(avg1>avg2){
      
      
      tsprod <- xts(newdata[cluster1[,2]],newdataTime[cluster1[,2]], tzone = "GMT")
      tsidle <- xts(newdata[cluster2[,2]],newdataTime[cluster2[,2]], tzone = "GMT")
      
      kpis2c$products <- floor(nrow(cluster1)/CT)
      kpis2c$productionT <- ceiling(nrow(cluster1)*Ts)
      kpis2c$idleT <- ceiling(nrow(cluster2)*Ts)
      kpis2c$kwProd <- total1
      kpis2c$kwIdle <- total2
      
    }
    else{
      
      tsprod <- xts(newdata[cluster2[,2]],newdataTime[cluster2[,2]], tzone = "GMT")
      tsidle <- xts(newdata[cluster1[,2]],newdataTime[cluster1[,2]], tzone = "GMT")
      
      kpis2c$products <- floor(nrow(cluster2)/CT)
      kpis2c$productionT <- ceiling(nrow(cluster2)*Ts)
      kpis2c$idleT <- ceiling(nrow(cluster1)*Ts)
      kpis2c$kwProd <- total2
      kpis2c$kwIdle <- total1
      
    }
    
    
    plotclusters <- cbind(tsprod, tsidle)
    
    
    graphCluster <- dygraph(plotclusters, main = "Clustering Output", ylab = "KW") %>%
      dySeries("..1", label = "Production") %>%
      dySeries("..2", label = "Idle") %>%
      dyOptions(drawGrid = TRUE) %>%
      dyRangeSelector()  %>%
      dyOptions(colors = c("darkgreen","orangered")) %>%
      dyLegend(width = 300) 
      #dyShading(from = fromtime, to = totime, color = "white")
    
    #Outputs Cluster
    graphCluster
    
  })
  
  output$productsBox2c <- renderInfoBox({
    
    infoBox("Produced parts", kpis2c$products, icon = icon("cubes"),color = "blue", fill = TRUE)
    
  })
  
  output$pTimeBox2c <- renderInfoBox({
    
    pTime <- round(kpis2c$productionT/3600, 2)
   
    
    infoBox("Production time", pTime, "hours",icon = icon("industry"),color = "green", fill = TRUE)
    
  })
  
  output$iTimeBox2c <- renderInfoBox({
    
    iTime <- round(kpis2c$idleT/3600, 2)
    
    infoBox("Idle time", iTime, "hours", icon = icon("spinner"),color = "orange", fill = TRUE)
    
  })
  
  
  #Reactive values to store the KPIs
  kpis3c <- reactiveValues(productionT = NULL, 
                           idleT = NULL, 
                           scrapT = NULL,
                           products = NULL,
                           kwIdle = NULL,
                           kwProd = NULL,
                           kwScrap = NULL
                           )
  
  output$clustering3 <- renderDygraph({
    
    #Garbage Collection: releases memory
    gc()
    
    from <- as.POSIXlt(req(input$timeseries_date_window[[1]]), format="%Y-%m-%dT%H:%M:%OSZ",tz ="GMT")
    
    to <- as.POSIXlt(req(input$timeseries_date_window[[2]]), format="%Y-%m-%dT%H:%M:%OSZ",tz ="GMT")
    
    Ts <- abs(as.numeric(difftime(Time[2], Time[1], units = "secs"))) 
    
    newdata <- subset(AvgKW, Time >= from & Time <= to)
    newdataTime <- subset(Time, Time >= from & Time <= to)
    
    x <- newdata
    
    #Sliding window (Cycle time computed as number of samples)
    CT <- input$ctime3/Ts
    
    #Extracted matrix using sliding window  
    windowsmatrix <- rollapply(x, CT, function(z) c(z))
    
    #Features construction using the fastICA algorithm
    library(fastICA)
    features <- fastICA(windowsmatrix, input$features3c, fun = "exp", method = "C", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE)
    
    #IF statement to choose between PCA or ICA, depending on input from dashboard
    if(input$components3c == "pca"){
      #PCA Components
      features <- as.data.frame(features$X %*% features$K) 
    }
    else{    
      #ICA Componets
      features <- as.data.frame(features$S)   
    }
    
    ### CLUSTERING STARTS
    set.seed(42)
    
    #The 'reshape' package provides the 'rescaler' function.
    library(reshape, quietly=TRUE)
    
    #Generate a kmeans cluster of size 2
    ckmeans <- kmeans(sapply(na.omit(features[,  names(features)]), rescaler, "range"), 3)
    
    ### END OF CLUSTERING
    
    #Aligning the cluster indexes array with the data array
    if((CT-1)%%2==1){
      Xnose <- floor((CT-1)/2)
      Xtail <- Xnose + 1 
    }else{
      Xnose <- (CT-1)/2
      Xtail <- Xnose
    }
    
    firstcluster <- ckmeans$cluster[1]
    lastcluster <- last(ckmeans$cluster)
    nosechunk <- rep(firstcluster, Xnose)
    tailchunk <- rep(lastcluster, Xtail)
    clusters_index <- as.matrix(c(nosechunk,ckmeans$cluster,tailchunk))
    
    #Resulting matrix of each data point assigned to one cluster
    vec <- as.matrix(cbind(newdata,seq(1,length(newdata),by = 1),clusters_index))
    
    #Subsetting clusters 1,2 and 3 and taking the average to tell production/idle/scrap apart
    cluster1 <- subset(vec, vec[,3] == 1)
    avg1 <- mean(cluster1[,1])
    
    cluster2 <- subset(vec, vec[,3] == 2)
    avg2 <- mean(cluster2[,1])
    
    cluster3 <- subset(vec, vec[,3] == 3)
    avg3 <- mean(cluster3[,1])
    
    avgs <- cbind(c(avg1,avg2,avg3),c(1:3))
    avgs <<- avgs[order(avgs[,1], decreasing = FALSE),]
    
    clusterIdle <- subset(vec, vec[,3] == avgs[1,2])
    clusterScrap <- subset(vec, vec[,3] == avgs[2,2])
    clusterProd <- subset(vec, vec[,3] == avgs[3,2])
    
    totalI <- sum(Ts*rollmean(clusterIdle[,1],2))
    totalS <- sum(Ts*rollmean(clusterScrap[,1],2))
    totalP <- sum(Ts*rollmean(clusterProd[,1],2))
      
    tsprod <- xts(newdata[clusterProd[,2]],newdataTime[clusterProd[,2]], tzone = "GMT")
    tsidle <- xts(newdata[clusterIdle[,2]],newdataTime[clusterIdle[,2]], tzone = "GMT")
    tsscrap <- xts(newdata[clusterScrap[,2]],newdataTime[clusterScrap[,2]], tzone = "GMT")
      
    kpis3c$products <- floor(nrow(clusterProd)/CT)
    kpis3c$productionT <- ceiling(nrow(clusterProd)*Ts)
    kpis3c$idleT <- ceiling(nrow(clusterIdle)*Ts)
    kpis3c$scrapT <- ceiling(nrow(clusterScrap)*Ts)
    kpis3c$kwProd <- totalP
    kpis3c$kwIdle <- totalI
    kpis3c$kwScrap <- totalS
      
    plotclusters <- cbind(tsprod, tsidle, tsscrap)
    
    graphCluster3 <- dygraph(plotclusters, main = "Clustering Output", ylab = "KW") %>%
      dySeries("..1", label = "Production") %>%
      dySeries("..2", label = "Idle") %>%
      dySeries("..3", label = "Scrap") %>%
      dyOptions(drawGrid = TRUE) %>%
      dyOptions(colors = c("darkgreen","navy","orangered")) %>%
      dyRangeSelector()  %>%
      dyLegend(width = 300) 
    #dyShading(from = fromtime, to = totime, color = "white")
    
    #Outputs Cluster
    graphCluster3
    
  })

  output$productsBox3c <- renderInfoBox({
    
    infoBox("Produced parts", kpis3c$products, icon = icon("cubes"),color = "blue", fill = TRUE)
    
  })
  
  output$pTimeBox3c <- renderInfoBox({
    
    pTime <- round(kpis3c$productionT/3600, 2)
    
    
    infoBox("Production time", pTime, "hours",icon = icon("industry"),color = "green", fill = TRUE)
    
  })
  
  output$iTimeBox3c <- renderInfoBox({
    
    iTime <- round(kpis3c$idleT/3600, 2)
    
    infoBox("Idle time", iTime, "hours", icon = icon("spinner"),color = "orange", fill = TRUE)
    
  })
  
})
