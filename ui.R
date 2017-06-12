## ui.R ##
##
## Samuel Carvalho
##
## IS4PROD App UI

#Loading Shiny packages
library(shinydashboard)
library(shiny)

#Setting right time zone
Sys.setenv(TZ='GMT')
Sys.setenv(LANG= "en")

dashboardPage(
  
  #Black and white skin colors
  skin = "black",
  #Title that comes on the browser tab
  title = "IS4PROD App",
  
  ## Header content
  #Image on top of sidebar
  dashboardHeader(title = tags$a(href='http://www.is4prod.ie',tags$img(src='logo.png',height='45',width='200'))),
 
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Upload", tabName = "dataupload", icon = icon("upload")),
      menuItem("Data Selection", tabName = "selection", icon = icon("line-chart")),
      menuItem("Energy Histogram", tabName = "energyhistogram", icon = icon("bar-chart")),
      menuItem("Cycle Time Analysis", tabName = "cycletime", icon = icon("clock-o")),
      menuItem("Clustering Analysis",  icon = icon("cogs"),
               menuSubItem("Production/Idle",tabName = "clustering2"),
               menuSubItem("Production/Scrap/Idle",tabName = "clustering3")
               ),
      menuItem("Report", tabName = "report", icon = icon("pie-chart")),
      br(),
      br(),
      menuItem("Info", tabName ="info", icon = icon("info-circle"))

    
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      
      # HOME tab content
      tabItem(tabName = "home",
              fluidRow(
                
                # Welcome box
                box(
                    title = "IS4PROD App",
                    status = "info",
                    solidHeader = TRUE,
                    width = 12,
                    #img(
                    #  src = "logo.png"
                    #),
                    #h2("IS4PROD App"),
                    h2("Dynamic Web-based Energy Profiles Analytics"),
                    br(),
                    h4(
                      "The IS4PROD App is a ",
                      a(href = 'http://shiny.rstudio.com', 'Shiny'),
                      "web application built on top of R for machine level energy-related data analytics, 
                      powered by several R packages (more on info tab). The app utilises Machine Learning 
                      techniques to extract useful insights from energy datasets."),
                    br(),
                    h4("To get started, upload a CSV file in the 'Data Upload' panel, 
                       then follow down the steps on the sidebar. If you prefer it, use the provided 
                       demo dataset to familiarise with the app and its features."),
                    br(),
                    h4("Further instructions are available inside each tab panel."),
                    br(),
                    h4(
                      HTML('&copy'),
                      '2017 By Samuel Carvalho. '
                     
                    )
                )
                
              )
      ),
      
      # Data Upload tab content
      tabItem(tabName = "dataupload",
              fluidRow(
                
                # Instructions box
                box(
                  title = "Instructions for upload",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 5,
                  p("This app expects you to upload a .CSV file with timestamped three-phase RMS current values obtained from your machine.
                    As this app follows the ",a(href = 'https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html', 'Tidy Data'), " guidelines, the input spreadsheet must have its first line as columns names, each column as one variable, and each row as one observation of that variable at the related timestamp."),
                 
                  p("The name of each column doesn't matter, as long as the order of the columns are the following: Timestamp, Phase A Currents, Phase B Currents, Phase C Currents."),
                
                  p("There is no need to input current values measured at the Neutral wirings."),
               
                  p("The timestamps must all be in the 'YYYY-mm-dd HH:MM:SS' format. A simple period must be used as the decimal point separator for the RMS current values."),
                  br(),
                  p("The following image shows an example of a valid input spreadsheet:"),
                  img(
                    src = "inputexample.png"
                  )
                ),
                
                #Upload Box
                box(
                  title = "Upload your file here",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 3,
                  fileInput('file1', 'Choose CSV File (max 60mb):',
                            accept=c('text/csv', 
                                     'text/comma-separated-values,text/plain', 
                                     '.csv')),
                  checkboxInput('demodataset',"I won't upload a file. Use the demo dataset instead.",FALSE),
                  tags$hr(),
                  h4("Quick Formatting Options:"),
                  numericInput('voltage','Phase-to-phase Voltage', 400, min = 0, max = 1000000, step = 1),
                  checkboxInput('header', 'Header', TRUE),
                  radioButtons('sep', 'Separator',
                               c(Comma=',',
                                 Semicolon=';',
                                 Tab='\t'),
                               ','),
                  radioButtons('quote', 'Quote',
                               c(None='',
                                 'Double Quote'='"',
                                 'Single Quote'="'"),
                               '"')
                ),  
             
                
                #Data Table Visualisation Box
                box(
                  title = "Data check: you can see here the first 15 rows of your data",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 4,
                  tableOutput('contents')
                )  
              )
      ),
      
      # Data Selection tab content
      tabItem(tabName = "selection",
             
              fluidRow(
                #Info Box
                box(
                  title = "Additional help/information:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  width = 12,
                  br(),
                  div("This is your energy data, indexed by time. Use the slider below it or 
                      click and drag over the graph to select a time window you want to analyse.
                      The values of average KW and total energy spending will be automatically 
                      updated on the information boxes below the graph. 
                      All subsequent analyses (Histogram, Cycle Times, Clustering, etc) on the next tabs will 
                      be based on your selection from this tab."),
                  br()


                ),
                
                #Graph box
                box(
                  title = "Select your data from the graph below:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  dygraphOutput("timeseries"),
                  hr(),
                  h3("Quick Insights:"),
                  # Dynamic infoBoxes
                  infoBoxOutput("fromtoBox"),
                  infoBoxOutput("avgBox"),
                  infoBoxOutput("totalBox")
                  
               
                )    
               
               
              )  
      ),
      
      # Energy Histogram tab content
      tabItem(tabName = "energyhistogram",
              fluidRow(
                
                #Info Box
                box(
                  title = "Additional help/information:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  width = 12,
                  br(),
                  div("This is the KW histogram from your selection. The bars show the occurence (in a density scale, 
                      from 0 to 1) of the values of KW given by the X axis. The line is a smoothed curve of the same information. 
                      A desirable energy histogram would have higher peaks around the typical production KW values, and no peaks 
                      close to zero, meaning that the process stays more often on production than in idle or stand-by status.
                      Use this histogram to establish a reasonable treshold between idle and production energy values."),
                  br()
                  
                  
                  ),
                
                
                #Graph box
                box(
                  title = "Energy histogram of your data:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 10,
                  plotOutput("histo", dblclick = "plot2_dblclick", brush = brushOpts(id = "plot2_brush",resetOnNew = TRUE)), 
                  br()
                ),
                
                #Graph Options
                box(
                  title = "Options:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 2,
                  sliderInput("binwidth", "Binwidth:", min = 0.05, max = 2, 0.5, step = 0.05),
                  br(),
                  p("Zoom in and out by selecting an area and double clicking it." )
                )
                
            )    
                
              
      ),
      
      # Cycle Time tab content
      tabItem(tabName = "cycletime",
              fluidRow(
                
                #Info Box
                box(
                  title = "Additional help/information:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  width = 12,
                  br(),
                  div("This graph is called the Autocorrelogram of the dataset. It is done by calculating the correlation between the dataset and 
                      lagged versions of itself. The x axis brings the number of lags in each comparison, and the y axis indicates the value of this
                      correlation, always from 0 to 1. At x=0 lags, the correlation is exactly 1, as we are comparing the dataset with itself shifted
                      by 0 lags. The blue dotted line indicates the statistical signifance treshold, so any peak above these lines is a good candidate 
                      for being the actual cycle time of this dataset. Higher peaks indicate higher correlation, and therefore more probability of being
                      the real value."),
                  br()
                  
                  
                ),
                
                
                #Graph box
                box(
                  title = "Cycle time identification:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 10,
                  plotOutput("correloplot"), 
                  br(),
                  # Dynamic infoBoxes
                  infoBoxOutput("stcandidate"),
                  infoBoxOutput("ndcandidate"),
                  infoBoxOutput("rdcandidate")
                ),
                
                #Graph Options
                box(
                  title = "Options:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 2,
                  sliderInput("lagsnumber", "Number of lags:", min = 2, max = 5000, 200, step = 10),
                  br(),
                  sliderInput("filter", "Filter selectivity:", min = 1, max = 20, 5, step = 1),
                  br(),
                  p("Move both sliders to find the best candidate cycle time." )
                )
                
              )    
      ),
      
      # Clustering 2 tab content
      tabItem(tabName = "clustering2",
              fluidRow(
                
                #Info Box
                box(
                  title = "Additional help/information:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  width = 12,
                  br(),
                  div("This graph is the outcome of a clustering algorithm called KMeans. The algorithm will automatically categorise your data into
                      two different categories, expected to be production and idle. The green points indicate production, and the blue ones indicate idle.
                      A cycle time must be specified. By the default, the best candidate from the previous tab will be used. There are also controls to choose
                      between PCA (Principal Component Analysis) and ICA (Independent Component Analysis) features, besides the desired number of features."
                  )    
                  
                  
                ),
                
                
                #Graph box
                box(
                  title = "Clustering:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 10,
                  dygraphOutput("clustering2"),
                  hr(),
                  h3("Quick Insights:"),
                  # Dynamic infoBoxes
                  infoBoxOutput("productsBox2c"),
                  infoBoxOutput("pTimeBox2c"),
                  infoBoxOutput("iTimeBox2c")
                  
                  
                ),
                
                
                #Graph Options
                box(
                  title = "Options:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 2,
                  uiOutput("cycletime2"),
                  radioButtons('components', 'Features extraction:', c('PCA Components'='pca', 'ICA Components'='ica')),
                  sliderInput('features', 'Number of features:', min = 2, max = 20, 5)
                                 
                                
                               
                )
               
              )    
      ),
      
      # Clustering 3 tab content
      tabItem(tabName = "clustering3",
              fluidRow(

                #Info Box
                box(
                  title = "Additional help/information:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  collapsed = TRUE,
                  width = 12,
                  br(),
                  div("This graph is the outcome of a clustering algorithm called KMeans. The algorithm will automatically categorise your data into
                      two different categories, expected to be production and idle. The green points indicate production, and the blue ones indicate idle.
                      A cycle time must be specified. By the default, the best candidate from the previous tab will be used. There are also controls to choose
                      between PCA (Principal Component Analysis) and ICA (Independent Component Analysis) features, besides the desired number of features."
                  )


                  ),


                #Graph box
                box(
                  title = "Clustering:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 10,
                  dygraphOutput("clustering3"),
                  hr(),
                  h3("Quick Insights:"),
                  # Dynamic infoBoxes
                  infoBoxOutput("productsBox3c"),
                  infoBoxOutput("pTimeBox3c"),
                  infoBoxOutput("iTimeBox3c")


                ),


                #Graph Options
                box(
                  title = "Options:",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 2,
                  uiOutput("cycletime3"),
                  radioButtons('components3c', 'Features extraction:', c('PCA Components'='pca', 'ICA Components'='ica')),
                  sliderInput('features3c', 'Number of features:', min = 2, max = 20, 5)



                )

            )
      ),
      
      # Report tab content
      tabItem(tabName = "report",
              h2("Report tab content")
      ),
      
      # Info tab content
      tabItem(tabName = "info",
              h2("Info tab content")
      )
    )
  )

)