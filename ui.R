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
      menuItem("Clustering Analysis", tabName = "clustering", icon = icon("cogs")),
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
                      "web application built on top of R for machine level energy-related data analytics, powered by several R packages (more on info tab)."),
                    
                    h4("The app utilises Machine Learning techniques to extract useful insights from datasets."),
                    br(),
                    h4("To get started, upload a CSV file on the 'Data Upload' panel at the sidebar, then follow down the steps on the sidebar."),
                    br(),
                    h4("Further instructions are available inside each tab panel."),
                    br(),
                    h4(
                      HTML('&copy'),
                      '2017 By Samuel Carvalho. ',
                      a(href = 'http://www.apache.org/licenses/LICENSE-2.0', 'Terms of Use.')
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
                  width = 6,
                  p("This app expects you to upload a .CSV file with timestamped three-phase RMS current values obtained from your machine.
                    As this app follows the 'Tidy Data' guidelines, the input spreadsheet must have its first line as columns names, each column as one variable, and each row as one observation of that variable at the related timestamp."),
                 
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
                  width = 6
                )  
              ),
              
              fluidRow(
                
                #Data Table Visualisation Box
                box(
                  title = "Data Table Visualisation",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12
                )  
              )
      ),
      
      # Data Selection tab content
      tabItem(tabName = "selection",
              fluidRow(
                box(
                  title = "Select your data from the graph below:",
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  br(),
                  div("This is your energy data, indexed by time. Use the slider below it or click and drag over the graph to select a time window.
                      The values of average KW and total energy spending will be automatically updated on the left panel. 
                      The further analyses (Histogram, Cycle Times, etc) on the next tabs will also be based on this selection."),
                  br(),
                 
                  dygraphOutput("timeseries")
                  
                      
                )
              )  
      ),
      
      # Energy Histogram tab content
      tabItem(tabName = "energyhistogram",
              h2("Energy Histogram tab content")
      ),
      
      # Cycle Time tab content
      tabItem(tabName = "cycletime",
              h2("Cycle time tab content")
      ),
      
      # Clustering tab content
      tabItem(tabName = "clustering",
              h2("Clustering tab content")
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