## ui.R ##
##
## Samuel Carvalho
##
## IS4PROD App UI

library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(),
  dashboardBody(
    
    fluidRow(
      
        box(plotOutput("plot1", height = 250)),
        
        box(
          title = "Controls",
          sliderInput("slider", "Number of observations:", 1, 100, 50)
        )
      
    )
  )
)