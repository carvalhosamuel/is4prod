## global.R ##
##
## Samuel Carvalho
##
## IS4PROD App Global

library(shiny)
library(ggplot2)
library(Cairo)
library(xts)
library(dygraphs)
library(zoo)

#Setting right time zone
Sys.setenv(TZ="GMT")
Sys.setenv(LANG= "en")




#CSS FOR THE LOADING GIF

mycss <-"#plot-container {
position: relative;
}
#loading-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: -1;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
z-index: -2;
}
.leftdiv{
height: 400px;
width: 50%;
float: left;
}
.rightdiv{
height: 400px;
width: 50%;
float: right;
}
.reporttext{
float: left;
position: relative;
}
"

##Shinyapps.io deploy
#library(rsconnect)
#deployApp()