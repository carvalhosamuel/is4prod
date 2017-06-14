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
library(fastICA, quietly=TRUE)
library(reshape, quietly=TRUE)
library(rmarkdown)

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

position: absolute;
left: 0px;
width: 700px;
border: 3px solid #73AD21;
padding: 10px;

}

.rightdiv{

position: absolute;
right: 0px;
width: 300px;
border: 3px solid #73AD21;
padding: 10px;

}
.reporttext{
float: left;
position: relative;
}
"

##Shinyapps.io deploy
#library(rsconnect)
#deployApp()