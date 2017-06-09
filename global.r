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


find_peaks <- function (x, m = 5){
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