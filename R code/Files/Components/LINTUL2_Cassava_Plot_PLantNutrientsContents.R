#-------------------------------------------------------------------------------------------------#
# FUNCTION PlantNutrients
#
# Author:       AGT Schut
# Copyright:    Copyright 2019, PPS
# Email:        tom.schut@wur.nl
# Date:         18-12-2019
#
# This file contains a function to plot output of the LINTUL-CASSAVA_NPK model. 
# The purpose of this function is to plot biomass components as function of time.
# In BIOMASS, measured values for WLVDM, WSTDM, WSODM and WLVDM_std,WSTDM_std,WSODM_std can be given
#--------------------------------------------------------------------------------------------------#

#require(gridExtra)
#require(ggplot2)
#library(grid)
#require(cowplot)
#require(ggpubr)

PlantNutrientContents<-function(NUlim=NA,TITLE=title,LEG=TRUE){
  
  plot( x    = NUlim$time, 
        y    = NUlim$ANLVG, type="l", 
        col  = "green", 
        xlab = "time (days)", 
        ylab = expression('N, g m'-2),
        ylim = c(-1,20), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1,
        cex.axis=0.85)
  text(x=400, y=18.5,labels=TITLE,cex=1.5)
  lines(x    = NUlim$time, 
        y    = NUlim$ANST, 
        col  = "blue", 
        lwd  = 2,
        lty  = 2)
  lines(x    = NUlim$time, 
        y    = NUlim$ANSO, 
        col  = "red", 
        lwd  = 2,
        lty  = 3)
  lines(x    = NUlim$time, 
        y    = NUlim$ANLVD, 
        col  = "black", 
        lwd  = 2,
        lty  = 3)
  
  
  plot( x    = NUlim$time, 
        y    = NUlim$APLVG, type="l", 
        col  = "green", 
        xlab = "time (days)", 
        ylab = expression('P, g m'-2),
        ylim = c(-0.1,1.5), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1,
        cex.axis=0.85)
  text(x=400, y=1.45,labels=TITLE,cex=1.5)
  lines(x    = NUlim$time, 
        y    = NUlim$APST, 
        col  = "blue", 
        lwd  = 2,
        lty  = 2)
  lines(x    = NUlim$time, 
        y    = NUlim$APSO, 
        col  = "red", 
        lwd  = 2,
        lty  = 3)
  lines(x    = NUlim$time, 
        y    = NUlim$APLVD, 
        col  = "black", 
        lwd  = 2,
        lty  = 3)
  
  plot( x    = NUlim$time, 
        y    = NUlim$AKLVG, type="l", 
        col  = "green", 
        xlab = "time (days)", 
        ylab = expression('K, g m'-2),
        ylim = c(-1,20), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1,
        cex.axis=0.85)
  text(x=400, y=19.5,labels=TITLE,cex=1.5)
  lines(x    = NUlim$time, 
        y    = NUlim$AKST, 
        col  = "blue", 
        lwd  = 2,
        lty  = 2)
  
  lines(x    = NUlim$time, 
        y    = NUlim$AKSO, 
        col  = "red", 
        lwd  = 2,
        lty  = 3)
  lines(x    = NUlim$time, 
        y    = NUlim$AKLVD, 
        col  = "black", 
        lwd  = 2,
        lty  = 3)
  if(LEG){
    legend("topleft", legend=c('LVG', 'ST', 'SO','LVD'), 
           col = c("green","blue","red","black"), 
           lwd = c(2,2,2,2),
           lty = c(1,2,3,3),bty="n")
  }
  
}

