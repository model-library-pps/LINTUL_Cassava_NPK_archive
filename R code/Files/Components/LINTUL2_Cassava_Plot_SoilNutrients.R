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

SoilNutrients<-function(NUlim=NA,TITLE=title,LEG=TRUE){
  
  plot( x    = NUlim$time, 
        y    = NUlim$NMINT, type="l", 
        col  = "green", 
        xlab = "time (days)", 
        ylab = 'N, g/m2',
        ylim = c(-1,40), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1,
        cex.axis=0.85)
  text(x=median(NUlim$time), y=38,labels=TITLE,cex=1.25)
  lines(x    = NUlim$time, 
        y    = NUlim$NMINS, 
        col  = "blue", 
        lwd  = 2,
        lty  = 2)
  lines(x    = NUlim$time, 
        y    = NUlim$NMINF, 
        col  = "red", 
        lwd  = 2,
        lty  = 3)

  
  plot( x    = NUlim$time, 
        y    = NUlim$PMINT, type="l", 
        col  = "green", 
        xlab = "time (days)", 
        ylab = 'P, g/m2',
        ylim = c(-1,7), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1,
        cex.axis=0.85)
  lines(x    = NUlim$time, 
        y    = NUlim$PMINS, 
        col  = "blue", 
        lwd  = 2,
        lty  = 2)
  lines(x    = NUlim$time, 
        y    = NUlim$PMINF, 
        col  = "red", 
        lwd  = 2,
        lty  = 3)
  
  plot( x    = NUlim$time, 
        y    = NUlim$KMINT, type="l", 
        col  = "green", 
        xlab = "time (days)", 
        ylab = 'K, g/m2',
        ylim = c(-1,25), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1,
        cex.axis=0.85)
  lines(x    = NUlim$time, 
        y    = NUlim$KMINS, 
        col  = "blue", 
        lwd  = 2,
        lty  = 2)
  lines(x    = NUlim$time, 
        y    = NUlim$KMINF, 
        col  = "red", 
        lwd  = 2,
        lty  = 3)
  
  if(LEG){
    legend("topleft", legend=c('Plant available', 'Soil', 'Fertilizer'), 
           col = c("green","blue","red","black"), 
           lwd = c(2,2,2,2),
           lty = c(1,2,3,1),bty="n")
  }
  
}

