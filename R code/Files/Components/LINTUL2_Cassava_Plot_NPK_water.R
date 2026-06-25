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

NPKI_nutrients_water<-function(NUlim=NA,TITLE=title,LEG=TRUE){
  
  plot( x    = NUlim$time, 
        y    = NUlim$NNI, type="l", 
        col  = "green", 
        xlab = "time (days)", 
        ylab = 'Nutrition index',
        ylim = c(-0.2,1.2), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1,
        cex.axis=0.85)
  text(x=median(NUlim$time), y=1.15,labels=TITLE,cex=1.25)
  
  lines(x    = NUlim$time, 
        y    = NUlim$PNI, 
        col  = "blue", 
        lwd  = 2,
        lty  = 2)
  lines(x    = NUlim$time, 
        y    = NUlim$KNI, 
        col  = "red", 
        lwd  = 2,
        lty  = 3)
  lines(x    = NUlim$time, 
        y    = NUlim$NPKI, 
        col  = "black", 
        lwd  = 2,
        lty  = 1)

  
  plot( x    = NUlim$time, 
        y    = 10 * NUlim$NMINT, type="l", 
        col  = "green", 
        xlab = "time (days)", 
        ylab = expression('Soil nutrients, kg ha'^-1),
        ylim = c(-5,300), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1,
        cex.axis=0.85)
  lines(x    = NUlim$time, 
        y    = 10 * NUlim$PMINT, 
        col  = "blue", 
        lwd  = 2,
        lty  = 2)
  lines(x    = NUlim$time, 
        y    = 10 * NUlim$KMINT, 
        col  = "red", 
        lwd  = 2,
        lty  = 3)
  if(LEG){
    legend("topleft", legend=c('N', 'P', 'K','NPK'), 
           col = c("green","blue","red","black"), 
           lwd = c(2,2,2,2),
           lty = c(1,2,3,1),bty="n")
  }
  plot( x    = NUlim$time, 
        y    = NUlim$WA, type="l", 
        col  = "blue", 
        xlab = "time (days)", 
        ylab = 'Soil water, mm',
        ylim = c(-0.2,1000), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1,
        cex.axis=0.85)
}


NPKI <- function(NUlim=NA,TITLE=title,LEG=TRUE){
  
  plot( x    = NUlim$time, 
        y    = NUlim$NNI, type="l", 
        col  = "green", 
        xlab = "time (days)", 
        ylab = "Nutrition index",
        ylim = c(-0.2,1.2), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1,
        cex.axis=0.85)
  text(x=400, y=1.15,labels=TITLE,cex=1.5)
  lines(x    = NUlim$time, 
        y    = NUlim$PNI, 
        col  = "blue", 
        lwd  = 2,
        lty  = 2)
  lines(x    = NUlim$time, 
        y    = NUlim$KNI, 
        col  = "red", 
        lwd  = 2,
        lty  = 3)
  lines(x    = NUlim$time, 
        y    = NUlim$NPKI, 
        col  = "black", 
        lwd  = 2,
        lty  = 1)
  if(LEG){
    leg_x <- 500   # higher is more to right
    leg_y <- 0.1  # higher is more to upper
    legend("topleft", legend=c('N', 'P', 'K','NPK'), 
           col = c("green","blue","red","black"), 
           lwd = c(2,2,2,2),
           lty = c(1,2,3,1),bty="n")
  }
}
Soil_nutrients <- function(NUlim=NA,TITLE=title,LEG=TRUE){
    
    
  plot( x    = NUlim$time, 
        y    = 10 * NUlim$NMINT, type="l", 
        col  = "green", 
        xlab = "time (days)", 
        ylab = expression('Soil nutrients, kg ha'^-1),
        ylim = c(-5,300), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1,
        cex.axis=0.85)
  
  text(x=400, y=290,labels=TITLE,cex=1.5)
  lines(x    = NUlim$time, 
        y    = 10 * NUlim$PMINT, 
        col  = "blue", 
        lwd  = 2,
        lty  = 2)
  lines(x    = NUlim$time, 
        y    = 10 * NUlim$KMINT, 
        col  = "red", 
        lwd  = 2,
        lty  = 3)
  if(LEG){
    leg_x <- 500   # higher is more to right
    leg_y <- 0.1  # higher is more to upper
    legend("topleft", legend=c('N', 'P', 'K'), 
           col = c("green","blue","red"), 
           lwd = c(2,2,2),
           lty = c(1,2,3),bty="n")
  }  
}

