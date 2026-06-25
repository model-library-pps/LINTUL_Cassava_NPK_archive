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

PlantNutrients<-function(NUlim=NA,TITLE=title,LEG=TRUE){
  
  plot( x    = NUlim$time, 
        y    = NUlim$NcLV, type="l", 
        col  = "green", 
        xlab = "time (days)", 
        ylab = 'N, %(DM)',
        ylim = c(-1,6), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1,
        cex.axis=0.85)
  text(x=400, y=5.5,labels=TITLE,cex=1.5)
  lines(x    = NUlim$time, 
        y    = NUlim$NcST, 
        col  = "blue", 
        lwd  = 2,
        lty  = 2)
  lines(x    = NUlim$time, 
        y    = NUlim$NcSO, 
        col  = "red", 
        lwd  = 2,
        lty  = 3)
  lines(x    = NUlim$time, 
        y    = NUlim$Nc, 
        col  = "black", 
        lwd  = 2,
        lty  = 3)
  
  
  plot( x    = NUlim$time, 
        y    = NUlim$PcLV, type="l", 
        col  = "green", 
        xlab = "time (days)", 
        ylab = 'P, %(DM)',
        ylim = c(-0.1,0.6), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1,
        cex.axis=0.85)
  text(x=400, y=0.55,labels=TITLE,cex=1.5)
  lines(x    = NUlim$time, 
        y    = NUlim$PcST, 
        col  = "blue", 
        lwd  = 2,
        lty  = 2)
  lines(x    = NUlim$time, 
        y    = NUlim$PcSO, 
        col  = "red", 
        lwd  = 2,
        lty  = 3)
  lines(x    = NUlim$time, 
        y    = NUlim$Pc, 
        col  = "black", 
        lwd  = 2,
        lty  = 3)
  
  plot( x    = NUlim$time, 
        y    = NUlim$KcLV, type="l", 
        col  = "green", 
        xlab = "time (days)", 
        ylab = 'K, %(DM)',
        ylim = c(-1,5), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1,
        cex.axis=0.85)
  text(x=400, y=4.5,labels=TITLE,cex=1.5)
  lines(x    = NUlim$time, 
        y    = NUlim$KcST, 
        col  = "blue", 
        lwd  = 2,
        lty  = 2)
  
  lines(x    = NUlim$time, 
        y    = NUlim$KcSO, 
        col  = "red", 
        lwd  = 2,
        lty  = 3)
  lines(x    = NUlim$time, 
        y    = NUlim$Kc, 
        col  = "black", 
        lwd  = 2,
        lty  = 3)
  if(LEG){
    legend("topright", legend=c('LVG', 'ST', 'SO','BIOM'), 
           col = c("green","blue","red","black"), 
           lwd = c(2,2,2,2),
           lty = c(1,2,3,1),bty="n")
  }
  
}

