#-------------------------------------------------------------------------------------------------#
# FUNCTION LeavesStemRootDynamics
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

LeavesStemRootDynamics<-function(Series1=Potential,
                                 Series2=NULL,
                                 Series3=NULL,
                                 Series4=NULL,
                                 Series5=NULL,
                                 Series6=NULL,
                                 BIOMASS=NA,
                                 BIOMASS2=NA,
                                 TITLE=title,
                                 LEG=TRUE,LEGTEXT=c('Potential', 'Water lim.', 'Nutrient lim.', "Observed1", "Observed2")){
  
  # leaves
  
  ColS1 <- "black"
  ColS2 <- "blue"
  ColS3 <- "red"
  ColS4 <- rgb(red = 0, green = 0.3, blue = 0)
  ColS5 <- rgb(red = 0, green = 0.3, blue = 1, alpha = 0.8)
  ColS6 <- rgb(red = 0, green = 0.6, blue = 1, alpha = 0.9)
  colBIOM <- "black" #'#22BB22'
  colBIOM2 <- ColS4
  
  plot( x    = Series1$time, 
        y    = Series1$WLVG, type="l", 
        col  = ColS1, 
        xlab = "time (days)", 
        ylab = expression('Green leaves, g DM m'^-2),
        ylim = c(0,500), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1.25, #1.0
        cex.axis= 1.1, #0.85
        mgp=c(2,0.5,0))
  text(x=median(Series1$time), y=490,labels=TITLE,cex=2.0)
  colLEG <- ColS1
  colLWD <- 2
  colLTY <- 1
  colPCH <- NA
  if(length(Series2)>1){
    lines(x    = Series2$time, 
        y    = Series2$WLVG, 
        col  = ColS2, 
        lwd  = 2,
        lty  = 2)
    colLEG <- c(colLEG,ColS2)
    colLWD <- c(colLWD, 2)
    colLTY <- c(colLTY, 2)
    colPCH <- c(colPCH, NA)
  }
  if(length(Series3)>1){
    lines(x    = Series3$time, 
        y    = Series3$WLVG, 
        col  = ColS3, 
        lwd  = 2,
        lty  = 3)
    colLEG <- c(colLEG,ColS3)
    colLWD <- c(colLWD, 2)
    colLTY <- c(colLTY, 3)
    colPCH <- c(colPCH, NA)
  }
  if(length(Series4)>1){
    lines(x    = Series4$time, 
          y    = Series4$WLVG, 
          col  = ColS4, 
          lwd  = 2,
          lty  = 1)
    colLEG <- c(colLEG,ColS4)
    colLWD <- c(colLWD, 2)
    colLTY <- c(colLTY, 1)
    colPCH <- c(colPCH, NA)
  }
  if(length(Series5)>1){
    lines(x    = Series5$time, 
          y    = Series5$WLVG, 
          col  = ColS5, 
          lwd  = 2,
          lty  = 2)
    colLEG <- c(colLEG,ColS5)
    colLWD <- c(colLWD, 2)
    colLTY <- c(colLTY, 2)
    colPCH <- c(colPCH, NA)
  }
  if(length(Series6)>1){
    lines(x    = Series6$time, 
          y    = Series6$WLVG, 
          col  = ColS6, 
          lwd  = 2,
          lty  = 3)
    colLEG <- c(colLEG,ColS6)
    colLWD <- c(colLWD, 2)
    colLTY <- c(colLTY, 3)
    colPCH <- c(colPCH, NA)
  }
  if(length(BIOMASS)>1){
    arrows(BIOMASS$time, BIOMASS$DMLeaves.g.DM.m2-BIOMASS$std_DMLeaves.g.DM.m2, 
           BIOMASS$time, BIOMASS$DMLeaves.g.DM.m2+BIOMASS$std_DMLeaves.g.DM.m2, 
           length=0.05, angle=90, code=3)
    points(x   = BIOMASS$time,
           y   = BIOMASS$DMLeaves.g.DM.m2,
           col = colBIOM,
           lwd = 2.5,
           pch = 19, cex=1.5)
    colLEG <- c(colLEG,colBIOM)
    colLWD <- c(colLWD, 2.5)
    colLTY <- c(colLTY, NA)
    colPCH <- c(colPCH, 19)
  }
  if(length(BIOMASS2)>1){
    arrows(BIOMASS2$time, BIOMASS2$DMLeaves.g.DM.m2-BIOMASS2$std_DMLeaves.g.DM.m2, 
           BIOMASS2$time, BIOMASS2$DMLeaves.g.DM.m2+BIOMASS2$std_DMLeaves.g.DM.m2, 
           length=0.05, angle=90, code=3)
    points(x   = BIOMASS2$time,
           y   = BIOMASS2$DMLeaves.g.DM.m2,
           col = colBIOM2,
           lwd = 2.5,
           pch = 0, cex=1.5)
    colLEG <- c(colLEG,colBIOM2)
    colLWD <- c(colLWD, 2.5)
    colLTY <- c(colLTY, NA)
    colPCH <- c(colPCH, 0)
  }
  
  ##############################################
  
  #STEMS
  
  #par(mfrow=c(1,1), oma = c(4,1,1,1), mar = c(0.6,5,0,0))
  plot( x    = Series1$time, 
        y    = Series1$WST, type="l", 
        col  = ColS1, 
        xlab = "time (days)", 
        ylab = expression('Stems, g DM m'^-2),
        ylim = c(0,2500), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1.25, #1.0
        cex.axis= 1.1, #0.85
        mgp=c(2,0.5,0))
  
  if(length(Series2)>1){
    lines(x    = Series2$time, 
          y    = Series2$WST, 
          col  = ColS2, 
          lwd  = 2,
          lty  = 2)
  }
  if(length(Series3)>1){
    lines(x    = Series3$time, 
          y    = Series3$WST, 
          col  = ColS3, 
          lwd  = 2,
          lty  = 3)
  }  
  if(length(Series4)>1){
    lines(x    = Series4$time, 
          y    = Series4$WST, 
          col  = ColS4, 
          lwd  = 2,
          lty  = 1)
  } 
  if(length(Series5)>1){
    lines(x    = Series5$time, 
          y    = Series5$WST, 
          col  = ColS5, 
          lwd  = 2,
          lty  = 2)
  } 
  if(length(Series6)>1){
    lines(x    = Series6$time, 
          y    = Series6$WST, 
          col  = ColS6, 
          lwd  = 2,
          lty  = 3)
  } 
  if(length(BIOMASS)>1){
    arrows(BIOMASS$time, BIOMASS$DMstems.g.DM.m2-BIOMASS$std_DMstems.g.DM.m2, 
           BIOMASS$time, BIOMASS$DMstems.g.DM.m2+BIOMASS$std_DMstems.g.DM.m2, 
           length=0.05, angle=90, code=3)
    points(x   = BIOMASS$time,
           y   = BIOMASS$DMstems.g.DM.m2,
           col = colBIOM,
           lwd = 2.5,
           pch = 19, cex=1.5)
  }
  if(length(BIOMASS2)>1){
    arrows(BIOMASS2$time, BIOMASS2$DMstems.g.DM.m2-BIOMASS2$std_DMstems.g.DM.m2, 
           BIOMASS2$time, BIOMASS2$DMstems.g.DM.m2+BIOMASS2$std_DMstems.g.DM.m2, 
           length=0.05, angle=90, code=3)
    points(x   = BIOMASS2$time,
           y   = BIOMASS2$DMstems.g.DM.m2,
           col = colBIOM2,
           lwd = 2.5,
           pch = 0, cex=1.5)
  }  
  if(LEG){
    leg_x <- 150   # higher is more to right
    leg_y <- 1100  # higher is more to upper
    legend("topleft", legend=LEGTEXT, col = colLEG, lwd = colLWD, lty = colLTY, 
           pch = colPCH, pt.cex=1.5, cex=1.5, bty = "n")
  }
  
  ###################################################################
  
  #Storage roots
  
  plot( x    = Series1$time, 
        y    = Series1$WSO, type="l", 
        col  = ColS1, 
        xlab = "time (days)", 
        ylab = expression('Storage roots, g DM m'^-2),
        ylim = c(0,4000), 
        lwd  = 2,
        lty  = 1,
        cex.lab  = 1.25, #1.0
        cex.axis= 1.1, #0.85
        mgp=c(2,0.5,0))
  if(length(Series2)>1){
    lines(x    = Series2$time, 
          y    = Series2$WSO, 
          col  = ColS2, 
          lwd  = 2,
          lty  = 2)
  }
  if(length(Series3)>1){
    lines(x    = Series3$time, 
          y    = Series3$WSO, 
          col  = ColS3, 
          lwd  = 2,
          lty  = 3)
  } 
  if(length(Series4)>1){
    lines(x    = Series4$time, 
          y    = Series4$WSO, 
          col  = ColS4, 
          lwd  = 2,
          lty  = 1)
  } 
  if(length(Series5)>1){
    lines(x    = Series5$time, 
          y    = Series5$WSO, 
          col  = ColS5, 
          lwd  = 2,
          lty  = 2)
  } 
  if(length(Series6)>1){
    lines(x    = Series6$time, 
          y    = Series6$WSO, 
          col  = ColS6, 
          lwd  = 2,
          lty  = 3)
  } 
  
  if(length(BIOMASS)>1){
    arrows(BIOMASS$time, BIOMASS$DMRoots.g.DM.m2 - BIOMASS$std_DMRoots.g.DM.m2, 
           BIOMASS$time, BIOMASS$DMRoots.g.DM.m2 + BIOMASS$std_DMRoots.g.DM.m2, 
           length=0.05, angle=90, code=3)
    points(x   = BIOMASS$time,
           y   = BIOMASS$DMRoots.g.DM.m2,
           col = colBIOM,
           lwd = 2.5,
           pch = 19, cex=1.5)
  }
  if(length(BIOMASS2)>1){
    arrows(BIOMASS2$time, BIOMASS2$DMRoots.g.DM.m2 - BIOMASS2$std_DMRoots.g.DM.m2, 
           BIOMASS2$time, BIOMASS2$DMRoots.g.DM.m2 + BIOMASS2$std_DMRoots.g.DM.m2, 
           length=0.05, angle=90, code=3)
    points(x   = BIOMASS2$time,
           y   = BIOMASS2$DMRoots.g.DM.m2,
           col = colBIOM2,
           lwd = 2.5,
           pch = 0, cex=1.5)
  }}
