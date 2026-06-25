#-------------------------------------------------------------------------------------------------#
# LINTUL2-CASSAVA_VALIDATION script
#
# Author:       AGT Schut
# Copyright:    Copyright 2020, PPS
# Email:        tom.schut@wur.nl
# Date:         06-02-2020
#
# This file is used to run the LINTUL2_CASSAVA_NPK model and all its related components. Changes to 
# parameters can be made in the LINTLUL2_CASSAVA_NPK_input_parameters file. 
#
#--------------------------------------------------------------------------------------------------#
# BEFORE START
# It is important before running this script to set the working directory to source file location: 
# This is done as follows: Go to 'Session' -> 'Set Working Directory' -> To Source File Location
#--------------------------------------------------------------------------------------------------#
# install.packages('deSolve')   # uncomment if the deSolve package is not yet installed

# GENERAL SETTINGS
if ( length( dev.list() ) > 1 ){
  for (i in 1:length( dev.list()  )){ dev.off() }
}

source('./Components/LINTUL2_Cassava_Plot_LeavesStemRootDynamics.R')
source('./Components/LINTUL2_Cassava_Plot_PLantNutrients.R')
source('./Components/LINTUL2_Cassava_Plot_PlantNutrientsContents.R')
source('./Components/LINTUL2_Cassava_Plot_NPK_water.R')

#===========================================================================================================
#PLOTTING validation results
#===========================================================================================================
  #Read in the data needed
  BIOMASS <- read.csv('./Data/Biomass_NPKuptake_plantpart_totals_per_treatment.csv', header = T) #plant parts g DM/m2
  
  #Change harvest day for Benue 2017 (needed as weather file only has data up to and including SDOY 530)
  ii <- which(BIOMASS[,"Location"] == "Benue" & BIOMASS[,"Year"] == 2017 & BIOMASS[,"time"] == 531)
  BIOMASS[ii,"time"] <- 530
  
  BIOMASS <- subset(BIOMASS, HarvestNumber == 3)
  
  treatments = data.frame(leg_text =     c("NfPfKf",      "NfPfK240",  "NfPfK180",  "NfPfK120", "NfPfK60",  "N150P40K180",   "N75P20K90",  "N0P0K0",      "N0PfKf",    "NfP0Kf",     "NfPfK0"),
                          cols =         c(rgb(0,0.2,0),rgb(0,0.4,0),rgb(0, 0.6,0),rgb(0,0.8,0),rgb(0,1,0),rgb(0.4,0,0.4),rgb(0.6,0,0.6),rgb(1,0,0),rgb(0.4,0.4,0),rgb(0,0,0.6),rgb(0,0.4,0.4)),
                          ltys =         c(         1,             1,           1,            1,         2,            2,              1,         3,             3,           3,            3 ),
                          lwds =         c(         3,             1,           1,            1,         3,            3,              1,         3,             1,           1,            1 ),
                          pchs =         c(        21,            21,          21,           21,        21,           22,             22,        22,             23,         24,           25 ),
                          cexs =         c(       2.5,           2.0,         1.5,          1.0,      0.75,          2.5,            1.5,       1.5,           1.5,         1.5,          1.5 ),
                          treatm_names = c("NfPfKf",        "NfPfK1",    "NfPfK2",     "NfPfK3",  "NfPfK4",       "NOTF",         "NOTH", "Control",      "N0PfKf",    "NfP0Kf",      "NfPfK0"))
  
  location   = c("Edo",    "CRS", "Benue", "CRS", "Benue")
  year       = c( 2017,     2017,    2017,  2016,    2016)
  bg_loc     = c("black", "blue",   "red",  "cyan", "orange") 
  

  location   = c("Edo",    "CRS", "Benue")
  year       = c( 2017,     2017,    2017)
  bg_loc     = c("black", "blue",   "red")
  
  ########BIOMASS, NTOT. PTOT, colored per treatment 
  meas_vs_pred_figure <- function( Treat = NULL){
    par(mfrow=c(2,2),mar=c(4,4,1,0),oma=c(2,2,2,2.0))
    
    #######BIOMASS
    plot(c(0,4000),c(0,4000),col="red",type="l",lty=1, lwd=2,
         main="2017 planting", xlab="Measured root yield , g DM m-2", ylab="Modelled root yield , g DM m-2")
    legend("topleft",legend = Treat[,"leg_text"],col=Treat[,"cols"],pch=Treat[,"pchs"],
           pt.cex=Treat[,"cexs"], bty = "n")  
    
    
    XY = NULL
    for (locyr in 1:length(location)) {
      for (trt in 1:nrow(Treat)) {
        #Read in the data needed
        filename = paste0("./Results/LINTUL_CASSAVA_nutrient_limited_growth_",
                          location[locyr],"_",year[locyr],"_",Treat[trt,"treatm_names"],".csv")
        print(paste0("adding...",filename))
        
        NUlim <- read.csv(filename)
  
        ii <- which(BIOMASS[,"Location"] == location[locyr] 
                    & BIOMASS[,"Year"] == year[locyr]
                    & BIOMASS[,"Treatment"] == as.character(Treat[trt,"treatm_names"]))
        
        xy = data.frame(x=BIOMASS[ii,"DMRoots.g.DM.m2"],  WSO = subset(NUlim,time %in% BIOMASS[ii,"time"],select = "WSO"))
        points(xy[,"x"],xy[,"WSO"],col=Treat[trt,"cols"],pch=Treat[trt,"pchs"],cex=Treat[trt,"cexs"],bg = bg_loc[locyr])
        xy[,"err"] <- (xy[,"x"] - xy[,"WSO"])
        XY = rbind(XY, xy)
      }
    }  
    yfit <- lm(WSO ~ 1 + x, XY)
    syfit <- summary(yfit)
    lines(x = c(min(XY[,"x"]),max(XY[,"x"])),syfit$coefficients[1] + c(min(XY[,"x"]),max(XY[,"x"])) * syfit$coefficients[2], col="red",lty=2,lwd=2)
    text(x= 2000, y= 10, paste0("y =",round(syfit$coefficients[1],3)," + ", 
                round(syfit$coefficients[2],3)," x; R2 = ",
                round(syfit$r.squared, 2)))
    text(x=2000, y=3800, paste0("RMSEP = ", round( sqrt(mean(XY[,"err"]^2)),1) ))
    
    #######NTOT  
    plot(c(0,50),c(0,50),col="red",type="l",lty=1, lwd=2,
         main="2017 planting", xlab="Measured N uptake , g m-2", ylab="Modelled N uptake, g m-2")
    legend("topleft",legend=location,pch=15, pt.cex=1.5, col=bg_loc, bg=bg_loc, bty="n")  
  
    XY = NULL
    for (locyr in 1:length(location)) {
      for (trt in 1:nrow(Treat)) {
        #Read in the data needed
        filename = paste0("./Results/LINTUL_CASSAVA_nutrient_limited_growth_",
                          location[locyr],"_",year[locyr],"_",Treat[trt,"treatm_names"],".csv")
        print(paste0("adding...",filename))
        
        NUlim <- read.csv(filename)
        NUlim[,"N_ABG"] = NUlim[,"ANLVG"] + NUlim[,"ANST"] + NUlim[,"ANSO"]
  
        ii <- which(BIOMASS[,"Location"] == location[locyr] 
                    & BIOMASS[,"Year"] == year[locyr]
                    & BIOMASS[,"Treatment"] == as.character(Treat[trt,"treatm_names"]))
        
        xy = data.frame(x=BIOMASS[ii,"NUP.g.m2"],  N_ABG=subset(NUlim,time %in% BIOMASS[ii,"time"],select = "N_ABG"))
        points(xy[,"x"],xy[,"N_ABG"],col=Treat[trt,"cols"],pch=Treat[trt,"pchs"],cex=Treat[trt,"cexs"],bg=bg_loc[locyr])
        xy[,"err"] <- (xy[,"x"] - xy[,"N_ABG"])
        XY = rbind(XY, xy)
      }
    }
    yfit <- lm(N_ABG ~ 1 + x, XY)
    syfit <- summary(yfit)
    lines(x = c(min(XY[,"x"]),max(XY[,"x"])),syfit$coefficients[1] + c(min(XY[,"x"]),max(XY[,"x"])) * syfit$coefficients[2], col="red",lty=2,lwd=2)
    text(x= 25, y= 1, paste0("y =",round(syfit$coefficients[1],3)," + ", 
                                  round(syfit$coefficients[2],3)," x; R2 = ",
                                  round(syfit$r.squared, 2)))
    text(x=25, y = 48, paste0("RMSEP = ", round( sqrt(mean(XY[,"err"]^2)),1) ))
    
    
    #######PTOT  
    plot(c(0,10),c(0,10),col="red",type="l",lty=1, lwd=2,
         xlab="Measured P uptake , g m-2", ylab="Modelled P uptake, g m-2")
    XY = NULL
    for (locyr in 1:length(location)) {
      for (trt in 1:nrow(Treat)) {
        #Read in the data needed
        filename = paste0("./Results/LINTUL_CASSAVA_nutrient_limited_growth_",
                          location[locyr],"_",year[locyr],"_",Treat[trt, "treatm_names"],".csv")
        print(paste0("adding...",filename))
        
        NUlim <- read.csv(filename)
        NUlim[,"P_ABG"] = NUlim[,"APLVG"] + NUlim[,"APST"] + NUlim[,"APSO"]
  
        ii <- which(BIOMASS[,"Location"] == location[locyr] 
                    & BIOMASS[,"Year"] == year[locyr]
                    & BIOMASS[,"Treatment"] == as.character(Treat[trt, "treatm_names"]))
        
        xy = data.frame(x=BIOMASS[ii,"PUP.g.m2"],  P_ABG=subset(NUlim,time %in% BIOMASS[ii,"time"],select = "P_ABG"))
        points(xy[,"x"],xy[,"P_ABG"],col=Treat[trt, "cols"],pch=Treat[trt, "pchs"],cex=Treat[trt, "cexs"],bg=bg_loc[locyr])
        xy[,"err"] <- (xy[,"x"] - xy[,"P_ABG"])
        XY = rbind(XY, xy)
      }
    }
    yfit <- lm(P_ABG ~ 1 + x, XY)
    syfit <- summary(yfit)
    lines(x = c(min(XY[,"x"]),max(XY[,"x"])),syfit$coefficients[1] + c(min(XY[,"x"]),max(XY[,"x"])) * syfit$coefficients[2], col="red",lty=2,lwd=2)
    text(x= 5, y= 0.5, paste0("y =",round(syfit$coefficients[1],3)," + ", 
                              round(syfit$coefficients[2],3)," x; R2 = ",
                              round(syfit$r.squared, 2)))
    text(x=5, y=9, paste0("RMSEP = ", round( sqrt(mean(XY[,"err"]^2)),1) ))
    
    #######KTOT  
    plot(c(0,40),c(0,40),col="red",type="l",lty=1, lwd=2,
         xlab="Measured K uptake , g m-2", ylab="Modelled K uptake, g m-2")
    
    XY = NULL
    for (locyr in 1:length(location)) {
      for (trt in 1:nrow(Treat)) {
        #Read in the data needed
        filename = paste0("./Results/LINTUL_CASSAVA_nutrient_limited_growth_",
                          location[locyr],"_",year[locyr],"_",Treat[trt, "treatm_names"],".csv")
        print(paste0("adding...",filename))
        
        NUlim <- read.csv(filename)
        NUlim[,"K_ABG"] = NUlim[,"AKLVG"] + NUlim[,"AKST"] + NUlim[,"ANSO"]
        
        ii <- which(BIOMASS[,"Location"] == location[locyr] 
                    & BIOMASS[,"Year"] == year[locyr]
                    & BIOMASS[,"Treatment"] == as.character(Treat[trt, "treatm_names"]))
    
        xy = data.frame(x=BIOMASS[ii,"KUP.g.m2"],  K_ABG=subset(NUlim,time %in% BIOMASS[ii,"time"],select = "K_ABG"))
        points(xy[,"x"],xy[,"K_ABG"],col=Treat[trt, "cols"],pch=Treat[trt, "pchs"],cex=Treat[trt, "cexs"],bg=bg_loc[locyr])
        xy[,"err"] <- (xy[,"x"] - xy[,"K_ABG"])
        XY = rbind(XY, xy)
      }
    }
    yfit <- lm(K_ABG ~ 1 + x, XY)
    syfit <- summary(yfit)
    lines(x = c(min(XY[,"x"]),max(XY[,"x"])),syfit$coefficients[1] + c(min(XY[,"x"]),max(XY[,"x"])) * syfit$coefficients[2], col="red",lty=2,lwd=2)
    text(x= 20, y= 1, paste0("y =",round(syfit$coefficients[1],3)," + ", 
                              round(syfit$coefficients[2],3)," x; R2 = ",
                              round(syfit$r.squared, 2)))
    text(x=20, y=38, paste0("RMSEP = ", round( sqrt(mean(XY[,"err"]^2)),1) ))
  }
  windows(width=10, height=9,xpos=-700)
  meas_vs_pred_figure(Treat = treatments)
  
  #Copy to PDF
  dev.copy(pdf,'./Figures/LINTUL_CASSAVA_VALIDATION_EDO17_BENUE17_CRS17_ALL_TREATMENTS.pdf')
  dev.off()
  
  #Only selected to have fully independent estimates of N, P and K uptake, i.e. excluding control and omission treatments
  sub_treatments = subset(treatments, leg_text %in% c("NfPfKf","NfPfK240","NfPfK180","NfPfK120", "NfPfK60", "N150P40K180", "N75P20K90"))

  windows(width=10, height=9,xpos=-600)
  meas_vs_pred_figure(Treat = sub_treatments)
  
  #Copy to PDF
  dev.copy(pdf,'./Figures/LINTUL_CASSAVA_VALIDATION_EDO17_BENUE17_CRS17_SELECTED_TREATMENTS.pdf')
  dev.off()