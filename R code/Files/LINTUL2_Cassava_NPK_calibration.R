#-------------------------------------------------------------------------------------------------#
# FUNCTION calibration
#
# Author:       AGT Schut
# Copyright:    Copyright 2020, PPS
# Email:        tom.schut@wur.nl
# Date:         31-01-2020
#
# This file contains a function to run a alibration for the LINTUL-CASSAVA_NPK model. 
# The purpose of this function is adjust specified parameters to optimize model outputs
#
#--------------------------------------------------------------------------------------------------#
rm(list=ls())
# install.packages('deSolve')   # uncomment if the deSolve package is not yet installed
require('deSolve')              #used for solving ODEs
source('./Components/LINTUL2_CASSAVA_iniSTATES.R')
source('./Components/LINTUL2_CASSAVA.r')	
source('./Components/LINTUL2_CASSAVA_NPK_iniSTATES.R')
source("./Components/LINTUL2_CASSAVA_PARAMETERS_EZUI.r")
source('./Components/LINTUL2_CASSAVA_PARAMETERS_ADIELE.R')
source('./Components/LINTUL2_CASSAVA_NPK_PARAMETERS.R')
source('./Components/LINTUL2_CASSAVA_NPK_field_management.R')
source('./Components/LINTUL2_Cassava_NPK_RUN.R')
source('./Components/LINTUL2_CASSAVA_NPK.r')	
source('./Components/LINTUL2_Cassava_Plot_LeavesStemRootDynamics.R')
source('./Components/LINTUL2_Cassava_Plot_PLantNutrients.R')
source('./Components/LINTUL2_Cassava_Plot_SoilNutrients.R')
source('./Components/LINTUL2_Cassava_Plot_NPK_water.R')
source('./Components/LINTUL2_Cassava_Plot_PlantNutrientsContents.R')
source('./Components/LINTUL2_Cassava_Plot_NPK_water.R')
source('./Components/LINTUL2_Cassava_NPK_CALIBRATION_functions.R')

if(length(dev.list()) > 1){
  for (i in 2:length(dev.list())){
    dev.off()
  }
}

#--------------------------------------------------------------------------------------------------#
#Read the data
data <- read.csv("Data//Biomass_NPKuptake_plantpart_totals_per_treatment.csv")

#Subset to Edo in 2016 only
sdata <- subset(data,Location == "Edo" & Year == 2016)

Soil <- read.csv("Data//Soil_NPK_supply_fertilizer_recovery_per_year_per_location.csv")
SiteInfo <- read.csv("./Data//Field_characteristics_field_management.csv")
#--------------------------------------------------------------------------------------------------#

#Prepare parameter sets----------------------------------------------------------------------------#
wdata <- get_weather(paste0(getwd(),"/Weather/"), country = "nig", station='1', year="016", endtime = 660)
#Treatment FULL
NPKfromSoil = subset(Soil, Location == "Edo" & Year == 2016)

fertilizerNPK = data.frame(  N = c(  0, 100, 100, 100),#kg/ha N
                           P = c(100,   0,   0,   0),#kg/ha P
                           K = c(  0, 100, 100, 100)) #kg/ha K
pars_NfPfKf  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK(SiteInfo = subset(SiteInfo, Location == "Edo" & Year_of_planting == 2016),
                                                     MODEL_PARAM = LINTUL2_CASSAVA_NPK_PARAMETERS(irri = FALSE),
                                                     fertilizerNPK = fertilizerNPK)
fertilizerNPK=data.frame(  N = c(  0, 100, 100, 100),#kg/ha N
                           P = c(100,   0,   0,   0),#kg/ha P
                           K = c(  0,  60,  60,  60)) #kg/ha K
pars_NfPfK180  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK(SiteInfo = subset(SiteInfo, Location == "Edo" & Year_of_planting == 2016),
                                                       MODEL_PARAM = LINTUL2_CASSAVA_NPK_PARAMETERS(irri = FALSE),
                                                       fertilizerNPK = fertilizerNPK)
fertilizerNPK=data.frame(  N = c(  0, 100, 100, 100),#kg/ha N
                           P = c(100,   0,   0,   0),#kg/ha P
                           K = c(  0,  20,  20,  20)) #kg/ha K
pars_NfPfK60  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK(SiteInfo = subset(SiteInfo, Location == "Edo" & Year_of_planting == 2016),
                                                      MODEL_PARAM = LINTUL2_CASSAVA_NPK_PARAMETERS(irri = FALSE),
                                                     fertilizerNPK = fertilizerNPK)
fertilizerNPK=data.frame(  N = c( 0, 50, 50, 50),#kg/ha N
                           P = c(40,  0,  0,  0),#kg/ha P
                           K = c( 0, 60, 60, 60)) #kg/ha K
pars_N150P40K180  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK(SiteInfo = subset(SiteInfo, Location == "Edo" & Year_of_planting == 2016),
                                                          MODEL_PARAM = LINTUL2_CASSAVA_NPK_PARAMETERS(irri = FALSE),                                                          
                                                         fertilizerNPK = fertilizerNPK)
fertilizerNPK=data.frame(  N = c(0, 0, 0, 0), #kg/ha N
                           P = c(0, 0, 0, 0), #kg/ha P
                           K = c(0, 0, 0, 0)) #kg/ha K
pars_N0P0K0  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK(SiteInfo = subset(SiteInfo, Location == "Edo" & Year_of_planting == 2016),
                                                     MODEL_PARAM = LINTUL2_CASSAVA_NPK_PARAMETERS(irri = FALSE),                                                          
                                                     fertilizerNPK = fertilizerNPK)

fertilizerNPK = data.frame(  N = c(  0, 0, 0, 0), #kg/ha N
                           P = c(100, 0, 0, 0), #kg/ha P
                           K = c(  0, 100, 100, 100)) #kg/ha K
pars_N0PfKf  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK(SiteInfo = subset(SiteInfo, Location == "Edo" & Year_of_planting == 2016),
                                                     MODEL_PARAM = LINTUL2_CASSAVA_NPK_PARAMETERS(irri = FALSE),                                                     
                                                     fertilizerNPK = fertilizerNPK)
fertilizerNPK = data.frame(  N = c(  0, 100, 100, 100), #kg/ha N
                             P = c(  0,   0,   0,   0), #kg/ha P
                             K = c(  0, 100, 100, 100)) #kg/ha K
pars_NfP0Kf  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK(SiteInfo = subset(SiteInfo, Location == "Edo" & Year_of_planting == 2016),
                                                     MODEL_PARAM = LINTUL2_CASSAVA_NPK_PARAMETERS(irri = FALSE),                                                     
                                                     fertilizerNPK = fertilizerNPK)
fertilizerNPK = data.frame(  N = c(  0, 100, 100, 100), #kg/ha N
                             P = c(100,   0,   0,   0), #kg/ha P
                             K = c(  0,   0,   0,   0)) #kg/ha K
pars_NfPfK0  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK(SiteInfo = subset(SiteInfo, Location == "Edo" & Year_of_planting == 2016),
                                                     MODEL_PARAM = LINTUL2_CASSAVA_NPK_PARAMETERS(irri = FALSE),                                                     
                                                     fertilizerNPK = fertilizerNPK)

pars_list = list(pars_NfPfKf, pars_NfPfK180, pars_NfPfK60,pars_N150P40K180, 
                 pars_N0P0K0, pars_N0PfKf, pars_NfP0Kf, pars_NfPfK0)
leg_text = c("NfPfKf", "NfPfK180", "NfPfK60","N150P40K180", "N0P0K0", "N0PfKf", "NfP0Kf", "NfPfK0")
cols =     c("darkgreen",  "blue",  "salmon","lightgreen", "darkred","indianred", "turquoise", "sandybrown")
ltys =     c(         1,       1,          2,            2,         3,         3,       3,      3 )
lwds =     c(         3,       1,          3,            3,         3,         1,       1,      1 )
pchs =     c(         0,       1,          2,            3,         4,         5,       15,      16 )
Trt_names = c("NfPfKf", "NfPfK2", "NfPfK4","NOTF", "Control", "N0PfKf", "NfP0Kf", "NfPfK0")
#End of parameter set preparations----------------------------------------------------------------------------#


#Calibration----------------------------------------------------------------------------#
pars_list_calib = list(pars_NfPfKf, pars_NfPfK180, pars_N150P40K180, pars_N0P0K0)
calibResult <- Calibrate_LINTUL_CASSAVA_NPK(wdata = wdata,  pars_list = pars_list_calib,
                             year = year, starttime = pars_NfPfKf[["DOYPL"]] - 100, endtime = 660,
                             parNamesToCalibrate = c("K_MAX",
                                                     "K_NPK_NI",
                                                     "TSUM_NPKI"),
                             StateNames   = c("WSO",
                                              "ANUP","APUP","AKUP"),#combined names for total uptake
                             obsData = sdata,
                             obsStateNames = c("DMRoots.g.DM.m2","NUP.g.m2","PUP.g.m2","KUP.g.m2"),
                             treatment_names = c("NfPfKf",
                                                 "NfPfK2",
                                                 "NOTF",
                                                 "Control"))

#Recompute all for the calibration data 

states_list = NULL
for (treat_nr in 1:length(pars_list)) {
  param <- pars_list[[treat_nr]]
  
  #adjust parameters values thar are calibrated
  param[calibResult[,"calibrationParameters"]] <- calibResult[,"calibValues"]
  states <- LINTUL2_CASSAVA_NPK_RUN(wdata = wdata,  pars = param, year = 2016, 
                                   starttime = param[["DOYPL"]] - 100, endtime = param[["DOYHAR"]])

  states[,"NUPT_AG"] <- states[,"ANLVG"] + states[,"ANST"] + states[,"ANSO"]
  states[,"PUPT_AG"] <- states[,"APLVG"] + states[,"APST"] + states[,"APSO"]
  states[,"KUPT_AG"] <- states[,"AKLVG"] + states[,"AKST"] + states[,"AKSO"]
  states[,"NUPT"] <- states[,"ANLVG"] + states[,"ANLVD"] + states[,"ANRT"] + states[,"ANST"] + states[,"ANSO"]
  states[,"PUPT"] <- states[,"APLVG"] + states[,"APLVD"] + states[,"APRT"] + states[,"APST"] + states[,"APSO"]
  states[,"KUPT"] <- states[,"AKLVG"] + states[,"AKLVD"] + states[,"AKRT"] + states[,"AKST"] + states[,"AKSO"]
  states_list=c(states_list,list(states))
}

#Plot figures with leaves, stem and root biomass (g/m2) as function of time
FigureNPKI <- function(){
  par(mfcol=c(3,4),mar=c(2,2,0,0),oma=c(3,3,1,1))
  NPKI_nutrients_water(NUlim=states_list[[1]],TITLE="Edo '16 NfPfKf",LEG=TRUE)
  NPKI_nutrients_water(NUlim=states_list[[2]],TITLE="Edo'16  NfPfK180",LEG=FALSE)
  NPKI_nutrients_water(NUlim=states_list[[3]],TITLE="Edo'16  NfPfK60",LEG=FALSE)
  NPKI_nutrients_water(NUlim=states_list[[4]],TITLE = "Edo'16 N150P40K180",LEG=FALSE)
  mtext(side=2,text="Nutrition index",outer=TRUE,line=1, cex=0.8, adj=1)
  mtext(side=2,text="Soil nutrients, kg/ha",outer=TRUE,line=1, cex=0.8, adj=0.55)
  mtext(side=2,text="Soil water, mm",outer=TRUE,line=1, cex=0.8, adj=0.1)
  mtext(side=1,text="Days since 1 January in year of planting",outer=TRUE,line=1.5)
}

#Plot figures with soil nutrients (g/m2) as function of time
FigureSoilNutrients <- function(){
  par(mfcol=c(3,4),mar=c(2,2,0,0),oma=c(3,3,1,1))
  SoilNutrients(NUlim=states_list[[1]],TITLE="Edo'16 NfPfKf",LEG=TRUE)
  SoilNutrients(NUlim=states_list[[2]],TITLE="Edo'16  NfPfK180",LEG=FALSE)
  SoilNutrients(NUlim=states_list[[3]],TITLE="Edo'16  NfPfK60",LEG=FALSE)
  SoilNutrients(NUlim=states_list[[4]],TITLE = "Edo'16 N150P40K180",LEG=FALSE)
  mtext(side=2,text="N, g/m2",outer=TRUE,line=1.0, adj=0.9)
  mtext(side=2,text="P, g/m2",outer=TRUE,line=1.0, adj=0.55)
  mtext(side=2,text="K, g/m2",outer=TRUE,line=1.0, adj=0.15)
  mtext(side=1,text="Days since 1 January in year of planting",outer=TRUE,line=1.5)
}

#Plot figures with mmodelled vs measured uptakes as function of time
FigureNPKuptake <- function(){
  par(mfrow=c(1,3),mar=c(2,2,1,1), oma=c(2,2,2,2))
  plot(states_list[[1]][,"time"],states_list[[1]][,"NUPT_AG"],type="l",col=cols[1],lwd=lwds[1],lty=ltys[1],ylim=c(0,60),ylab="N uptake, g/m2",xlab = "time", main="N")
  for(i in 2:length(pars_list)){
    lines(states_list[[i]][,"time"],states_list[[i]][,"NUPT_AG"],col=cols[i],lwd=lwds[i],lty=ltys[i])
  }
  
  for(ut in unique(sdata[,"Treatment"])){
    ii<- which(Trt_names==ut, arr.ind = TRUE)
    ij<-which(sdata[,"Treatment"]==ut)
    points(sdata[ij,"time"],sdata[ij,"NUP.g.m2"],col=cols[ii],pch=pchs[ii])
    for(p in ij){
      lines(c(sdata[p,"time"], sdata[p,"time"]),
             c(sdata[p,"NUP.g.m2"]-sdata[p,"std_NUP.g.m2"],
               sdata[p,"NUP.g.m2"]+sdata[p,"std_NUP.g.m2"]),
             col=cols[ii],lty=ltys[ii])
    }
  }
  plot(states_list[[1]][,"time"],states_list[[1]][,"PUPT_AG"],type="l",col=cols[1],lwd=lwds[1],lty=ltys[1],ylim=c(0,10),
       ylab="P uptake, g/m2",xlab = "time", main="P")
  for(i in 2:length(pars_list)){
    lines(states_list[[i]][,"time"],states_list[[i]][,"PUPT_AG"],col=cols[i],lwd = lwds[i],lty=ltys[i])
  }
  for(ut in unique(sdata[,"Treatment"])){
    ii<- which(Trt_names==ut, arr.ind = TRUE)
    ij<-which(sdata[,"Treatment"]==ut)
    points(sdata[ij,"time"],sdata[ij,"PUP.g.m2"],col=cols[ii],pch=pchs[ii])
    for(p in ij){
      lines(c(sdata[p,"time"], sdata[p,"time"]),
            c(sdata[p,"PUP.g.m2"]-sdata[p,"std_PUP.g.m2"],
              sdata[p,"PUP.g.m2"]+sdata[p,"std_PUP.g.m2"]),
            col=cols[ii],lty=ltys[ii])
    }
  }

  plot(states_list[[1]][,"time"],states_list[[1]][,"KUPT_AG"],type="l",col=cols[1],lwd=lwds[1],lty=ltys[1], ylim=c(0,40),
       ylab="K uptake, g/m2",xlab = "time", main="K")
  for(i in 2:length(pars_list)){
    lines(states_list[[i]][,"time"],states_list[[i]][,"KUPT_AG"],col=cols[i],lwd = lwds[i],lty=ltys[i])
  }
  for(ut in unique(sdata[,"Treatment"])){
    ii<- which(Trt_names==ut, arr.ind = TRUE)
    ij<-which(sdata[,"Treatment"]==ut)
    points(sdata[ij,"time"],sdata[ij,"KUP.g.m2"],col=cols[ii],pch=pchs[ii])
    for(p in ij){
      lines(c(sdata[p,"time"], sdata[p,"time"]),
            c(sdata[p,"KUP.g.m2"]-sdata[p,"std_KUP.g.m2"],
              sdata[p,"KUP.g.m2"]+sdata[p,"std_KUP.g.m2"]),
            col=cols[ii],lty=ltys[ii])
    }
  }
  mtext(side=2,text="Uptake, g/m2",outer=TRUE,line=0.2, adj=0.5)
  mtext(side=1,text="Days since 1 January in year of planting",outer=TRUE,line=0.2)
}

#Plot figures with modelled vs measured roots, leaves and stems as function of time
FigureBiomass <- function(){
  par(mfrow=c(1,3),mar=c(2,2,1,1), oma=c(2,2,2,2))
  plot(states_list[[1]][,"time"],states_list[[1]][,"WSO"],type="l",col=cols[1],lwd=lwds[1],lty=ltys[1],
       ylim=c(0,4000),ylab="Storage roots, g/m2",xlab = "time", main="Storage roots")
  for(i in 2:length(pars_list)){
    lines(states_list[[i]][,"time"],states_list[[i]][,"WSO"],col=cols[i],lwd = lwds[i],lty=ltys[i])
  }
  for(ut in unique(sdata[,"Treatment"])){
    ii<- which(Trt_names==ut, arr.ind = TRUE)
    ij<-which(sdata[,"Treatment"]==ut)
    points(sdata[ij,"time"],sdata[ij,"DMRoots.g.DM.m2"],col=cols[ii],pch=pchs[ii])
    for(p in ij){
      lines(c(sdata[p,"time"], sdata[p,"time"]),
            c(sdata[p,"DMRoots.g.DM.m2"]-sdata[p,"std_DMRoots.g.DM.m2"],
              sdata[p,"DMRoots.g.DM.m2"]+sdata[p,"std_DMRoots.g.DM.m2"]),
            col=cols[ii],lty=ltys[ii])
    }
  }
  legend("topleft",legend=leg_text, col=cols, lty = ltys, lwd = lwds, bty="n")
  
  plot(states_list[[1]][,"time"],states_list[[1]][,"WLVG"],type="l",col=cols[1],lwd=lwds[1],lty=ltys[1],
       ylim=c(0,600),ylab="Green leeaves, g/m2",xlab = "time", main="Leaves")
  for(i in 2:length(pars_list)){
    lines(states_list[[i]][,"time"],states_list[[i]][,"WLVG"],col=cols[i],lwd = lwds[i],lty=ltys[i])
  }
  for(ut in unique(sdata[,"Treatment"])){
    ii<- which(Trt_names==ut, arr.ind = TRUE)
    ij<-which(sdata[,"Treatment"]==ut)
    points(sdata[ij,"time"],sdata[ij,"DMLeaves.g.DM.m2"],col=cols[ii],pch=pchs[ii])
    for(p in ij){
      lines(c(sdata[p,"time"], sdata[p,"time"]),
            c(sdata[p,"DMLeaves.g.DM.m2"]-sdata[p,"std_DMLeaves.g.DM.m2"],
              sdata[p,"DMLeaves.g.DM.m2"]+sdata[p,"std_DMLeaves.g.DM.m2"]),
            col=cols[ii],lty=ltys[ii])
    }
  }  
  plot(states_list[[1]][,"time"],states_list[[1]][,"WST"],type="l",col=cols[1],lwd=lwds[1],lty=ltys[1],
       ylim=c(0,3000),ylab="Stems, g/m2",xlab = "time", main="Stems")
  for(i in 2:length(pars_list)){
    lines(states_list[[i]][,"time"],states_list[[i]][,"WST"],col=cols[i],lwd = lwds[i],lty=ltys[i])
  }
  for(ut in unique(sdata[,"Treatment"])){
    ii<- which(Trt_names==ut, arr.ind = TRUE)
    ij<-which(sdata[,"Treatment"]==ut)
    points(sdata[ij,"time"],sdata[ij,"DMstems.g.DM.m2"],col=cols[ii],pch=pchs[ii])
    for(p in ij){
      lines(c(sdata[p,"time"], sdata[p,"time"]),
            c(sdata[p,"DMstems.g.DM.m2"]-sdata[p,"std_DMstems.g.DM.m2"],
              sdata[p,"DMstems.g.DM.m2"]+sdata[p,"std_DMstems.g.DM.m2"]),
            col=cols[ii],lty=ltys[ii])
    }
  }
  mtext(side=2,text="Biomass, g DM/m2",outer=TRUE,line=0.2, adj=0.5)
  mtext(side=1,text="Days since 1 January in year of planting",outer=TRUE,line=0.2)
}

FigureDilution <- function(){
  par(mfrow=c(1,3),mar=c(2,2,1,1), oma=c(2,2,2,2))
  plot(states_list[[1]][,"WSO"]+states_list[[1]][,"WST"]+states_list[[1]][,"WLVG"],states_list[[1]][,"Nc"],type="l",col=cols[1],lty=ltys[1],
       ylim=c(0,4.0),ylab="Nc, %",xlab = "Biomass, g/m2", main="N")
  for(i in 2:length(pars_list)){
    lines(states_list[[i]][,"WSO"]+states_list[[i]][,"WST"]+states_list[[i]][,"WLVG"],states_list[[i]][,"Nc"],col=cols[i],lty=ltys[i])
  }
  plot(states_list[[1]][,"WSO"]+states_list[[1]][,"WST"]+states_list[[1]][,"WLVG"],states_list[[1]][,"Pc"],type="l",col=cols[1],lty=ltys[1],
       ylim=c(0,0.5),ylab="Pc, %",xlab = "Biomass, g/m2", main="P")
  for(i in 2:length(pars_list)){
    lines(states_list[[i]][,"WSO"]+states_list[[i]][,"WST"]+states_list[[i]][,"WLVG"],states_list[[i]][,"Pc"],col=cols[i],lty=ltys[i])
  }
  plot(states_list[[1]][,"WSO"]+states_list[[1]][,"WST"]+states_list[[1]][,"WLVG"],states_list[[1]][,"Kc"],type="l",col=cols[1],lty=ltys[1],
       ylim=c(0,2),ylab="Kc, %",xlab = "Biomass, g/m2", main="K")
  for(i in 2:length(pars_list)){
    lines(states_list[[i]][,"WSO"]+states_list[[i]][,"WST"]+states_list[[i]][,"WLVG"],states_list[[i]][,"Kc"],col=cols[i],lty=ltys[i])
  }
  legend("topright",legend=leg_text, col=cols, lty=ltys, lwd= lwds,bty="n")
  mtext(side=2,text="Concentration, %",outer=TRUE,line=0.2, adj=0.5)
  mtext(side=1,text="Biomass, g DM/m2",outer=TRUE,line=0.2)
}


windows(width=10, height=9,xpos=-100)
FigureNPKI()

windows(width=10, height=9,xpos=-400)
FigureSoilNutrients()

windows(width=10, height=9,xpos=-600)
FigureNPKuptake()

windows(width=10, height=9,xpos=-800)
FigureBiomass()

windows(width=10, height=9,xpos=-950)
FigureDilution()

pdf("./Figures/Calibration results for EDO 2016.pdf")
  FigureNPKI()
  FigureSoilNutrients()
  FigureNPKuptake()
  FigureBiomass()
  FigureDilution()
dev.off()
  
  