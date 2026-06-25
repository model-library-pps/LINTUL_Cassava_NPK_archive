#-------------------------------------------------------------------------------------------------#
# LINTUL2-CASSAVA_NPK run script
#
# Author:       Rob van den Beuken
# Copyright:    Copyright 2019, PPS
# Email:        rob.vandenbeuken@wur.nl
# Date:         06-02-2019
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
if(length(dev.list()) > 1){
  for(i in 1:length(dev.list())){dev.off()}
}

rm(list=ls())
# install.packages('deSolve')   # uncomment if the deSolve package is not yet installed
require('deSolve')              #used for solving ODEs
source('./Components/LINTUL2_CASSAVA_NPK_iniSTATES.R')
source("./Components/LINTUL2_CASSAVA_PARAMETERS_EZUI.r")
source('./Components/LINTUL2_CASSAVA_PARAMETERS_ADIELE.R')
source('./Components/LINTUL2_CASSAVA_NPK_PARAMETERS.R')
source('./Components/LINTUL2_CASSAVA_NPK_field_management.R')
source('./Components/LINTUL2_Cassava_NPK_RUN.R')
source('./Components/LINTUL2_Cassava_Plot_LeavesStemRootDynamics.R')
source('./Components/LINTUL2_Cassava_Plot_PLantNutrients.R')
source('./Components/LINTUL2_Cassava_Plot_SoilNutrients.R')
source('./Components/LINTUL2_Cassava_Plot_PlantNutrientsContents.R')
source('./Components/LINTUL2_Cassava_Plot_NPK_water.R')
source('./Components/LINTUL2_Cassava_Weather.R')
source('./Components/LINTUL2_CASSAVA_NPK.r')	


#Read in data from the soil (generated in the "Calibration" )
#Soil <- read.csv("Data//Soil_NPK_supply_fertilizer_recovery_per_year_per_location.csv")
SiteInfo <- read.csv("./Data//Field_characteristics_field_management.csv")


#wdirectory <- paste0(getwd(),"/Weather/")
country   <- "nig"
#--------------------------------------------------------------------------------------------------#
fertilizerNPK= data.frame(  N = c(  0, 100, 100, 100),#kg/ha N
                           P = c(100,   0,   0,   0),#kg/ha P
                           K = c(  0, 100, 100, 100)) #kg/ha K


year       <- 2017
wdata <- get_weather('./Weather/', country=country, station='1', year=substr(toString(year),2,4), endtime = 515)
#Water-limited growth
pars_rf  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK(SiteInfo = subset(SiteInfo, Location == "Edo" & Year_of_planting == 2017),
                                                 MODEL_PARAM = LINTUL2_CASSAVA_NPK_PARAMETERS(irri = FALSE, nutrient_limited = FALSE),
                                                 fertilizerNPK = fertilizerNPK)
Wlim_Edo17 <- LINTUL2_CASSAVA_NPK_RUN(wdata = wdata,  pars = pars_rf, year = year, starttime = pars_rf[["DOYPL"]]-100, endtime = pars_rf[["DOYHAR"]])

pars_rf  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK(SiteInfo = subset(SiteInfo, Location == "Edo" & Year_of_planting == 2017),
                                                 MODEL_PARAM = LINTUL2_CASSAVA_NPK_PARAMETERS(irri = FALSE, nutrient_limited = TRUE),
                                                 fertilizerNPK = fertilizerNPK)
NUlim_Edo17 <- LINTUL2_CASSAVA_NPK_RUN(wdata = wdata,  pars = pars_rf, year = year, starttime = pars_rf[["DOYPL"]]-100, endtime = pars_rf[["DOYHAR"]])
write.csv(NUlim_Edo17,'./Results/LINTUL_CASSAVA_nutrient_limited_growth_EDO_2017_NfPfKf.csv')

year       <- 2016
wdata <- get_weather('./Weather/', country=country, station='3', year=substr(toString(year),2,4), endtime = 620)
pars_rf  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK(SiteInfo = subset(SiteInfo, Location == "Cross River1" & Year_of_planting == 2016),
                                                 MODEL_PARAM = LINTUL2_CASSAVA_NPK_PARAMETERS(irri = FALSE),
                                                 fertilizerNPK = fertilizerNPK)
NUlim_CRS16 <- LINTUL2_CASSAVA_NPK_RUN(wdata = wdata,  pars = pars_rf, year = year, starttime = pars_rf[["DOYPL"]]-100, endtime = pars_rf[["DOYHAR"]])
write.csv(NUlim_CRS16,'./Results/LINTUL_CASSAVA_nutrient_limited_growth_CRS3_2016_NfPfKf.csv')

year       <- 2017
wdata <- get_weather('./Weather/', country=country, station='4', year=substr(toString(year),2,4), endtime = 550)
pars_rf  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK(SiteInfo = subset(SiteInfo, Location == "Cross River2" & Year_of_planting == 2017),
                                                 MODEL_PARAM = LINTUL2_CASSAVA_NPK_PARAMETERS(irri = FALSE),
                                                 fertilizerNPK = fertilizerNPK)
NUlim_CRS17 <- LINTUL2_CASSAVA_NPK_RUN(wdata = wdata,  pars = pars_rf, year = year, starttime = pars_rf[["DOYPL"]]-100, endtime = pars_rf[["DOYHAR"]])
write.csv(NUlim_CRS17,'./Results/LINTUL_CASSAVA_nutrient_limited_growth_CRS_2017_NfPfKf.csv')


year       <- 2017
wdata <- get_weather('./Weather/', country=country, station='2', year=substr(toString(year),2,4), endtime = 550)
pars_rf  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK(SiteInfo = subset(SiteInfo, Location == "Benue1" & Year_of_planting == 2017),
                                                 MODEL_PARAM = LINTUL2_CASSAVA_NPK_PARAMETERS(irri = FALSE),
                                                 fertilizerNPK = fertilizerNPK)
NUlim_Ben17 <- LINTUL2_CASSAVA_NPK_RUN(wdata = wdata,  pars = pars_rf, year = year, starttime = pars_rf[["DOYPL"]]-100, endtime = pars_rf[["DOYHAR"]])
write.csv(NUlim_Ben17,'./Results/LINTUL_CASSAVA_nutrient_limited_growth_BENUE_2017.csv')

#===========================================================================================================
#PLOTTING biomass figure
#===========================================================================================================
  #Read in the data needed
  Pot_Edo17 <- read.csv('./Results/LINTUL_CASSAVA_potential_growth_EDO_2017.csv')
  Wlim_Edo17 <- read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_EDO_2017.csv')
  NUlim_Edo17_NfPfKf <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_EDO_2017_NfPfKf.csv')
  NUlim_Edo17_NOTF <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_EDO_2017_NOTF.csv')
  
  Pot_CRS16 <- read.csv('./Results/LINTUL_CASSAVA_potential_growth_CRS_2016.csv')
  Wlim_CRS16 <- read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_CRS_2016.csv')
  NUlim_CRS16_NfPfKf <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_CRS_2016_NfPfKf.csv')
  NUlim_CRS16_NOTF <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_CRS_2016_NOTF.csv')
  Pot_CRS17 <- read.csv('./Results/LINTUL_CASSAVA_potential_growth_CRS_2017.csv')
  Wlim_CRS17 <- read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_CRS_2017.csv')
  NUlim_CRS17_NfPfKf <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_CRS_2017_NfPfKf.csv')
  NUlim_CRS17_NOTF <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_CRS_2017_NOTF.csv')
  
  Pot_Ben17 <- read.csv('./Results/LINTUL_CASSAVA_potential_growth_BENUE_2017.csv')
  Wlim_Ben17 <- read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_BENUE_2017.csv')
  NUlim_Ben17_NfPfKf <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_BENUE_2017_NfPfKf.csv')
  NUlim_Ben17_NOTF <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_BENUE_2017_NOTF.csv')
  Pot_Ben16 <- read.csv('./Results/LINTUL_CASSAVA_potential_growth_BENUE_2016.csv')
  Wlim_Ben16 <- read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_BENUE_2016.csv')
  NUlim_Ben16_NfPfKf <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_BENUE_2016_NfPfKf.csv')
  NUlim_Ben16_NOTF <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_BENUE_2016_NOTF.csv')
  
  BIOMASS <- read.csv('./Data/Biomass_NPKuptake_plantpart_totals_per_treatment.csv', header = T) #plant parts g DM/m2
  
  #Plot figures with leaves, stem and root biomass (g/m2) as function of time
  #Shows NfPfKf as Water limited and NOTH as nutrient limited
  windows(width=10, height=9,xpos=-700)
  par(mfcol=c(3,3),mar=c(1,4,1,0),oma=c(3,1,1,0))
  LeavesStemRootDynamics(Series1=Pot_Edo17,Series2=NUlim_Edo17_NfPfKf,Series3=NUlim_Edo17_NOTF,
                         BIOMASS=subset(BIOMASS,Location == "Edo"  & Year == 2017 & (Treatment == "NfPfKf" | Treatment == "NOTF")),
                         TITLE="Edo '17",LEG=TRUE, LEGTEXT = c('Potential', 'NfPfKf', 'N150P40K180','Measured'))
  par(mar=c(1,2,1,2))
  LeavesStemRootDynamics(Series1=Pot_Ben17,Series2=NUlim_Ben17_NfPfKf,Series3=NUlim_Ben17_NOTF,
                         BIOMASS=subset(BIOMASS,Location == "Benue" & Year == 2017 & (Treatment == "NfPfKf" | Treatment == "NOTF")),
                         TITLE="Ben. '17",LEG=FALSE, LEGTEXT = c('Potential', 'NfPfKf', 'N150P40K180','Measured'))
  mtext(side=1,text="Days after planting",outer=FALSE,line=2.5)
  par(mar=c(1,0,1,4))
  LeavesStemRootDynamics(Series1=Pot_CRS17,Series2=NUlim_CRS17_NfPfKf,Series3=NUlim_CRS17_NOTF,
                         BIOMASS=subset(BIOMASS,Location == "CRS" & Year == 2017 & (Treatment == "NfPfKf" | Treatment == "NOTF")),
                         TITLE="CRS '17",LEG=FALSE, LEGTEXT = c('Potential', 'NfPfKf', 'N150P40K180','Measured'))

  #Copy to PDF
  dev.copy(pdf,'./Figures/LINTUL_CASSAVA_potential_water_nutrient_limited_biomass_EDO_CRS_BENUE_2017.pdf')
  dev.off()

  
  #Plot figures with leaves, stem and root biomass (g/m2) as function of time
  windows(width=10, height=9,xpos=-700)
  par(mfcol=c(3,3),mar=c(1,4,1,0),oma=c(3,1,1,0))
  LeavesStemRootDynamics(Series1=Pot_Ben16,Series2=Wlim_Ben16,Series3=NUlim_Ben16_NOTF,BIOMASS=subset(BIOMASS,Location == "Benue" & Year == 2016 & (Treatment == "NfPfKf" | Treatment == "NOTF")),TITLE="Ben. '16",LEG=FALSE)
  par(mar=c(1,2,1,2))
  LeavesStemRootDynamics(Series1=Pot_Ben17,Series2=Wlim_Ben17,Series3=NUlim_Ben17_NOTF,BIOMASS=subset(BIOMASS,Location == "Benue" & Year == 2017 & (Treatment == "NfPfKf" | Treatment == "NOTF")),TITLE="Ben. '17",LEG=FALSE)
  mtext(side=1,text="Days after planting",outer=FALSE,line=2.5)
  par(mar=c(1,0,1,4))
  LeavesStemRootDynamics(Series1=Pot_CRS16,Series2=Wlim_CRS16,Series3=NUlim_CRS16_NOTF,BIOMASS=subset(BIOMASS,Location == "CRS" & Year == 2016 & (Treatment == "NfPfKf" | Treatment == "NOTF")),TITLE="CRS '16",LEG=FALSE)
  
  #Copy to PDF
  dev.copy(pdf,'./Figures/LINTUL_CASSAVA_potential_water_nutrient_limited_biomass_BEN2016_BEN2017_CRS2016.pdf')
  dev.off()  
  
  
  #===========================================================================================================
  #PLOTTING nutrient concentrations figure
  #===========================================================================================================
  
  #Plot figures with leaves, stem and root biomass (g/m2) as function of time
  windows(width=10, height=9,xpos=-350)
  par(mfcol=c(3,3),mar=c(1,4,1,0),oma=c(3,1,1,0))
  PlantNutrients(NUlim=NUlim_Edo17_NfPfKf,TITLE="Edo '17",LEG=TRUE)
  par(mar=c(1,2,1,2))
  PlantNutrients(NUlim=NUlim_Ben17_NfPfKf,TITLE="Ben. '17",LEG=FALSE)
  mtext(side=1,text="Days after planting",outer=FALSE,line=2.5)
  par(mar=c(1,0,1,4))
  PlantNutrients(NUlim=NUlim_CRS17_NfPfKf,TITLE="CRiv. '17",LEG=FALSE)
  
  
  #Copy to PDF
  dev.copy(pdf,'./Figures/LINTUL_CASSAVA_soil_plant_nutrients_EDO_CRS_BENUE_2017.pdf')
  dev.off()

  #===========================================================================================================
  #PLOTTING nutrient content figure
  #===========================================================================================================
  
  #Plot figures with leaves, stem and root biomass (g/m2) as function of time
  windows(width=10, height=9,xpos=-350)
  par(mfcol=c(3,3),mar=c(1,4,1,0),oma=c(3,1,1,0))
  PlantNutrientContents(NUlim=NUlim_Edo17_NfPfKf,TITLE="Edo '17",LEG=TRUE)
  par(mar=c(1,2,1,2))
  PlantNutrientContents(NUlim=NUlim_Ben17_NfPfKf,TITLE="Ben. '17",LEG=FALSE)
  mtext(side=1,text="Days after planting",outer=FALSE,line=2.5)
  par(mar=c(1,0,1,4))
  PlantNutrientContents(NUlim=NUlim_CRS17_NfPfKf,TITLE="CRiv. '17",LEG=FALSE)
  
  
  #Copy to PDF
  dev.copy(pdf,'./Figures/LINTUL_CASSAVA_soil_plant_nutrient_contents_EDO_CRS_BENUE_2017.pdf')
  dev.off()  
  
  #Plot figures with leaves, stem and root biomass (g/m2) as function of time
  windows(width=10, height=9,xpos=-350)
  par(mfcol=c(3,3),mar=c(1,4,1,0),oma=c(3,1,1,0))
  PlantNutrientContents(NUlim=NUlim_CRS16_NfPfKf,TITLE="CRS '16",LEG=TRUE)
  par(mar=c(1,2,1,2))
  PlantNutrientContents(NUlim=NUlim_CRS17_NfPfKf,TITLE="CRS '17",LEG=FALSE)
  mtext(side=1,text="Days after planting",outer=FALSE,line=2.5)
  par(mar=c(1,0,1,4))
  PlantNutrientContents(NUlim=NUlim_Ben16_NfPfKf,TITLE="Ben. '16",LEG=FALSE)
  
  
  #Copy to PDF
  dev.copy(pdf,'./Figures/LINTUL_CASSAVA_soil_plant_nutrient_contents_CRS2016_CRS2017_BENUE2017.pdf')
  dev.off()  
  
  #===========================================================================================================
  #PLOTTING NPKI, soil nutrient, water figure
  #===========================================================================================================
  
  #Plot figures with leaves, stem and root biomass (g/m2) as function of time
  windows(width=10, height=9,xpos=-10)
  par(mfcol=c(3,3),mar=c(1,4,1,0),oma=c(3,1,1,0))
  NPKI_nutrients_water(NUlim=NUlim_Edo17_NfPfKf,TITLE="Edo '17",LEG=TRUE)
  par(mar=c(1,2,1,2))
  NPKI_nutrients_water(NUlim=NUlim_Ben17_NfPfKf,TITLE="Ben. '17",LEG=FALSE)
  mtext(side=1,text="Days after planting",outer=FALSE,line=2.5)
  par(mar=c(1,0,1,4))
  NPKI_nutrients_water(NUlim=NUlim_CRS17_NfPfKf,TITLE="CRS '17",LEG=FALSE)
  
  
  #Copy to PDF
  dev.copy(pdf,'./Figures/LINTUL_CASSAVA_NPKI_soilnutrients_water_EDO_CRS_BENUE_2017.pdf')
  dev.off()
  
  #===========================================================================================================
  #PLOTTING NPKI, soil nutrient, water figure
  #===========================================================================================================
  
  #Plot figures with leaves, stem and root biomass (g/m2) as function of time
  windows(width=10, height=9,xpos=-10)
  par(mfcol=c(3,3),mar=c(1,4,1,0),oma=c(3,1,1,0))
  NPKI_nutrients_water(NUlim=NUlim_Ben16,TITLE="Ben '16",LEG=TRUE)
  par(mar=c(1,2,1,2))
  NPKI_nutrients_water(NUlim=NUlim_Ben17,TITLE="Ben. '17",LEG=FALSE)
  mtext(side=1,text="Days after planting",outer=FALSE,line=2.5)
  par(mar=c(1,0,1,4))
  NPKI_nutrients_water(NUlim=NUlim_CRS16,TITLE="CRS '16",LEG=FALSE)
  
  
  #Copy to PDF
  dev.copy(pdf,'./Figures/LINTUL_CASSAVA_NPKI_soilnutrients_water_BEN2016_BEN2017_CRS2016.pdf')
  dev.off()  
  
  
  #Plot figures with soil nutrients (g/m2) as function of time
  windows(width=10, height=9,xpos=-10)
  par(mfcol=c(3,3),mar=c(1,4,1,0),oma=c(3,1,1,0))
  SoilNutrients(NUlim=NUlim_Edo17,TITLE="Edo '17",LEG=TRUE)
  par(mar=c(1,2,1,2))
  SoilNutrients(NUlim=NUlim_Ben17,TITLE="Ben. '17",LEG=FALSE)
  mtext(side=1,text="Days after planting",outer=FALSE,line=2.5)
  par(mar=c(1,0,1,4))
  SoilNutrients(NUlim=NUlim_CRS17,TITLE="CRiv. '17",LEG=FALSE)
  
  
  #Copy to PDF
  dev.copy(pdf,'./Figures/LINTUL_CASSAVA_Soilnutrients_EDO_CRS_BENUE_2017.pdf')
  dev.off()
  