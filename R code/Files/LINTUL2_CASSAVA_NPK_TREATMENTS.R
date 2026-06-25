#-------------------------------------------------------------------------------------------------#
# LINTUL2-CASSAVA_NPK treatments script
#
# Author:       AGT Schut
# Copyright:    Copyright 2019, PPS
# Email:        tom.schut@wur.nl
# Date:         17-02-2020
#
# This file is used to run the LINTUL2_CASSAVA_NPK model for all treatments on the three locations
#
#--------------------------------------------------------------------------------------------------#
# BEFORE START
# It is important before running this script to set the working directory to source file location: 
# This is done as follows: Go to 'Session' -> 'Set Working Directory' -> To Source File Location
#--------------------------------------------------------------------------------------------------#
# install.packages('deSolve')   # uncomment if the deSolve package is not yet installed

# GENERAL SETTINGS
rm(list=ls())

# install.packages('deSolve')   # uncomment if the deSolve package is not yet installed
require('deSolve')              #used for solving ODEs
#LINTUL_CASSAVA
source('./Components/LINTUL2_Cassava_Weather.R')
#LINTUL_CASSAVA_NPK
source('./Components/LINTUL2_CASSAVA_NPK_iniSTATES.R')
source("./Components/LINTUL2_CASSAVA_PARAMETERS_EZUI.r")
source('./Components/LINTUL2_CASSAVA_PARAMETERS_ADIELE.R')
source('./Components/LINTUL2_CASSAVA_NPK_PARAMETERS.R')
source('./Components/LINTUL2_CASSAVA_NPK_field_management.R')
source('./Components/LINTUL2_Cassava_NPK_RUN.R')
source('./Components/LINTUL2_Cassava_NPK_GLAI.R')
source('./Components/LINTUL2_CASSAVA_NPK.r')


wdirectory <- paste0(getwd(),"/Weather/")
#--------------------------------------------------------------------------------------------------#
#Read in data from the site, soil characteristics and soil NPK supply.
SiteInfo <- read.csv("./Data//Field_characteristics_field_management.csv")

#Define fertilizer treatments
treatm_names = c("NfPfKf","NfPfK1","NfPfK2","NfPfK3","NfPfK4","NOTF","NOTH","Control","N0PfKf","NfP0Kf","NfPfK0")
fertilizerNPK = list(data.frame(  N = c(  0, 100, 100, 100), #kg/ha N, NfPfKf
                                  P = c(100,   0,   0,   0), #kg/ha P
                                  K = c(  0, 100, 100, 100)),#kg/ha K
                     data.frame(  N = c(  0, 100, 100, 100), #kg/ha N, NfPfK1
                                  P = c(100,   0,   0,   0), #kg/ha P
                                  K = c(  0,  80,  80,  80)),#kg/ha K
                     data.frame(  N = c(  0, 100, 100, 100), #kg/ha N, NfPfK2
                                  P = c(40,    0,   0,   0), #kg/ha P
                                  K = c(  0,  60,  60,  60)),#kg/ha K
                     data.frame(  N = c(  0, 100, 100, 100), #kg/ha N, NfPfK3
                                  P = c(100,   0,   0,   0), #kg/ha P
                                  K = c(  0,  40,  40,  40)),#kg/ha K
                     data.frame(  N = c(  0, 100, 100, 100), #kg/ha N, NfPfK4
                                  P = c(100,   0,   0,   0), #kg/ha P
                                  K = c(  0,  20,  20,  20)),#kg/ha K
                     data.frame(  N = c(  0,  50,  50,  50), #kg/ha N, NOTF
                                  P = c( 40,   0,   0,   0), #kg/ha P
                                  K = c(  0,  60,  60,  60)),#kg/ha K
                     data.frame(  N = c(  0,  25,  25,  25), #kg/ha N, NOTH
                                  P = c( 20,   0,   0,   0), #kg/ha P
                                  K = c(  0,  30,  30,  30)),#kg/ha K
                     data.frame(  N = c(  0,   0,   0,   0), #kg/ha N, Control
                                  P = c(  0,   0,   0,   0), #kg/ha P
                                  K = c(  0,   0,   0,   0)), #kg/ha K
                     data.frame(  N = c(  0,   0,   0,   0), #kg/ha N, N0PfKf
                                  P = c(100,   0,   0,   0), #kg/ha P
                                  K = c(  0, 100, 100, 100)),#kg/ha K
                     data.frame(  N = c(  0, 100, 100, 100), #kg/ha N, NfP0Kf
                                  P = c(  0,   0,   0,   0), #kg/ha P
                                  K = c(  0, 100, 100, 100)),#kg/ha K
                     data.frame(  N = c(  0, 100, 100, 100), #kg/ha N, NfPfK0
                                  P = c(100,   0,   0,   0), #kg/ha P
                                  K = c(  0,   0,   0,   0)))#kg/ha K

location   = c("Edo","Edo","CRS","CRS","Benue","Benue")
year       = c( 2016, 2017, 2016, 2017,   2016,   2017)
station_nr = c(  "1",  "1",  "3",  "4",    "2",    "2")

for (i in 1:length(location) ) {
  wdata <- get_weather('./Weather/', country = "nig", station = station_nr[i], 
                       year = substr(toString(year[i]),2,4), endtime = 730)
  
  #Potential growth
  pars_iri  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK(SiteInfo = subset(SiteInfo, Location == location[i] & Year_of_planting == year[i]),
                                                    MODEL_PARAM = LINTUL2_CASSAVA_NPK_PARAMETERS(irri = TRUE, nutrient_limited = FALSE))
  Pot <- LINTUL2_CASSAVA_NPK_RUN(wdata = wdata,  pars = pars_iri, year = year[i], 
                                 starttime = pars_iri[["DOYPL"]] - 100, endtime = pars_iri[["DOYHAR"]])
  filename = paste0("./Results/LINTUL_CASSAVA_potential_growth_",location[i],"_",year[i],".csv")
  write.csv(Pot,filename)
  print(paste0("Saved: ", filename))
  
  #Water-limited growth
  pars_rf  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK(SiteInfo = subset(SiteInfo, Location == location[i] & Year_of_planting == year[i]),
                                                   MODEL_PARAM = LINTUL2_CASSAVA_NPK_PARAMETERS(irri = FALSE, nutrient_limited = FALSE))
  Wlim <- LINTUL2_CASSAVA_NPK_RUN(wdata = wdata,  pars = pars_rf, year = year[i], 
                                  starttime = pars_rf[["DOYPL"]] - 100, endtime = pars_rf[["DOYHAR"]])
  filename = paste0("./Results/LINTUL_CASSAVA_water_limited_growth_",location[i],"_",year[i],".csv")
  write.csv(Wlim,filename)
  print(paste0("Saved: ", filename))
  
  
  #Nutrient-limited growth
  for (j in 1:length(fertilizerNPK)) {
    pars_nl  <- LINTUL2_CASSAVA_FIELD_MANAGEMENT_NPK(SiteInfo = subset(SiteInfo, Location == location[i] & Year_of_planting == year[i]),
                                                     MODEL_PARAM = LINTUL2_CASSAVA_NPK_PARAMETERS(irri = FALSE),                                                     
                                                     fertilizerNPK = fertilizerNPK[[j]])
    NUlim <- LINTUL2_CASSAVA_NPK_RUN(wdata = wdata,  pars = pars_nl, year = year[i], 
                                     starttime = pars_nl[["DOYPL"]] - 100, endtime = pars_nl[["DOYHAR"]])
    filename = paste0("./Results/LINTUL_CASSAVA_nutrient_limited_growth_",location[i],"_",year[i],"_",treatm_names[j],".csv")
    write.csv(NUlim,filename)
    print(paste0("Saved: ", filename))
  }
}

