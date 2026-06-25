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

source('./Components/LINTUL2_Cassava_Plot_LeavesStemRootDynamics.R')
source('./Components/LINTUL2_Cassava_Plot_PLantNutrients.R')
source('./Components/LINTUL2_Cassava_Plot_SoilNutrients.R')
source('./Components/LINTUL2_Cassava_Plot_PlantNutrientsContents.R')
source('./Components/LINTUL2_Cassava_Plot_NPK_water.R')
source('./LINTUL2_Cassava_NPK_validation.R')
library(grDevices)


#===========================================================================================================
#PLOTTING figures for paper
#===========================================================================================================
  #Read in the data needed
  Pot_Edo16 <- read.csv('./Results/LINTUL_CASSAVA_potential_growth_EDO_2016.csv')
  Wlim_Edo16 <- read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_EDO_2016.csv')
  NUlim_Edo16_NfPfKf <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_EDO_2016_NfPfKf.csv')
  NUlim_Edo16_NfPfK2 <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_EDO_2016_NfPfK2.csv')
  NUlim_Edo16_NfPfK4 <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_EDO_2016_NfPfK4.csv')
  NUlim_Edo16_NOTF <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_EDO_2016_NOTF.csv')
  
  Pot_Edo17 <- read.csv('./Results/LINTUL_CASSAVA_potential_growth_EDO_2017.csv')
  Wlim_Edo17 <- read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_EDO_2017.csv')
  NUlim_Edo17_NfPfKf <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_EDO_2017_NfPfKf.csv')
  NUlim_Edo17_NfPfK2 <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_EDO_2017_NfPfK2.csv')
  NUlim_Edo17_NfPfK4 <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_EDO_2017_NfPfK4.csv')
  NUlim_Edo17_NOTF <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_EDO_2017_NOTF.csv')

  
  Pot_CRS16 <- read.csv('./Results/LINTUL_CASSAVA_potential_growth_CRS_2016.csv')
  Wlim_CRS16 <- read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_CRS_2016.csv')
  NUlim_CRS16_NfPfKf <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_CRS_2016_NfPfKf.csv')
  NUlim_CRS16_NfPfK2 <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_CRS_2016_NfPfK2.csv')
  NUlim_CRS16_NfPfK4 <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_CRS_2016_NfPfK4.csv')
  NUlim_CRS16_NOTF <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_CRS_2016_NOTF.csv')
  
  Pot_CRS17 <- read.csv('./Results/LINTUL_CASSAVA_potential_growth_CRS_2017.csv')
  Wlim_CRS17 <- read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_CRS_2017.csv')
  NUlim_CRS17_NfPfKf <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_CRS_2017_NfPfKf.csv')
  NUlim_CRS17_NfPfK2 <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_CRS_2017_NfPfK2.csv')
  NUlim_CRS17_NfPfK4 <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_CRS_2017_NfPfK4.csv')
  NUlim_CRS17_NOTF <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_CRS_2017_NOTF.csv')

  Pot_Ben16 <- read.csv('./Results/LINTUL_CASSAVA_potential_growth_BENUE_2016.csv')
  Wlim_Ben16 <- read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_BENUE_2016.csv')
  NUlim_Ben16_NfPfKf <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_BENUE_2016_NfPfKf.csv')
  NUlim_Ben16_NOTF <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_BENUE_2016_NOTF.csv')
  
  Pot_Ben17 <- read.csv('./Results/LINTUL_CASSAVA_potential_growth_BENUE_2017.csv')
  Wlim_Ben17 <- read.csv('./Results/LINTUL_CASSAVA_water_limited_growth_BENUE_2017.csv')
  NUlim_Ben17_NfPfKf <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_BENUE_2017_NfPfKf.csv')
  NUlim_Ben17_NOTF <- read.csv('./Results/LINTUL_CASSAVA_nutrient_limited_growth_BENUE_2017_NOTF.csv')
  
  
  BIOMASS <- read.csv('./Data/Biomass_NPKuptake_plantpart_totals_per_treatment.csv', header = T) #plant parts g DM/m2
  #Change harvest day for Benue 2017
  ii <- which(BIOMASS[,"Location"] == "Benue" & BIOMASS[,"Year"] == 2017 & BIOMASS[,"time"] == 531)
  BIOMASS[ii,"time"] <- 530
  

  #===========================================================================================================
  #PLOTTING NPKI, soil nutrient, water figure
  #===========================================================================================================

  Figure1_paper <- function(){ #Edo 2016
    par(mfrow=c(2,4),oma=c(6,4,1,2),mar=c(1,1,1,1))
    NPKI(NUlim=NUlim_Edo16_NfPfKf,TITLE="NfPfKf",LEG=TRUE)
    mtext(side=2,text='Nutrition index',outer=FALSE,line=2.5)
    NPKI(NUlim=NUlim_Edo16_NfPfK2,TITLE="NfPfK180",LEG=FALSE)
    NPKI(NUlim=NUlim_Edo16_NfPfK4,TITLE="NfPfK60",LEG=FALSE)
    NPKI(NUlim=NUlim_Edo16_NOTF,TITLE="N150P40K180",LEG=FALSE)
    
    Soil_nutrients(NUlim=NUlim_Edo16_NfPfKf,TITLE="NfPfKf",LEG=TRUE)
    mtext(side=2,text=expression('Soil nutrients, kg ha'^-1),outer=FALSE,line=2.5)
    Soil_nutrients(NUlim=NUlim_Edo16_NfPfK2,TITLE="NfPfK180",LEG=FALSE)
    Soil_nutrients(NUlim=NUlim_Edo16_NfPfK4,TITLE="NfPfK60",LEG=FALSE)
    Soil_nutrients(NUlim=NUlim_Edo16_NOTF,TITLE="N150P40K180",LEG=FALSE)
    
    mtext(side=1,text="Days after planting",outer=TRUE,line=2.5)
  }
  Figure2_paper <- function(){ #CRS 2016
    par(mfrow=c(2,4),oma=c(6,4,1,2),mar=c(1,1,1,1))
    NPKI(NUlim=NUlim_CRS16_NfPfKf,TITLE="NfPfKf",LEG=TRUE)
    mtext(side=2,text='Nutrition index',outer=FALSE,line=2.5)
    NPKI(NUlim=NUlim_CRS16_NfPfK2,TITLE="NfPfK180",LEG=FALSE)
    NPKI(NUlim=NUlim_CRS16_NfPfK4,TITLE="NfPfK60",LEG=FALSE)
    NPKI(NUlim=NUlim_CRS16_NOTF,TITLE="N150P40K180",LEG=FALSE)
    
    Soil_nutrients(NUlim=NUlim_CRS16_NfPfKf,TITLE="NfPfKf",LEG=TRUE)
    mtext(side=2,text=expression('Soil nutrients, kg ha'^-1),outer=FALSE,line=2.5)
    Soil_nutrients(NUlim=NUlim_CRS16_NfPfK2,TITLE="NfPfK180",LEG=FALSE)
    Soil_nutrients(NUlim=NUlim_CRS16_NfPfK4,TITLE="NfPfK60",LEG=FALSE)
    Soil_nutrients(NUlim=NUlim_CRS16_NOTF,TITLE="N150P40K180",LEG=FALSE)
    
    mtext(side=1,text="Days after planting",outer=TRUE,line=2.5)
  }
  Figure3_paper <- function(){
    par(mfcol=c(3,3),mar=c(1,4,1,0),oma=c(3,1,1,0))
    LeavesStemRootDynamics(Series1=Pot_Edo17,Series2=Wlim_Edo17,Series3=NUlim_Edo17_NfPfKf,Series4=NUlim_Edo17_NOTF,
                           BIOMASS=subset(BIOMASS,Location == "Edo"  & Year == 2017 & Treatment == "NfPfKf"),
                           BIOMASS2=subset(BIOMASS,Location == "Edo"  & Year == 2017 & Treatment == "NOTF"),
                           TITLE="Edo 2017",LEG=TRUE, LEGTEXT = c('Potential', 'Water lim.', 'NfPfKf', 'N150P40K180','Meas. NfPfKf', 'Meas. N150P40K180'))
    par(mar=c(1,2,1,2))
    LeavesStemRootDynamics(Series1=Pot_Ben17,Series2=Wlim_Ben17,Series3=NUlim_Ben17_NfPfKf,Series4=NUlim_Ben17_NOTF,
                           BIOMASS=subset(BIOMASS,Location == "Benue" & Year == 2017 & Treatment == "NfPfKf"),
                           BIOMASS2=subset(BIOMASS,Location == "Benue" & Year == 2017 & Treatment == "NOTF"),
                           TITLE="Benue 2017",LEG=FALSE, LEGTEXT = c('Potential', 'Water lim.', 'NfPfKf', 'N150P40K180','Meas. NfPfKf', 'Meas. N150P40K180'))
    mtext(side=1,text="Days since Jan. 1 in year of planting",outer=FALSE,line=2.5)
    par(mar=c(1,0,1,4))
    LeavesStemRootDynamics(Series1=Pot_CRS17,Series2=Wlim_CRS17,Series3=NUlim_CRS17_NfPfKf,Series4=NUlim_CRS17_NOTF,
                           BIOMASS=subset(BIOMASS,Location == "CRS" & Year == 2017 & Treatment == "NfPfKf"),
                           BIOMASS2=subset(BIOMASS,Location == "CRS" & Year == 2017 & Treatment == "NOTF"),
                           TITLE="Cross River 2017",LEG=FALSE, LEGTEXT = c('Potential', 'Water lim.', 'NfPfKf', 'N150P40K180','Meas. NfPfKf', 'Meas. N150P40K180'))
  } 
  Figure_SA_paper <- function(){
    par(mfcol=c(3,3),mar=c(1,4,1,0),oma=c(3,1,1,0))
    LeavesStemRootDynamics(Series1=Pot_Edo16,Series2=Wlim_Edo16,Series3=NUlim_Edo16_NfPfKf,Series4=NUlim_Edo16_NOTF,
                           BIOMASS=subset(BIOMASS,Location == "Edo"  & Year == 2016 & Treatment == "NfPfKf"),
                           BIOMASS2=subset(BIOMASS,Location == "Edo"  & Year == 2016 & Treatment == "NOTF"),
                           TITLE="BIOMASS2 = do 2016",LEG=TRUE, LEGTEXT = c('Potential', 'Water lim.', 'NfPfKf', 'N150P40K180','Meas. NfPfKf', 'Meas. N150P40K180'))
    par(mar=c(1,2,1,2))
    LeavesStemRootDynamics(Series1=Pot_Ben16,Series2=Wlim_Ben16,Series3=NUlim_Ben16_NfPfKf,Series4=NUlim_Ben16_NOTF,
                           BIOMASS=subset(BIOMASS,Location == "Benue"  & Year == 2016 & Treatment == "NfPfKf"),
                           BIOMASS2=subset(BIOMASS,Location == "Benue"  & Year == 2016 & Treatment == "NOTF"),
                           TITLE="Benue 2016",LEG=FALSE, LEGTEXT = c('Potential', 'Water lim.', 'NfPfKf', 'N150P40K180','Meas. NfPfKf', 'Meas. N150P40K180'))
    mtext(side=1,text="Days after planting",outer=FALSE,line=2.5)
    par(mar=c(1,0,1,4))
    LeavesStemRootDynamics(Series1=Pot_CRS16,Series2=Wlim_CRS16,Series3=NUlim_CRS16_NfPfKf,Series4=NUlim_CRS16_NOTF,
                           BIOMASS=subset(BIOMASS,Location == "CRS"  & Year == 2016 & Treatment == "NfPfKf"),
                           BIOMASS2=subset(BIOMASS,Location == "CRS"  & Year == 2016 & Treatment == "NOTF"),
                           TITLE="Cross River 2016",LEG=FALSE, LEGTEXT = c('Potential', 'Water lim.', 'NfPfKf', 'N150P40K180','Meas. NfPfKf', 'Meas. N150P40K180'))
  }  
  
  
  tiff('./Figures/Figure1_paper_EDO2016.tif',
       res=600, height=20,width=30,units="cm",compression = "lzw",pointsize = 11)
    Figure1_paper()
  dev.off()
       
  tiff('./Figures/Figure2_paper_CrossRiver2016.tif',
       res=600, height=20,width=30,units="cm",compression = "lzw",pointsize = 11)
  Figure2_paper()
  dev.off()
  
  
  #Plot figures with leaves, stem and root biomass (g/m2) as function of time
  #Shows NfPfKf as Water limited and NOTH as nutrient limited
  tiff('./Figures/Figure3_paper_EDO_BENUE_CrossRiver2017.tif',
       res=600, height=20,width=25,units="cm",compression = "lzw",pointsize = 11)
    Figure3_paper()
  dev.off()
  
  
  ###FIGURE 4, MEASURED vs PREDICTED for yield, and N, P K uptakes
  tiff('./Figures/Figure4SUP_VALIDATION_EDO17_BENUE17_CRS17_selected_TREATMENTS.tif',
       res=600, height=20,width=20,units="cm",compression = "lzw",pointsize = 12)
  #Only selected to have fully independent estimates of N, P and K uptake, i.e. excluding control and omission treatments
  sub_treatments = subset(treatments, leg_text %in% c("NfPfKf","NfPfK240","NfPfK180","NfPfK120", "NfPfK60", "N150P40K180", "N75P20K90"))
  meas_vs_pred_figure(Treat = sub_treatments)
  dev.off()
  
  tiff('./Figures/Figure4_VALIDATION_EDO17_BENUE17_CRS17_ALL_TREATMENTS.tif',
       res=600, height=20,width=20,units="cm",compression = "lzw",pointsize = 12)
    meas_vs_pred_figure(Treat = treatments)
  dev.off()
  
  #Plot figures with leaves, stem and root biomass (g/m2) as function of time
  tiff('./Figures/FigureSA_paper_EDO_BENUE_CrossRiver2016.tif',
       res=600, height=20,width=20,units="cm",compression = "lzw",pointsize = 12)
    Figure_SA_paper()
  dev.off()
  