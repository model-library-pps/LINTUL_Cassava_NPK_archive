#-------------------------------------------------------------------------------------------------#
# #HELP functions for calibration with the optim function
#
# Author:       AGT Schut
# Copyright:    Copyright 2020, PPS
# Email:        tom.schut@wur.nl
# Date:         31-01-2020
#
# This file contains functions to enable calibration for the LINTUL-CASSAVA_NPK model. 
# 
# 
# Calibrate_LINTUL_CASSAVA_NPK for actual calibration that minimizes the error
# fn_LINTUL_CASSAVA_NPK for computing the errors per iteration
#--------------------------------------------------------------------------------------------------#

Calibrate_LINTUL_CASSAVA_NPK <- function(wdata,  pars_list, year, starttime, endtime, 
                                         parNamesToCalibrate, StateNames, 
                                         obsData, obsStateNames,
                                         treatment_names){
  #Get the first element of the list with some parameter set
  #All treatments will have the same fixed parameters but differ in NPK supply!!
  pars = pars_list[[1]]
  
  par = NULL
  for (sel_pars in parNamesToCalibrate) {
    spar = getElement(pars, sel_pars)
    par = c(par, spar)
  }
  calibResult <- data.frame(calibrationParameters = parNamesToCalibrate,
                            origValues = par )
  
  #Set values to log scale to prevent selection of negative values
  log_par = log(par)
  
  #Optimize for all treatments with a listed parameter set
  log_par_calib = optim( par = log_par,
                         fn = fn_LINTUL_CASSAVA_NPK,
                         pars_list = pars_list,
                         wdata = wdata,  
                         year = year, starttime = starttime, endtime = endtime,
                         parNamesToCalibrate = parNamesToCalibrate,
                         selStateNames = StateNames,
                         obsData = obsData,
                         obsDataNames = obsStateNames,
                         treatment_names = treatment_names)
  #Store calibration result
  calibResult[,"calibValues"] = exp(log_par_calib$par)
  print("Calibrated parameter values")
  print(calibResult)
  print("Calibration COMPLETED")
  return(calibResult)
}

#This function is called repetively by the OPTIM procedure
#calculates error for all treatments that have observations and parameter sets included 
fn_LINTUL_CASSAVA_NPK <- function(par, pars_list, wdata,  year, starttime, endtime,
                                  parNamesToCalibrate, selStateNames, 
                                  obsData, obsDataNames,
                                  treatment_names){
  #Ensure all is back to normal scale
  par = exp(par)
  names(par) <- parNamesToCalibrate
  
  SMSE = 0
  for (treat_nr in 1:length(pars_list)) {
    paramDEF <- pars_list[[treat_nr]]
    obsData_treat <- subset(obsData, Treatment == treatment_names[treat_nr])
    #Replace calibration parameters
    for (i in 1:length(parNamesToCalibrate)) {
      paramDEF[parNamesToCalibrate[i]] = par[i]
    }
    
    states_pred <- ode(LINTUL2_CASSAVA_NPK_iniSTATES(paramDEF), 
                       seq(starttime, endtime, by = 1), 
                       LINTUL2_CASSAVA_NPK, paramDEF,  WDATA = wdata,
                       method = "euler")
    states_pred <- as.data.frame(states_pred)
    
    #Determine total predicted uptake
    states_pred[,"ANUP"]<- states_pred[,"ANLVG"]+states_pred[,"ANLVD"]+states_pred[,"ANST"]+states_pred[,"ANSO"]+states_pred[,"ANRT"]
    states_pred[,"APUP"]<- states_pred[,"APLVG"]+states_pred[,"APLVD"]+states_pred[,"APST"]+states_pred[,"APSO"]+states_pred[,"APRT"]
    states_pred[,"AKUP"]<- states_pred[,"AKLVG"]+states_pred[,"AKLVD"]+states_pred[,"AKST"]+states_pred[,"AKSO"]+states_pred[,"AKRT"]
    
    #Select specific rows: returns dataframe with selected column for times matching observations
    sel_state_pred = subset(states_pred,
                            time %in% obsData_treat[,"time"],
                            select = selStateNames)
    
    #Compute relative error for every selected state variable and add tot the total
    for (i in 1:length(obsDataNames) ) {
      yres = (obsData_treat[,obsDataNames[i]] -  sel_state_pred[,selStateNames[i]]) / mean(obsData_treat[,obsDataNames[i]])
      SMSE = SMSE + mean(yres^2)
    }
  }
  print( c(RMSRE = sqrt(SMSE), par = par))
  #Return sum of mean squared relative errors
  return(SMSE = SMSE)  
}
