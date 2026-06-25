#------------------------------------------------------------------------------------------------------#
# FUNCTION LINTUL_CASSAVA_NPK_RUN  
#
# Author:       AGT Schut
# Copyright:    Copyright 2019, PPS
# Email:        tom.schut@wur.nl
#
# This file contains a help function of the LINTUL-CASSAVA_NPK model. The LINTUL-CASSAVA_RUN
# function is running the full LINTUL2-CASSAVA script with all its components. 
#
#------------------------------------------------------------------------------------------------------#

#LINTUL:
LINTUL2_CASSAVA_NPK_RUN <- function(wdata, pars, year, starttime, endtime){
  DELT <- as.numeric(pars[which(names(pars)=='DELT')])
  
  state_res <- ode(LINTUL2_CASSAVA_NPK_iniSTATES(pars), 
                    seq(starttime, endtime, by = DELT), 
                    LINTUL2_CASSAVA_NPK, pars,  WDATA = wdata,
                    method = "euler")
  state_res = data.frame(state_res)
  
  year_info = data.frame(year_planting = rep(year,nrow(state_res)),
                         year = rep(as.numeric(year), nrow(state_res)),
                         DOY = state_res[,'time'])
  
  if (as.numeric(year)%%4 == 0 & endtime > 366){
    ii<-which(year_info[,'DOY'] > 366)
    year_info[ii,'year'] <- year_info[ii,'year'] + 1
    year_info[ii,'DOY'] <- year_info[ii,'DOY'] - 366
  } else if(endtime > 365){
    ii<-which(year_info[,'DOY'] > 365)
    year_info[ii,'DOY'] <- year_info[ii,'DOY'] - 365
    year_info[ii,'year'] <- year_info[ii,'year'] + 1
  }
  return(Modelresults = cbind(year_info, state_res))
}