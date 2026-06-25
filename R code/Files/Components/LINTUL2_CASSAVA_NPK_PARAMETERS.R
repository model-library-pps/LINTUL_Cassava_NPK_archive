#----------------------------------------------------------------------------------------------------#
# FUNCTION LINTUL2_CASSAVA_NPK_PARAMETERS_ADIELE                                  
#
# Author:       AGT Schut
# Copyright:    Copyright 2019, PPS
# Email:        tom.schut@wur.nl
# Date:         18-12-2019
#
# This code lists the default parameters required to run the LINTUL-Cassava model
# 
# Parameters taken from: Adiele et al. (2019). EJA, under review.
#
#----------------------------------------------------------------------------------------------------#

#---------------------------------------------------------------------#
# FUNCTION adapted parameters                                               #
# Purpose: Listing the input parameters for Lintul2                   #
#---------------------------------------------------------------------#

LINTUL2_CASSAVA_NPK_PARAMETERS <- function(irri =TRUE, nutrient_limited = TRUE) {
  #get the defaults
  PARAM <- LINTUL2_CASSAVA_PARAMETERS_ADIELE(irri = irri) 
  
  #-------------------------------------LINTUL2_Cassava_NPK-------------------------------------------#
  # Nutrient (N/P/K) use (taken from Wolf, J. (2002), LINTUL5: Simple generic model for simulation of 
  # crop growth under potential, water limited and nitrogen, phosphorus and potassium limited conditions)
  # Derived from WOFOST41 data set published in Diepen, C.A. van, C. Rappoldt, J. Wolf & H. van Keulen, 1998. 
  # Crop growth simulation model WOFOST. Documentation version 4.1, Centre for world foord studies, Wageningen, 299 pp. 
  NPK_PARAM <- c(
    #Switch to turn nutrient limitaitons on or off
    NUTRIENT_LIMITED = nutrient_limited,

    #VERY IMPOPRTANT, If NLUE=0, no effect of nutrient stress on growth rate
    NLAI   =  0.0,     # -          : Coefficient for the reduction due to nutrient stress of the LAI increase (during juvenile phase)
    RDRNS  = 0.05,     # d-1        : maximum relative death rate of the leaves due to nutrient stress
#PARAMETERS FOR CALIBRATION
    K_MAX = 4.157277,         # Maximum value of K, a value for K_NPK_NI larger than K_MAX 
                       # will mirror the Monod function. 
                       # A value larger than 2* K_MAX is the same as a K of 0.
    K_NPK_NI = 5.885810, # K value in Monod relationship to reduce influence of slightly 
                       # lower NI value. A higher values give quicker stress.
    TSUM_NPKI = 218.232616,   #  dC, minimal TSUM: before this TSUM nutrient limitations do not reduce growth rates.

    K_WATER = 0.2,     #  K value in Monod relationship with TRANSRF to reduce uptake and mineralisation rates at drought stress

    #Measured maximum uptake rates are:
    #N: 0.21 g N m-1 d-1, P: 0.025 g m-2 d-1,  K: 0.124 g m-2 d-1
    #measured in Edo (N+P) and CRS (K), see table 2 in Adiele et al.
    #This equates to max. NEQ uptake rates of 
    #NEQ= 0.21 * 0.055 + 0.025 * 0.055/0.0044 + 0.124 * 0.055/0.021 = 0.649 g NEQ m-2 d-1
    #With soil supply in the first fase of the season for the NfPfKf (with 100, 100, 100 kg NPK with recovery of 0.7, 0.3, 0.6) 
    #of about 70 kg N ha-1, 30 kg P ha-1, 60 kg K ha-1, the slope suply with uptake then becomes
    #NEQ supply: 7 * 0.055 + 3 * 0.055/0.0044 + 6 * 0.055/0.021 =  53.6 g NEQ m-1 
    #The slope of the relationship of NEQ supply vs. NEQ uptake then becomes 0.649 / 53.6 = 0.0121   

    SLOPE_NEQ_SOILSUPPLY_NEQ_PLANTUPTAKE = 0.01207,  # d-1 nutrient equivalent uptake rate  as function of soil nutrient equivalents

    #Guestimated from figure in Adiele et al.
    FR_MAX   =  0.8,     # 0.8 -    : Optimal NPK concentration as fraction of maximum NPK concentration

    N_RECOV = 0.75, # measured values
    P_RECOV = 0.28, # measured values
    K_RECOV = 0.7,  # measured values

#END PARAMETERS FOR CALIBRATION #

    #NPART  =  1.0,     # -          : Coefficient for the effect of N stress on leaf allocation
    #NSLA   =  0.5,     # -          : Coefficient for the effect of nutrient stress on SLA reduction
    #Nutrient contents of fallen leaves 
    #Numbers from Howeler: http://ciat-library.ciat.cgiar.org/articulos_ciat/cabi_10ch7.pdf
    #Table 7.2 for fertilized conditions
    #Slight differences due to fertilization ar ignored here
    #SET TO ZERO as we do not have measured amounts in dead leaves.
    NFLVD  =  0, #30.5/1860,   # g N g-1 DM : N fraction in fallen leaves 
    PFLVD  =  0, # 2.0/1860,   # g P g-1 DM : P fraction in fallen leaves
    KFLVD  =  0, # 7.1/1860,   # g K g-1 DM : K fraction in fallen leaves
    
    

    #It takes on average TCNPKT days for nutrients to move from one organ to the other
    TCNPKT    =  10.0,      # d  : Time coefficient for NPK translocation. Must be >5 to prevent oscillating nutrient contents  between organs   

    #Fertilizer becomes available with decreasing rates: Fav=Frec*Fsupp*exp(-rFa*t) and dFav/dt=-rFa * Fa
    RTNMINF = 1 / 10,   # d-1  : relative rate of fertilizer N becoming available per day
    RTPMINF = 1 / 100,  # d-1  : relative rate of fertilizer P becoming available per day
    RTKMINF = 1 / 25    # d-1  : relative rate of fertilizer K becoming available per day 
  )
  
  # Maximum nutrient N/P/K concentration as function of development stage (kg N kg-1 DM). The development stage as it is used 
  # in LINTUL5 is converted to temperature sums using the results of Ezui et al. It is assumed that maturity (Development stage = 2)
  # is reached when the TSUMCROP = 4320 Deg. C. 
  #The values are taken from Adiele et al., from minimum and maximum values for H1, H2 and H3 data.
  #Leaves, min and max contents
  NMINMAXLV <- matrix(c( 0, 0.034,  0.055,
                      1500, 0.027, 0.049,
                      8000, 0.026, 0.059), ncol = 3, byrow=TRUE)  # g kg N kg-1 DM
  
  PMINMAXLV <- matrix(c( 0, 0.0015, 0.0044,
                      1500, 0.0014, 0.0027, 
                      8000, 0.0018, 0.0048), ncol = 3, byrow=TRUE)  # g kg P kg-1 DM
  
  KMINMAXLV <- matrix(c(  0, 0.0053, 0.0211,
                      1500, 0.0048, 0.0126,
                      8000, 0.0041, 0.0172), ncol = 3, byrow=TRUE)  # g kg K kg-1 DM
  #Stems
  NMINMAXST <- matrix(c( 0, 0.0061, 0.0116,
                      1500, 0.0055, 0.0145,
                      8000, 0.0043, 0.0131), ncol = 3, byrow=TRUE)  # g kg N kg-1 DM
  
  PMINMAXST <- matrix(c( 0, 0.00107,0.0026,
                      1500, 0.00057,0.0019, 
                      8000, 0.00038,0.0019), ncol = 3, byrow=TRUE)  # g kg P kg-1 DM
  
  KMINMAXST <- matrix(c( 0, 0.0038,0.0126,
                      1500, 0.0021,0.0081,
                      8000, 0.0015,0.0095), ncol = 3, byrow=TRUE)  # g kg K kg-1 DM

  #Storage organs
  NMINMAXSO <- matrix(c( 0, 0.0039, 0.0158,
                      1500, 0.0040, 0.0089,
                      8000, 0.0021, 0.0091), ncol = 3, byrow=TRUE)  # g kg N kg-1 DM
  
  PMINMAXSO <- matrix(c( 0, 0.00057, 0.0022,
                      1500, 0.00056, 0.0013, 
                      8000, 0.00046, 0.0014), ncol = 3, byrow=TRUE)  # g kg P kg-1 DM
  
  KMINMAXSO <- matrix(c( 0, 0.0057, 0.0125,
                      1500, 0.0025, 0.0092,
                      8000, 0.0024, 0.0100), ncol = 3, byrow=TRUE)  # g kg K kg-1 DM
  #Roots: not based on data, simply assumed 0%N, 0%P, 0%K
  #NOW SET TO VERY SMALL NUMBERS AS RECOVERY ISN'T KNOWN WHEN INCLUDING ROOTS
  NMINMAXRT <- matrix(c(  0,    0.0, 0.0,
                      1500, 0.0, 0.0,
                      8000, 0.0, 0.0), ncol = 3, byrow=TRUE)  # g kg N kg-1 DM
  
  PMINMAXRT <- matrix(c(  0,    0.0, 0.0,
                      1500, 0.0, 0.0, 
                      8000, 0.0, 0.0), ncol = 3, byrow=TRUE)  # g kg P kg-1 DM
  
  KMINMAXRT <- matrix(c(  0,    0.0, 0.0,
                      1500, 0.0, 0.0,
                      8000, 0.0, 0.0), ncol = 3, byrow=TRUE)  # g kg K kg-1 DM
  
  return(c(PARAM, NPK_PARAM, 
           list( NMINMAXLV = NMINMAXLV,
                 PMINMAXLV = PMINMAXLV,
                 KMINMAXLV = KMINMAXLV,
                 NMINMAXST = NMINMAXST,
                 PMINMAXST = PMINMAXST,
                 KMINMAXST = KMINMAXST,
                 NMINMAXSO = NMINMAXSO,
                 PMINMAXSO = PMINMAXSO,
                 KMINMAXSO = KMINMAXSO,
                 NMINMAXRT = NMINMAXRT,
                 PMINMAXRT = PMINMAXRT,
                 KMINMAXRT = KMINMAXRT)))
}

#
# par(mfrow=c(2,3),mar=c(4,5,1,0),oma=c(1,1,1,1))
# xx=seq(0,8000,1)
# plot(x =xx,100 * approx(NMINMAXLV[,1],NMINMAXLV[,3], xx)$y,xlab=expression("TSUM, dC"^o),ylab="%N",ylim=c(0,6),main="N",type="l",col="green",lwd=2,cex.lab=1.5)
# lines(x=xx,100 * approx(NMINMAXLV[,1],NMINMAXLV[,2], xx)$y,col="green",lwd=2)
# lines(x=xx,100 * approx(NMINMAXLV[,1],NMINMAXST[,3], xx)$y,col="red",lwd=2,lty=2)
# lines(x=xx,100 * approx(NMINMAXLV[,1],NMINMAXST[,2], xx)$y,col="red",lwd=2,lty=2)
# lines(x=xx,100 * approx(NMINMAXLV[,1],NMINMAXSO[,3], xx)$y,col="black",lwd=3,lty=4)
# lines(x=xx,100 * approx(NMINMAXLV[,1],NMINMAXSO[,2], xx)$y,col="black",lwd=3,lty=4)
# 
# plot(x =xx,100 * approx(PMINMAXLV[,1],PMINMAXLV[,3], xx)$y,xlab=expression("TSUM, dC"^o),ylab="%P",ylim=c(0,0.6),main="P",type="l",col="green",lwd=2,cex.lab=1.5)
# lines(x=xx,100 * approx(PMINMAXLV[,1],PMINMAXLV[,2], xx)$y,col="green",lwd=2)
# lines(x=xx,100 * approx(PMINMAXLV[,1],PMINMAXST[,3], xx)$y,col="red",lwd=2,lty=2)
# lines(x=xx,100 * approx(PMINMAXLV[,1],PMINMAXST[,2], xx)$y,col="red",lwd=2,lty=2)
# lines(x=xx,100 * approx(PMINMAXLV[,1],PMINMAXSO[,3], xx)$y,col="black",lwd=3,lty=4)
# lines(x=xx,100 * approx(PMINMAXLV[,1],PMINMAXSO[,2], xx)$y,col="black",lwd=3,lty=4)
# legend("topleft",legend=c("leaves","Stems","Storage roots"),col=c("green","red","black"),lty=c(1,2,4),bty="n")
# 
# plot(x =xx,100 * approx(KMINMAXLV[,1],KMINMAXLV[,3], xx)$y,xlab=expression("TSUM, dC"^o),ylab="%K",ylim=c(0,2.5),main="K",type="l",col="green",lwd=2,cex.lab=1.5)
# lines(x=xx,100 * approx(KMINMAXLV[,1],KMINMAXLV[,2], xx)$y,col="green",lwd=2)
# lines(x=xx,100 * approx(KMINMAXLV[,1],KMINMAXST[,3], xx)$y,col="red",lwd=2,lty=2)
# lines(x=xx,100 * approx(KMINMAXLV[,1],KMINMAXST[,2], xx)$y,col="red",lwd=2,lty=2)
# lines(x=xx,100 * approx(KMINMAXLV[,1],KMINMAXSO[,3], xx)$y,col="black",lwd=3,lty=4)
# lines(x=xx,100 * approx(KMINMAXLV[,1],KMINMAXSO[,2], xx)$y,col="black",lwd=3,lty=4)
# 
# 




