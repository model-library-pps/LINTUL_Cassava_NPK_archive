#-------------------------------------------------------------------------------------------------#
# Plotting HI
#
# Author:       AGT Schut
# Copyright:    Copyright 2020, PPS
# Email:        tom.schut@wur.nl
# Date:         12-06-2020
#

# GENERAL SETTINGS
if(length(dev.list()) > 1){
  for(i in 1:length(dev.list())){dev.off()}
}

rm(list=ls())
# install.packages('deSolve')   # uncomment if the deSolve package is not yet installed

  BIOMASS <- read.csv('./Data/Biomass_NPKuptake_plantpart_totals_per_treatment.csv', header = T) #plant parts g DM/m2
  BIOMASS[,"HI_DM"] <- BIOMASS[,"DMRoots.g.DM.m2"]/(BIOMASS[,"DMLeaves.g.DM.m2"]+BIOMASS[,"DMstems.g.DM.m2"]+BIOMASS[,"DMRoots.g.DM.m2"])
  BIOMASS[,"HI_N"] <- BIOMASS[,"NUP_SO.g.m2"]/(BIOMASS[,"NUP_L.g.m2"]+BIOMASS[,"NUP_ST.g.m2"]+BIOMASS[,"NUP_SO.g.m2"])
  BIOMASS[,"HI_P"] <- BIOMASS[,"PUP_SO.g.m2"]/(BIOMASS[,"PUP_L.g.m2"]+BIOMASS[,"PUP_ST.g.m2"]+BIOMASS[,"PUP_SO.g.m2"])
  BIOMASS[,"HI_K"] <- BIOMASS[,"KUP_SO.g.m2"]/(BIOMASS[,"KUP_L.g.m2"]+BIOMASS[,"KUP_ST.g.m2"]+BIOMASS[,"KUP_SO.g.m2"])
  
  BIOMASS[,"yLYTH"] <- paste0(BIOMASS[,"Year"],"_",BIOMASS[,"Location"],"_",BIOMASS[,"Treatment"],"_",BIOMASS[,"HarvestNumber"])
  BIOMASS[,"yLYH"] <- paste0(BIOMASS[,"Year"],"_",BIOMASS[,"Location"],"_",BIOMASS[,"HarvestNumber"])
  
  1000 * BIOMASS[,"NUP_SO.g.m2"] / BIOMASS[,"DMRoots.g.DM.m2"]
  

  
  #######

  leg_text =     c("NfPfKf",      "NfPfK240",  "NfPfK180",  "NfPfK120", "NfPfK60",  "N150P40K180",   "N75P20K90",  "N0P0K0",      "N0PfKf",    "NfP0Kf",     "NfPfK0")
  cols =         c(rgb(0,0.2,0),rgb(0,0.4,0),rgb(0, 0.6,0),rgb(0,0.8,0),rgb(0,1,0),rgb(0.4,0,0.4),rgb(0.6,0,0.6),rgb(1,0,0),rgb(0.4,0.4,0),rgb(0,0,0.6),rgb(0,0.4,0.4))
  ltys =         c(         1,             1,           1,            1,         2,            2,              1,         3,             3,           3,            3 )
  lwds =         c(         3,             1,           1,            1,         3,            3,              1,         3,             1,           1,            1 )
  pchs =         c(        21,            21,          21,           21,        21,           22,             22,        22,             23,         24,           25 )
  cexs =         c(       2.5,           2.0,         1.5,          1.0,      0.75,          2.5,            1.5,       1.5,           1.5,         1.5,          1.5 )
  treatm_names = c("NfPfKf",        "NfPfK1",    "NfPfK2",     "NfPfK3",  "NfPfK4",       "NOTF",         "NOTH", "Control",      "N0PfKf",    "NfP0Kf",      "NfPfK0")
  
  location   = c("Edo",    "CRS", "Benue", "Edo",    "CRS", "Benue")
  year       = c(2017,     2017,    2017,   2016,     2016,    2016)
  bg_loc     = c("black",  "blue",   "red", "grey",  "cyan", "orange") 


  #Change harvest day for Benue 2017
  ii <- which(BIOMASS[,"Location"] == "Benue" & BIOMASS[,"Year"] == 2017 & BIOMASS[,"time"] == 531)
  BIOMASS[ii,"time"] <- 530

  location   = c("Edo",    "CRS", "Edo",    "CRS")
  year       = c(2017,     2017,    2016,     2016)
  bg_loc     = c("black",  "blue",   "grey",  "cyan") 
  
  XY = NULL
  for (locyr in 1:length(location)) {
    for (trt in treatm_names ) {
      #Read in the data needed
      filename = paste0("./Results/LINTUL_CASSAVA_nutrient_limited_growth_",
                        location[locyr],"_",year[locyr],"_",trt,".csv")
      print(paste0("adding...",filename))
      
      NUlim <- read.csv(filename)
      
      ii <- which(BIOMASS[,"Location"] == location[locyr] 
                  & BIOMASS[,"Year"] == year[locyr]
                  & BIOMASS[,"Treatment"] == trt)
      
      #select last harvest only
      harvNUlim <- subset(NUlim,time %in% BIOMASS[ii,"time"])
      #remove rows if harvest date is not matching....
      harvBIOM <-subset(BIOMASS[ii,],time %in% harvNUlim[,"time"])
      #Use same method to determine HI, so without fine roots
      harvNUlim[,"HI"]<-harvNUlim[,"WSO"] /(harvNUlim[,"WLV"]+harvNUlim[,"WST"]+harvNUlim[,"WSO"])
      harvNUlim[,"HI_N"]<-harvNUlim[,"ANSO"] /(harvNUlim[,"ANLVG"]+harvNUlim[,"ANST"]+harvNUlim[,"ANSO"])
      harvNUlim[,"HI_P"]<-harvNUlim[,"APSO"] /(harvNUlim[,"APLVG"]+harvNUlim[,"APST"]+harvNUlim[,"APSO"])
      harvNUlim[,"HI_K"]<-harvNUlim[,"AKSO"] /(harvNUlim[,"AKLVG"]+harvNUlim[,"AKST"]+harvNUlim[,"AKSO"])
      harvNUlim[,"Biomass.g.DM.m2"]<- harvNUlim[,"WLV"] + harvNUlim[,"WST"] + harvNUlim[,"WSO"]
      
      xy = data.frame(location = location[locyr],
                      year = year[locyr],
                      treatment = trt,
                      time = harvNUlim[,"time"],
                      meas_WST.t.ha = 0.01 * harvBIOM[,"DMstems.g.DM.m2"],
                      meas_WLV.t.ha = 0.01 * harvBIOM[,"DMLeaves.g.DM.m2"],
                      meas_yield.t.ha = 0.01 * harvBIOM[,"DMRoots.g.DM.m2"],
                      meas_biom.t.ha = 0.01 * harvBIOM[,"Biomass.g.DM.m2"],
                      meas_HI_DM = harvBIOM[,"HI_DM"], 
                      meas_HI_N = harvBIOM[,"HI_N"], 
                      meas_HI_P = harvBIOM[,"HI_P"], 
                      meas_HI_K = harvBIOM[,"HI_K"], 
                      mod_WST.t.ha = 0.01 * harvNUlim[,"WST"],
                      mod_WLV.t.ha = 0.01 * harvNUlim[,"WLVG"],
                      mod_yield.t.ha = 0.01 * harvNUlim[,"WSO"],
                      mod_biom.t.ha = 0.01 * (harvNUlim[,"WST"]+harvNUlim[,"WRT"]+harvNUlim[,"WSO"]+harvNUlim[,"WLV"]),
                      mod_HI_DM = harvNUlim[,"HI"],
                      mod_HI_N = harvNUlim[,"HI_N"],
                      mod_HI_P = harvNUlim[,"HI_P"],
                      mod_HI_K = harvNUlim[,"HI_K"])

      XY = rbind(XY, xy)
    }
  }   
  
  
#######biomass and components per treatment 
meas_vs_pred_biom_figure <- function(){
    
    par(mfrow=c(2,2),mar=c(1,1,2,1),oma=c(4,4,1,1))
    
    plot(c(-100,100),c(-100,100),type="l",col="black", lwd=2, xlim=c(0,50),ylim=c(0,50), xlab="Measured, t DM/ha", ylab="Modelled, t DM/ha", main="Biomass, t/ha" )
    for (nr in 1:length(treatm_names)){
      ii <- which(XY[,"treatment"] == treatm_names[nr] )
      points(XY[ii,"meas_biom.t.ha"], XY[ii,"mod_biom.t.ha"], pch = pchs[nr], col=cols[nr])
    }    
    fit<-lm(mod_biom.t.ha ~ -1 + meas_biom.t.ha, XY)
    sfit<-summary(fit)
    #lines(c(0,100),c(sfit$coefficients[1], sfit$coefficients[1] + 100 * sfit$coefficients[2]),col="black",lwd=2,lty=2)
    #text(25, 50, labels=paste0("R2= " , round(sfit$r.squared,2),", slope = ", round(sfit$coefficients[2],3)))
    lines(c(0,100),c(0, 100 * sfit$coefficients[1]),col="black",lwd=2,lty=2)
    text(25, 50, labels=paste0("R2= " , round(sfit$r.squared,2),", slope = ", round(sfit$coefficients[1],3)))

    plot(c(-100,100),c(-100,100),type="l",col="black", lwd=2, xlim=c(0,30),ylim=c(0,30), 
         xlab="Measured, t DM/ha", ylab="Modelled, t DM/ha", main="Storage roots, t/ha" )
    for (nr in 1:length(treatm_names)){
      ii <- which(XY[,"treatment"] == treatm_names[nr] )
      points(XY[ii,"meas_yield.t.ha"], XY[ii,"mod_yield.t.ha"], pch = pchs[nr], col=cols[nr])
    }
    fit<-lm(mod_yield.t.ha ~ -1 + meas_yield.t.ha, XY)
    sfit<-summary(fit)
    #lines(c(0,100),c(sfit$coefficients[1], sfit$coefficients[1] + 100 * sfit$coefficients[2]),col="black",lwd=2,lty=2)
    #text(15, 30, labels=paste0("R2= " , round(sfit$r.squared,2),", slope = ", round(sfit$coefficients[2],3)))
    lines(c(0,100),c(0, 100 * sfit$coefficients[1]),col="black",lwd=2,lty=2)
    text(15, 30, labels=paste0("R2= " , round(sfit$r.squared,2),", slope = ", round(sfit$coefficients[1],3)))
    

    plot(c(-100,100),c(-100,100),type="l",col="black", lwd=2, 
         xlim=c(0,20),ylim=c(0,20), xlab="Measured, t/ha", ylab="Modelled, t/ha", main="Stems, t/ha" )
    for (nr in 1:length(treatm_names)){
      ii <- which(XY[,"treatment"] == treatm_names[nr] )
      points(XY[ii,"meas_WST.t.ha"], XY[ii,"mod_WST.t.ha"], pch = pchs[nr], col=cols[nr])
    }
    fit<-lm(mod_WST.t.ha ~ -1 + meas_WST.t.ha, XY)
    sfit<-summary(fit)
    #lines(c(0,100),c(sfit$coefficients[1], sfit$coefficients[1] + 100 * sfit$coefficients[2]),col="black",lwd=2,lty=2)
    #text(10, 20, labels=paste0("R2= " , round(sfit$r.squared,2),", slope = ", round(sfit$coefficients[2],3)))
    lines(c(0,100),c(0, 100 * sfit$coefficients[1]),col="black",lwd=2,lty=2)
    text(10, 20, labels=paste0("R2= " , round(sfit$r.squared,2),", slope = ", round(sfit$coefficients[1],3)))
    
    # plot(c(-100,100),c(-100,100),type="l",col="black", lwd=2, 
    #      xlim=c(0,4),ylim=c(0,4), xlab="Measured, t/ha", ylab="Modelled, t/ha", main="Leaves, t/ha"  )
    # for (nr in 1:length(treatm_names)){
    #   ii <- which(XY[,"treatment"] == treatm_names[nr] )
    #   points(XY[ii,"meas_WLV.t.ha"], XY[ii,"mod_WLV.t.ha"], pch = pchs[nr], col=cols[nr])
    # }
    # fit<-lm(mod_WLV.t.ha ~ -1 + meas_WLV.t.ha, XY)
    # sfit<-summary(fit)
    # #lines(c(0,100),c(sfit$coefficients[1], sfit$coefficients[1] + 100 * sfit$coefficients[2]),col="black",lwd=2,lty=2)
    # #text(7, 25, labels=paste0("R2= " , round(sfit$r.squared,2),", slope = ", round(sfit$coefficients[2],3)))
    # lines(c(0,100),c(0, 100 * sfit$coefficients[1]),col="black",lwd=2,lty=2)
    # text(7, 25, labels=paste0("R2= " , round(sfit$r.squared,2),", slope = ", round(sfit$coefficients[1],3)))

    plot(c(-100,-10),c(-100,-10),type="l",col="black", 
         lwd=2, xlim=c(0,1),ylim=c(0,1),yaxt="n",xaxt="n")
    legend("topleft",legend=leg_text,col=cols,pch=pchs, ncol = 2, bty="n")
    mtext("Modelled",side = 2,line=1,outer=TRUE,adj=0.5,cex=1.5)
    mtext("Measured",side = 1,line=1,outer=TRUE,adj=0.5,cex=1.5)
    
}

meas_vs_pred_HI_figure <- function(){
  
  par(mfrow=c(2,2),mar=c(1,1,2,1),oma=c(4,4,1,1))
  
  plot(c(-100,100),c(-100,100),type="l",col="black", lwd=2,
       xlim=c(0.1,0.8),ylim=c(0.1,0.8), xlab="Measured, HI", ylab="Modelled, HI", main="DM"  )
  for (nr in 1:length(treatm_names)){
    ii <- which(XY[,"treatment"] == treatm_names[nr] )
    points(XY[ii,"meas_HI_DM"], XY[ii,"mod_HI_DM"], pch = pchs[nr], col=cols[nr])
  }
  fit<-lm(mod_HI_DM ~ -1 + meas_HI_DM, XY)
  sfit<-summary(fit)
  lines(c(0,100),c(0, 100 * sfit$coefficients[1]),col="black",lwd=2,lty=2)
  text(0.4, 0.75, labels=paste0("R2= " , round(sfit$r.squared,2),", slope = ", round(sfit$coefficients[1],3)))

  plot(c(-100,100),c(-100,100),type="l",col="black", lwd=2,
       xlim=c(0.1,0.8),ylim=c(0.1,0.8), xlab="Measured, HI", ylab="Modelled, HI", main="N"  )
  for (nr in 1:length(treatm_names)){
    ii <- which(XY[,"treatment"] == treatm_names[nr] )
    points(XY[ii,"meas_HI_N"], XY[ii,"mod_HI_N"], pch = pchs[nr], col=cols[nr])
  }
  fit<-lm(mod_HI_N ~ -1 + meas_HI_N, XY)
  sfit<-summary(fit)
  lines(c(0,100),c(0, 100 * sfit$coefficients[1]),col="black",lwd=2,lty=2)
  text(0.4, 0.75, labels=paste0("R2= " , round(sfit$r.squared,2),", slope = ", round(sfit$coefficients[1],3)))

  plot(c(-100,100),c(-100,100),type="l",col="black", lwd=2,
       xlim=c(0.1,0.8),ylim=c(0.1,0.8), xlab="Measured, HI", ylab="Modelled, HI", main="P"  )
  for (nr in 1:length(treatm_names)){
    ii <- which(XY[,"treatment"] == treatm_names[nr] )
    points(XY[ii,"meas_HI_P"], XY[ii,"mod_HI_P"], pch = pchs[nr], col=cols[nr])
  }
  fit<-lm(mod_HI_P ~ -1 + meas_HI_P, XY)
  sfit<-summary(fit)
  lines(c(0,100),c(0, 100 * sfit$coefficients[1]),col="black",lwd=2,lty=2)
  text(0.4, 0.75, labels=paste0("R2= " , round(sfit$r.squared,2),", slope = ", round(sfit$coefficients[1],3)))
  
  plot(c(-100,100),c(-100,100),type="l",col="black", lwd=2,
       xlim=c(0.1,0.8),ylim=c(0.1,0.8), xlab="Measured, HI", ylab="Modelled, HI", main="K"  )
  for (nr in 1:length(treatm_names)){
    ii <- which(XY[,"treatment"] == treatm_names[nr] )
    points(XY[ii,"meas_HI_K"], XY[ii,"mod_HI_K"], pch = pchs[nr], col=cols[nr])
  }
  fit<-lm(mod_HI_K ~ -1 + meas_HI_K, XY)
  sfit<-summary(fit)
  lines(c(0,100),c(0, 100 * sfit$coefficients[1]),col="black",lwd=2,lty=2)
  text(0.4, 0.75, labels=paste0("R2= " , round(sfit$r.squared,2),", slope = ", round(sfit$coefficients[1],3)))
  mtext("Modelled HI",side = 2,line=1.5,outer=TRUE,adj=0.5,cex=1.5)
  mtext("Measured HI",side = 1,line=1.5,outer=TRUE,adj=0.5,cex=1.5)
  
}

meas_vs_pred_biom_figure()
  
pdf('./Figures/Meas vs modelled for biomass, storage roots, stems.pdf')
  meas_vs_pred_biom_figure()
dev.off()
  
meas_vs_pred_HI_figure()

pdf('./Figures/Meas vs modelled for HI for Dm, N, P and K.pdf')
meas_vs_pred_HI_figure()
dev.off()
