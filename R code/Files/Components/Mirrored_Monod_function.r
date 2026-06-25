
Mirrored_Monod <- function(x, K, Kmax = 4){
  if(K <= Kmax){
    C=K+1
    y=ifelse(x == 0, 0, C * x/(x+K))
  }else{
    K=max(0, 2 * Kmax - K)
    C=K+1
    x=1-x
    y=ifelse(x == 0, 0, C * x/(x+K))
    y=1-y
  }
}

# tiff('../Figures/Fig S9. Mirrored_Monod.tif',
#      res=600, height=20,width=30,units="cm",compression = "lzw",pointsize = 11)
#   par(oma=c(2,2,2,2),mar=c(4,5,2,0))
#   Kmax= 4
#   xx=seq(0,1,0.01)
#   plot(xx,xx,type="l",ylim=c(0,1),col="black",lty=2, lwd=2, cex.lab=2,
#        xlab="NIN*NIP*NIK",ylab="NPKI", main="Calibrated")
#   kk= seq(0.05, 2 * Kmax, 0.2)
#   cols=rainbow(2*length(kk))
#   nr=0
#   for(K in kk){
#     nr=nr+1
#     lines(xx,Mirrored_Monod(xx,K, Kmax),col=cols[nr])
#   }
#   #Calibrated values
#   lines(xx,Mirrored_Monod(xx,K = 5.885810, Kmax = 4.157277),col="black",lwd=4,lty=1)
# dev.off()
# 



