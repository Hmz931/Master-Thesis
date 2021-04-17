load("ut.RData")

load("utfullcsa.Rdata")
load("utfull.Rdata")
load("utarcsa.Rdata")
load("utar.Rdata")

#Aggregated incertainty
library(dygraphs)
utdata = data.frame(utcsa[,1],utarcsa[,1],utfullcsa[,1]) #h = 1
utdata =ts(utdata ,   frequency = 12, start=c(2000,7), end=c(2020,6))
dygraph(utdata)


#Individual incertainty
library(dygraphs)
utdata = data.frame(ut[,1,1],utar[,1,1],utfull[,1,1]) #h = 1 and first variable
utdata =ts(utdata ,   frequency = 12, start=c(2000,7), end=c(2020,6))
dygraph(utdata)



ut =ts(ut[,,1] ,   frequency = 12, start=c(2000,7), end=c(2020,6))#h = 1
utar =ts(utar[,,1] ,   frequency = 12, start=c(2000,7), end=c(2020,6))#h = 1
utfull =ts(utfull[,,1] ,   frequency = 12, start=c(2000,7), end=c(2020,6))#h = 1

pdf("VariblesU.pdf")
par(mfrow=c(3,2))

#Individual incertainty of IPC
plot(ut[,88],type = "l", col = "black",  xlab = "",    ylab = "Incertitude",
     main = "IPC"  )  
lines(utar[,88], col = "blue", lwd=2)

legend("topleft",                                     
       legend = c("Aucun Predicteur", "Predicteurs Siginficatifs"),
       col = c("blue", "black"),
       lwd=2, ncol = 1,  cex = 0.72)
#Individual incertainty of IPI
plot(ut[,28],type = "l", col = "black",  xlab = "",    ylab = "Incertitude",
     main = "IPI"  ) 
lines(utar[,28],col = "blue", lwd=2)

#Individual incertainty of Tunindex
plot(ut[,15],type = "l", col = "black",  xlab = "",    ylab = "Incertitude",
     main = "Tunindex"  )  
lines(utar[,15], col = "blue", lwd=2)

#Individual incertainty of Indice des prix à l'import
plot(ut[,91],type = "l", col = "black",  xlab = "",    ylab = "Incertitude",
     main = "Indice des prix à l'import"  ) 
lines(utar[,91],col = "blue", lwd=2)

#Individual incertainty of Indice des prix à l'export
plot(ut[,95],type = "l", col = "black",  xlab = "",    ylab = "Incertitude",
     main = "Indice des prix à l'export"  ) 
lines(utar[,95],col = "blue", lwd=2)

#Individual incertainty of M3
plot(utar[,2],type = "l", col = "black",  xlab = "",    ylab = "Incertitude",
     main = "M3"  , lwd=1) 
lines(ut[,2], col = "blue",lwd=2)




dev.off()

