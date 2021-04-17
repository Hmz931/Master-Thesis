library(tempdisagg)
library(dygraphs)
library(readxl)
library(BCDating)
library(seasonal)

# Monthly Tunisian Dataset from 2000-01 to 2020-06
TNdata = data.frame(read_excel("C:/Users/Lenovo/Desktop/TN_JLN_R/TNdata.xlsx"))
datatime = TNdata$Date
TNdata = TNdata[,2:dim(TNdata)[2]] #Data without date column
name = names(TNdata)

TNdata <- ts(TNdata ,frequency = 12, start = c(2000,1),end = c(2020,6) ) 
IPI=TNdata[,27:83]
#Example with one serie , the global IPI index
G_IPI =  ts(IPI[7:246,1] ,frequency = 12, start = c(2000,7),end = c(2020,6) ) #7/2000--6/2020    
plot(G_IPI,type = 'l',xlab = "Time" ,ylab = name[27] )


# seasonal adjust time series using X13. 
sa_series <- seas(G_IPI)

# plot original and seasonally adjusted series.
plot(G_IPI)
lines(final(sa_series),col=2)

IPI_sa = final(sa_series)

#Harding-Pagan (Bry-Boschan trimestriel) Procédure de datation du cycle économique
library(mFilter)
LN_IPI_sa = log(IPI_sa)

hp.decom <- hpfilter(LN_IPI_sa, freq = 14400, type = "lambda")

pdf("HP Filter IPI.pdf")
par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(LN_IPI_sa, ylab = "", lwd=2 )  # plot time series
lines(hp.decom$trend, col = "red", lwd=2)  # include HP trend
legend("left", legend = c("Log IPI", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n" , lwd=2)
plot.ts(hp.decom$cycle, ylab = "", col = 'blue' , lwd=2 )  # plot cycle
legend("left", legend = c("Cycle de croissance de IPI"), lty = 1, col = c("blue"), 
       bty = "n", lwd=2)
dev.off()

library(BCDating)
#Harding-Pagan (Bry-Boschan trimestriel) Procédure de datation du cycle de croissance tunisien
#avec une durée minimale de cycle de six trimestre et l'une de ses phases (recession ou expansion)
#d'une durée minimale de 3 semestre suivant Dungey et Pagan 2000, la meme demarche de Elachhab 2007
#dans le cadre de l'etude de cycle de croissance tunisien entre 1970 et 2002 en utilisant
#l'indice de production industriel 
dat <- BBQ( hp.decom$cycle, name="Dating Business Cycles of Tunisia", mincycle = 15,minphase = 5)
show(dat)

summary(dat)
plot(dat)
par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot(dat,hp.decom$cycle , lwd=2 )
plot(dat,LN_IPI_sa , lwd=2 )


#BRY-BOSCHAN IPI Business Cycle dating using Matlab code and "cyc" as input from 7:2000--6:2020
BB.Date = data.frame(Peaks = c( "2002M3", "2004M3", "2007M6", "2010M7","2013M4","2014M11","2020M1")
                       , Troughs = c( "2003M3","2006M10","2009M3","2011M2","2014M1","2017M4", NA ),
                       Duration = c (12,31,21,7,9,29,NA )  )

library(readxl)
u01 <- read_excel("C:/Users/Lenovo/Desktop/TN_JLN_R/Tunisian_Uncertainty_Index.xlsx", 
                  sheet = "F1")
u01 = u01$u01
u01 =  ts(u01 ,frequency = 12, start = c(2000,7),end = c(2020,6) ) 

cyc = ts(hp.decom$cycle, frequency = 12, start = c(2000,7) )

pdf("contercycle.pdf")
par(mfrow = c(2, 1), mar = c(2.2, 2, 1, 1), cex = 1.2)
plot(dat,u01 , lwd=2 , col = 'blue')
abline(h = mean(u01) , col = 'black' , lty=3)
legend("topleft",                                     
       legend = c("h = 1"),
       col = c("blue"),
       lwd=2, ncol = 1,  cex = 0.72, bg='white')

plot(dat, cyc  , lwd=2 , col = 'red' )
abline(h = mean(cyc) , col = 'black' , lty=3)
legend("bottomleft",                                     
       legend = c("Cycle de croissance tunisien"),
       col = c("red"),
       lwd=2, ncol = 1,  cex = 0.72, bg='white')
dev.off()

cor.test(cyc, u01)
#Summary statistics
library(moments)
sd(hp.decom$cycle)
skewness(hp.decom$cycle)
kurtosis(hp.decom$cycle)

plot(density(scale(hp.decom$cycle)))
lines(density(rnorm(10000)))
lines(density(scale(u01)))
abline(v=mean(hp.decom$cycle))

library(writexl)
write_xlsx( data.frame(time(cyc), cyc), "data1.xlsx")


plot(cyc, type="l", panel.first = rect(c(cyc[22],cyc[150]), -1e6, c(cyc[30],cyc[190]), 1e6, col='red', border=NA))

