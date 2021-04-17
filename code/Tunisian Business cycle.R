library(readxl)
TUN_GDP <- read_excel("C:/Users/Lenovo/Desktop/TN_JLN_R/Tunisian_Uncertainty_Index.xlsx", 
                      sheet = "F6")
TUN_GDP = TUN_GDP$`PIB (aux prix du marché) aux prix de l'année 2010 en million de dinars`
LN_TUN_GDP =  ts(log(TUN_GDP[1:82]) ,frequency = 4, start = c(2000,1)  ) #1:82 ==T1 2000 T2 2020

library(mFilter)
hp.decom <- hpfilter(LN_TUN_GDP, freq = 1600, type = "lambda")
pdf("HP Filter.pdf")
par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(LN_TUN_GDP, ylab = "", lwd=2 )  # plot time series
lines(hp.decom$trend, col = "red", lwd=2)  # include HP trend
legend("topleft", legend = c("Log PIB", "HP trend"), lty = 1, 
       col = c("black", "red"), bty = "n" , lwd=2)
plot.ts(hp.decom$cycle, ylab = "", col = 'blue' , lwd=2 )  # plot cycle
legend("topleft", legend = c("Cycle de croissance"), lty = 1, col = c("blue"), 
       bty = "n", lwd=2)
dev.off()

library(BCDating)
#Harding-Pagan (Bry-Boschan trimestriel) Procédure de datation du cycle de croissance tunisien
#avec une durée minimale de cycle de six trimestre et l'une de ses phases (recession ou expansion)
#d'une durée minimale de 3 semestre suivant Dungey et Pagan 2000, la meme demarche de Elachhab 2007
#dans le cadre de l'etude de cycle de croissance tunisien entre 1970 et 2002 en utilisant
#l'indice de production industriel 
dat <- BBQ( hp.decom$cycle, name="Dating Business Cycles of Tunisia", mincycle = 6,minphase = 3)
show(dat)

summary(dat)
plot(dat)
par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot(dat,hp.decom$cycle , lwd=2 )
plot(dat,LN_TUN_GDP , lwd=2 )


library(readxl)
u01 <- read_excel("C:/Users/Lenovo/Desktop/TN_JLN_R/Tunisian_Uncertainty_Index.xlsx", 
                  sheet = "F1")
u01 = u01$u01
u01 =  ts(u01 ,frequency = 12, start = c(2000,7),end = c(2020,6) ) 
u01 = ts(aggregate(u01 ,  nfrequency = 4)/3, frequency = 4, start = c(2000,3)) #3-Months mean ==> Quarterly


par(mfrow = c(2, 1), mar = c(2.2, 8, 1, 1), cex = 1.2)
plot(dat,u01 , lwd=2 , col = 'blue')
abline(h = mean(u01) , col = 'black' , lty=3)
legend("topleft",                                     
       legend = c("h = 1"),
       col = c("blue"),
       lwd=2, ncol = 1,  cex = 0.72)
cyc = ts(hp.decom$cycle[3:82], frequency = 4, start = c(2000,3) )
plot(dat, cyc  , lwd=2 , col = 'red' )
abline(h = mean(cyc) , col = 'black' , lty=3)
legend("bottomleft",                                     
       legend = c("Cycle de croissance tunisien"),
       col = c("red"),
       lwd=2, ncol = 1,  cex = 0.72)


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
write_xlsx( data.frame(time(hp.decom$cycle), hp.decom$cycle), "data1.xlsx")

