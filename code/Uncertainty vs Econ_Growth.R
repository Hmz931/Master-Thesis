library(readxl)
TUN_GDP <- read_excel("C:/Users/Lenovo/Desktop/TN_JLN_R/Tunisian_Uncertainty_Index.xlsx", 
                                         sheet = "F6")
TUN_QEG = TUN_GDP$`Glissement annuel (T/T-4) (DonnéesCVS-CJO en %)`
TUN_QEG =  ts(TUN_QEG[5:length(TUN_QEG)] ,frequency = 4, start = c(2001,1)  ) 

library(BCDating)
#Harding-Pagan (Bry-Boschan trimestriel) Procédure de datation du cycle économique
dat <- BBQ( TUN_QEG, name="Dating Business Cycles of Tunisia")
show(dat)

summary(dat)
plot(dat)
plot(dat,TUN_QEG , lwd=3 )
plot(dat,u01 , lwd=3 )

library(readxl)
u01 <- read_excel("C:/Users/Lenovo/Desktop/TN_JLN_R/Tunisian_Uncertainty_Index.xlsx", 
                  sheet = "F1")
u01 = u01$u01
u01 =  ts(u01 ,frequency = 12, start = c(2000,7),end = c(2020,6) ) 
u01 = ts(aggregate(u01 ,  nfrequency = 4)/3, frequency = 4, start = c(2000,3)) #3-Months mean ==> Quarterly
u01 = ts(u01[3:length(u01)], frequency = 4, start = c(2001,1)) 


plot(dat,TUN_QEG , lwd=3 )
legend("bottomleft",                                     
       legend = c("Croissance économique trimestrielle"),
       col = c("red"),
       lwd=3, ncol = 1,  cex = 0.72)

plot(dat,u01 , lwd=3 , col = 'blue')
legend("topleft",                                     
       legend = c("h = 1"),
       col = c("blue"),
       lwd=3, ncol = 1,  cex = 0.72)



