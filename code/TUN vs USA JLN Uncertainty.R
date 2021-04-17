library(readxl)
Uncertainty_Index <- read_excel("Tunisian_Uncertainty_Index.xlsx", 
                                sheet = "F9", range = "B2:G242")
Uncertainty_Index = ts(Uncertainty_Index , frequency = 12 , start = c(2000,7))

par(mfrow=c(1,3))

boxplot(Uncertainty_Index[,1],Uncertainty_Index[,4],main="Comparaison pour h = 1",las=1,horizontal=FALSE
          ,col=c("violet","turquoise")
          ,names =c("TUN h = 1","USA h = 1"))

boxplot(Uncertainty_Index[,2],Uncertainty_Index[,5],main="Comparaison pour h = 3",las=1,horizontal=FALSE
        ,col=c("violet","turquoise")
        ,names =c("TUN h = 3","USA h = 3"))

boxplot(Uncertainty_Index[,3],Uncertainty_Index[,6],main="Comparaison pour h = 12",las=1,horizontal=FALSE
        ,col=c("violet","turquoise")
        ,names =c("TUN h = 12","USA h = 12"))


par(mfrow=c(1,2))
boxplot(Uncertainty_Index[,1],main="Comparaison pour h = 1",las=1,horizontal=FALSE
        ,col=c("violet","turquoise")
        ,names =c("TUN h = 1" ))
f = fivenum(Uncertainty_Index[,1])
text(rep(1.25, 5), f, labels = c("minimum", "lower", "median", "upper", "maximum"), 
     adj = c(0, 0), cex = 0.75)
boxplot(Uncertainty_Index[,4],main="Comparaison pour h = 1",las=1,horizontal=FALSE
        ,col=c("violet","turquoise")
        ,names =c( "USA h = 1"))
f = fivenum(Uncertainty_Index[,4])
text(rep(1.25, 5), f, labels = c("minimum", "lower", "median", "upper", "maximum"), 
     adj = c(0, 0), cex = 0.75)



boxplot(Uncertainty_Index[,1])
f = fivenum(Uncertainty_Index[,1])
text(rep(1.25, 5), f, labels = c("minimum", "lower", "median", "upper", "maximum"), 
     adj = c(0, 0), cex = 0.75)
points(mean(Uncertainty_Index[,1]), col = "red", pch = '-', cex = 1.5)


#summary give [Minimum] [1st Quartile] [Median] [Mean] [3rd Quartile] [Maximum]
#fivenum and boxplot functions give the Tukey statistics :
#[Minimum] [Lower hinge : median of fisrt half less than the median]
#[Median] [Upper hinge : median of fisrt half more than the median]] [Maximum]

cor(Uncertainty_Index)
par(mfrow=c(1,3))
pdf("TNJLN.pdf")
par(cex=0.3, mar=c(5, 4, 3, 0) + 0.1, mfrow=c(1, 3)) #sets the bottom, left, top and right margins respectively of the plot region in number of lines of text.
plot( Uncertainty_Index[,4] ,type = "l" , col = "green",  xlab = "",ylab = "Incertitude", lwd=2)
lines( Uncertainty_Index[,1] ,type = "l", col = "blue"  , lwd=2 )
legend("topleft",                                     
       legend = c( "USA h = 1","TUN h = 1"),
       col = c("green", "blue"),
       lwd=3, ncol = 1,  cex = 0.92, 
       title="Correlation = 0.4 ",text.font=2,bty="n")

plot( Uncertainty_Index[,5] ,type = "l" , col = "green",  xlab = "",ylab = "Incertitude",lwd=2)
lines( Uncertainty_Index[,2] ,type = "l", col = "black"  , lwd=2 )
legend("topleft",                                     
       legend = c("USA h = 3", "TUN h = 3"),
       col = c("green", "black"),
       lwd=3, ncol = 1,  cex = 0.92,
       title="Correlation = 0.3 " ,text.font=2,bty="n")

plot( Uncertainty_Index[,6] ,type = "l" , col = "green",  xlab = "",ylab = "Incertitude", lwd=2)
lines( Uncertainty_Index[,3] ,type = "l", col = "red"  , lwd=2 )
legend("topleft",                                     
       legend = c("USA h = 12", "TUN h = 12"),
       col = c("green", "red"),
       lwd=3, ncol = 1,  cex = 0.9, 
       title="Correlation = 0.16 ",text.font=2,bty="n")

dev.off()
#autocorrelation (and partial autocorrelation) and stationary test of Tunisian uncertainty index  
## correlogram of the Tunisian uncertainty index
u_acf = acf(Uncertainty_Index[,1], lag.max = 36)
u_pacf = pacf(Uncertainty_Index[,1], lag.max = 36)

library(tseries)
#unit root adf test of stationarity of the index
adf.test(Uncertainty_Index[,1])         #Not stationary
adf.test(diff( Uncertainty_Index[,1]))  #Stationary

library(dygraphs)
dygraph(cbind( Uncertainty_Index[2:length(Uncertainty_Index[,1]),1] ,  diff( Uncertainty_Index[,1]) ))

library(moments)
skewness(Uncertainty_Index[,1])
kurtosis(Uncertainty_Index[,1])

plot(density(Uncertainty_Index[,1]))
lines( density( rnorm(1000000,mean(Uncertainty_Index[,1]) ,sd(Uncertainty_Index[,1]) )))

#Normality test : Jarque-Bera Normality Test H0 : Normal Distribution
jarque.test(c(Uncertainty_Index[,1]))

#Normality test : Shapiro-Wilk Normality Test H0 : Normal Distribution
shapiro.test(c(Uncertainty_Index[,1]))

