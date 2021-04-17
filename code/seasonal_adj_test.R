 data(AirPassengers)
 
AP <- AirPassengers
 
AP.decom <- decompose(AP, "multiplicative")
plot(decompose(AP, "multiplicative"))
acf(AP.decom$random[7:138])
 
plot(ts(AP))
lines(ts(AP/ (AP.decom$seasonal  )))
 
# seasonal adjust time series using X13. 
sa_series <- seas(AP)

# plot original and seasonally adjusted series.
plot(AP)
lines(final(sa_series),col=2)

dygraph(ts(data.frame(final(sa_series)  ,  (AP/ (AP.decom$seasonal  )))))
cor(final(sa_series)  ,  (AP/ (AP.decom$seasonal  )))


###################################################################################################
#Seasonal adjustment
library(readxl)
TNdata <- read_excel("C:/Users/Lenovo/Desktop/TN_JLN_R/TNdata.xlsx")

IPI= ts(TNdata[,28:83] ,frequency = 12, start = c(2000,1),end = c(2020,6))

IPI_1 =  IPI[,4]  
plot(IPI_1, type='l')

IPI_1.decom <- decompose(IPI_1, "additive") #or multiplicative
plot(IPI_1.decom)

plot(ts(IPI_1))
lines(ts(IPI_1- (IPI_1.decom$seasonal  )))

# seasonal adjust time series using X13. 
sa_series = seas(IPI_1)
sa_series1 = seas(IPI_1 , outlier = NULL) 
# plot original and seasonally adjusted series.
plot(IPI_1)
lines(final(sa_series),col=2)

dygraph(ts(data.frame(    final(sa_series) , final(sa_series1) ,  (IPI_1- (IPI_1.decom$seasonal))     )))
cor(data.frame(final(sa_series)  ,  (IPI_1- (IPI_1.decom$seasonal  )) , final(sa_series1) ))
