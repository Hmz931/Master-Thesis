#Disaggregate annual Unemployment rate from 2000 to 2019 and quarter Unemployment 
#from Q4-2011 to Q2-2020

library(tempdisagg)
library(dygraphs)
library(readxl)
Annual <- read_excel("TNdata.xlsx", sheet = "Annual_Unemloyment_ILO", 
                     range = "B1:B21")

Quarter <- read_excel("TNdata.xlsx", sheet = "Quarter_Unemloyment_INS", 
                      range = "B1:B36")

Annual=ts(Annual,frequency = 1, start = c(2000),end = c(2019))
Annual_M=td(Annual~1 , to = "monthly" , method = "denton-cholette",conversion = "average")
dygraph(predict(Annual_M))
Annual_A_Predict=predict(Annual_M)
length(Annual_A_Predict)

Quarter=ts(Quarter,frequency = 4, start = c(2011,4),end = c(2020,2))
Quarter_M=td(Quarter~1 , to = "monthly" , method = "denton-cholette",conversion = "average")
dygraph(predict(Quarter_M))
Quarter_M_Predict=predict(Quarter_M)
length(Quarter_M_Predict)

Merged_data=data.frame(c(Annual_A_Predict[1:141],Quarter_M_Predict))

U_disaggregation=ts(Merged_data,frequency = 12, start = c(2000,1),end = c(2020,6))
dygraph(U_disaggregation)

plot(U_disaggregation, col = 'red')
lines(Annual)
lines(Quarter)

#library(writexl)
#write_xlsx(data.frame(U_disaggregation),'U_disaggregation.xlsx')

#########################################################################################################
#Season adjustment of IPI indexes

# start with an empty workspace
#rm(list=ls())

# install and load seasonal package 
# install.packages("seasonal")
library(seasonal)



# Monthly Tunisian Dataset from 2000-01 to 2020-06
TNdata = data.frame(read_excel("TNdata.xlsx"))
datatime = TNdata$Date
TNdata = TNdata[,2:dim(TNdata)[2]] #Data without date column
name = names(TNdata)

TNdata <- ts(TNdata ,frequency = 12, start = c(2000,1),end = c(2020,6) ) 
IPI=TNdata[,27:83]
#Example with one serie , the main IPI index
plot(IPI[,1],type = 'l',xlab = "Time" ,ylab = name[27] )


# seasonal adjust time series using X13. 
sa_series <- seas(IPI[,1])

# plot original and seasonally adjusted series.
plot(IPI[,1])
lines(final(sa_series),col=2)

#Main function of the seasonal package. With the default options,
#seas calls the automatic procedures of X-13ARIMA-SEATS to perform a seasonal adjustment
#that works well in most circumstances. Via the argument,
#it is possible to invoke almost all options that are available in X-13ARIMA-SEATS (see details).
#The default options of seas are listed as explicit arguments and are discussed in the arguments section.
#A full-featured graphical user interface can be accessed by the view function.
#The final function returns the adjusted series

#seasonal adjust all IPI indexes using X13 and
#Outlier set equal to NULL to turn off (disabled) automatic oulier detection
require(svMisc) # fancier text progress
IPI_Seasonly_Adjusted = matrix(0, dim(IPI)[1] ,dim(IPI)[2])
for (i in 1:dim(IPI)[2]){
  progress(i*(100/dim(IPI)[2]))
  IPI_Seasonly_Adjusted[,i] = final(seas(IPI[,i] , outlier = NULL) )
  Sys.sleep(0.01)
  if (i == dim(IPI)[2])
    cat("Done!\n")
}


IPI_Seasonly_Adjusted = data.frame(IPI_Seasonly_Adjusted)
names(IPI_Seasonly_Adjusted) = name[27:83]
#write_xlsx(IPI_Seasonly_Adjusted,'IPI_Seasonly_Adjusted.xlsx')

IPI_Seasonly_Adjusted = ts(IPI_Seasonly_Adjusted ,frequency = 12, start = c(2000,1),end = c(2020,6) )

#########################################################################################################
#This step of stationary all series in the dataset

TNdata_SA = data.frame(cbind(U_disaggregation,  TNdata[,1:26] ,IPI_Seasonly_Adjusted  , TNdata[,84:dim(TNdata)[2]]))
TNdata_SA = ts(TNdata_SA,frequency = 12, start = c(2000,1),end = c(2020,6) ) 

names(TNdata_SA_Stationary) = c('chomage',name)
#Chomage : H0 no Stationarity (ADF = DF avec k= 1)
library(urca)
dygraph((TNdata_SA[,1]))
dygraph(diff(diff(log((TNdata_SA[,1]))), main = "Chomage"))
summary(ur.df(diff(diff(TNdata_SA[,1])),lags = 1))



TNdata_SA_Stationary = data.frame(cbind(
  diff(TNdata_SA[,1])	,
  diff(log(TNdata_SA[,2]))	,
  diff(log(TNdata_SA[,3]))	,
  diff(log(TNdata_SA[,4]))	,
  diff(log(TNdata_SA[,5]))	,
  diff(log(TNdata_SA[,6]))	,
  diff(log(TNdata_SA[,7]))	,
  diff(log(TNdata_SA[,8]))	,
  diff(log(TNdata_SA[,9]))	,
  diff(log(TNdata_SA[,10]))	,
  diff(log(TNdata_SA[,11]))	,
  diff(diff(TNdata_SA[,12]))	,
  diff(log(TNdata_SA[,13]))	,
  diff(log(TNdata_SA[,14]))	,
  diff(log(TNdata_SA[,15]))	,
  diff(TNdata_SA[,16])	,
  diff(TNdata_SA[,17])	,
  diff(TNdata_SA[,18])	,
  diff(TNdata_SA[,19])	,
  diff(TNdata_SA[,20])	,
  diff(TNdata_SA[,21])	,
  diff(TNdata_SA[,22])	,
  diff(TNdata_SA[,23])	,
  diff(diff(log(TNdata_SA[,24])))	,
  diff(diff(log(TNdata_SA[,25])))	,
  diff(diff(log(TNdata_SA[,26])))	,
  diff(diff(log(TNdata_SA[,27])))	,
  diff(log(TNdata_SA[,	28	]))	,
  diff(log(TNdata_SA[,	29	]))	,
  diff(log(TNdata_SA[,	30	]))	,
  diff(log(TNdata_SA[,	31	]))	,
  diff(log(TNdata_SA[,	32	]))	,
  diff(log(TNdata_SA[,	33	]))	,
  diff(log(TNdata_SA[,	34	]))	,
  diff(log(TNdata_SA[,	35	]))	,
  diff(log(TNdata_SA[,	36	]))	,
  diff(log(TNdata_SA[,	37	]))	,
  diff(log(TNdata_SA[,	38	]))	,
  diff(log(TNdata_SA[,	39	]))	,
  diff(log(TNdata_SA[,	40	]))	,
  diff(log(TNdata_SA[,	41	]))	,
  diff(log(TNdata_SA[,	42	]))	,
  diff(log(TNdata_SA[,	43	]))	,
  diff(log(TNdata_SA[,	44	]))	,
  diff(TNdata_SA[,	45	])	,
  diff(log(TNdata_SA[,	46	]))	,
  diff(TNdata_SA[,	47	])	,
  diff(TNdata_SA[,	48	])	,
  diff(log(TNdata_SA[,	49	]))	,
  diff(log(TNdata_SA[,	50	]))	,
  diff(log(TNdata_SA[,	51	]))	,
  diff(log(TNdata_SA[,	52	]))	,
  diff(log(TNdata_SA[,	53	]))	,
  diff(log(TNdata_SA[,	54	]))	,
  diff(log(TNdata_SA[,	55	]))	,
  diff(log(TNdata_SA[,	56	]))	,
  diff(log(TNdata_SA[,	57	]))	,
  diff(log(TNdata_SA[,	58	]))	,
  diff(log(TNdata_SA[,	59	]))	,
  diff(log(TNdata_SA[,	60	]))	,
  diff(log(TNdata_SA[,	61	]))	,
  diff(log(TNdata_SA[,	62	]))	,
  diff(log(TNdata_SA[,	63	]))	,
  diff(log(TNdata_SA[,	64	]))	,
  diff(log(TNdata_SA[,	65	]))	,
  diff(log(TNdata_SA[,	66	]))	,
  diff(log(TNdata_SA[,	67	]))	,
  diff(log(TNdata_SA[,	68	]))	,
  diff(log(TNdata_SA[,	69	]))	,
  diff(log(TNdata_SA[,	70	]))	,
  diff(log(TNdata_SA[,	71	]))	,
  diff(log(TNdata_SA[,	72	]))	,
  diff(log(TNdata_SA[,	73	]))	,
  diff(log(TNdata_SA[,	74	]))	,
  diff(log(TNdata_SA[,	75	]))	,
  diff(log(TNdata_SA[,	76	]))	,
  diff(TNdata_SA[,	77	])	,
  diff(log(TNdata_SA[,	78	]))	,
  diff(log(TNdata_SA[,	79	]))	,
  diff(log(TNdata_SA[,	80	]))	,
  diff(log(TNdata_SA[,	81	]))	,
  diff(log(TNdata_SA[,	82	]))	,
  diff(log(TNdata_SA[,	83	]))	,
  diff(log(TNdata_SA[,	84	]))	,
  diff(log(TNdata_SA[,85]))	,
  diff(log(TNdata_SA[,86]))	,
  diff(TNdata_SA[,87])	,
  diff(TNdata_SA[,88])	,
  diff(diff(log(TNdata_SA[,88])))	,
  diff(TNdata_SA[,90])	,
  diff(TNdata_SA[,91])	,
  diff(TNdata_SA[,92])	,
  diff(TNdata_SA[,93])	,
  diff(TNdata_SA[,94])	,
  diff(TNdata_SA[,95])	,
  diff(TNdata_SA[,96])	,
  diff(TNdata_SA[,97])	,
  diff(TNdata_SA[,98])	,
  diff(TNdata_SA[,99])	,
  diff(TNdata_SA[,100])	,
  diff(TNdata_SA[,101])	,
  diff(TNdata_SA[,102])	))
  




library(writexl)
write_xlsx(data.frame('Date' = datatime[3:246],TNdata_SA_Stationary[2:245,]),'TNdata_SA_Stationary.xlsx')
save.image("generateTN_data_SA_Stationary.RData")
           