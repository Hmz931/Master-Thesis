# libraries
library(dygraphs)
library(xts) # To make the convertion data-frame / xts format
library(lubridate) # To work with dates
library(tidyverse)
library(dplyr)
load("ut.RData")
options("scipen"=100, "digits"=4)

# Change to xts format
don <- xts(x = utcsa, order.by =  datatime[(length(datatime) - dim(utcsa)[1]+1) : length(datatime)] )

# Chart
p <- dygraph(cbind('U(1)' = don[,1],'U(3)' = don[,3],'U(12)' = don[,12]) ,
             main = "Evolution de l'indice d'incertitude de 2000 à 2020", 
             xlab = "Année", 
             ylab = "Uncertitude")%>%
  dyLimit((1.65*sd(don[,1]))+mean(don[,1]),label = "",color = "red",strokePattern = "dashed")%>%
  dyLimit((1.65*sd(don[,3]))+mean(don[,3]),label = "",color = "green",strokePattern = "dashed")%>%
  dyLimit((1.65*sd(don[,12]))+mean(don[,12]),label = "",color = "green",strokePattern = "dashed")


p %>%dyShading(from = "2010-10-1", to = "2011-5-1", 
          color = rgb(red = 0.85, green = 0.85, blue = 0.93, alpha = 0.8)) %>%
  dyShading(from = "2019-12-1", to = "2020-6-30", 
            color = rgb(red = 0.85, green = 0.85, blue = 0.93, alpha = 0.8))%>%
  dyShading(from = "2008-10-1", to = "2009-5-1", 
            color = rgb(red = 0.85, green = 0.85, blue = 0.93, alpha = 0.8))%>%
  dyEvent(c("2009-05-01","2011-04-15", "2019-12-01"),
          c("Crise financiere","Revolution Tunisienne", "Apparition de la pandémie COVID-19"),
          color = "black",
          labelLoc = "top",
          strokePattern = c(1,500,30,400))%>%
  dyOptions(fillGraph = FALSE)


# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/dygraphs316-4.html"))


########################################################################################################""
#Or use plot function

ts_u=ts(utcsa ,   frequency = 12, start=c(2000,7), end=c(2020,6))

plot(ts_u[,1],  type = "l" , col = "blue",  xlab = "",    ylab = "Incertitude",
     main = "Evolution de l'indice d'incertitude de 2000 à 2020"  , lwd=3)                                 
lines(ts_u[,3], type = "l", col = "black"  , lwd=3)                                   
lines(ts_u[,12], type = "l", col = "red"  , lwd=3)                                
abline(h = (1.65*sd(ts_u[,1]))+mean(ts_u[,1]), col = "blue" ,lty=2, lwd=1)        # Add first line
abline(h = (1.65*sd(ts_u[,3]))+mean(ts_u[,3]), col = "black",lty=2, lwd=1)        # Add second line
abline(h = (1.65*sd(ts_u[,12]))+mean(ts_u[,12]), col = "red",lty=2, lwd=1)        # Add third line

legend("topleft",                                     
       legend = c("h = 1", "h = 3", "h = 12"),
       col = c("blue", "black", "red"),
       lwd=3, ncol = 1,  cex = 0.72)






########################################################################################################
#Plot Factors Variances

library(readxl)
TNdata_SA_Stationary <- read_excel("TNdata_SA_Stationary.xlsx")

data = TNdata_SA_Stationary[,2:(dim(TNdata_SA_Stationary)[2])]

#Apply directly in the Var-Cov Matrix ==> eigen( scale(data, center = TRUE, scale = TRUE)) %*% t(scale(data, center = TRUE, scale = TRUE))  )

#install.packages("factoextra")
library(factoextra)
pca <- prcomp(data, scale = TRUE, center = TRUE)
# Valeurs propres
eig.val <- get_eigenvalue(pca)
eig.val   #Percentage variation
pdf("factors.pdf")
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 20),
         main = '',# main = 'Pourcentage de variance expliqué par chacun des facteurs'
         xlab = 'Facteurs',ylab = 'Pourcentage de variance expliquée')
dev.off()

fviz_pca_ind(pca,
             col.ind = "cos2", # Colorer par le cos2
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)

fviz_pca_var(pca,
             col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     
)

fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF", 
                col.ind = "#696969"  
)


library(factoextra)
# Valeurs propres
eig.val <- get_eigenvalue(pca)
eig.val

# Résultats des variables
res.var <- get_pca_var(pca)
res.var$coord          # Coordonnées
res.var$contrib        # Contributions aux axes



# Plus grande contribution par les 3  variables au facteur 1
data.frame(res.var$contrib[,1]) %>% slice_max(res.var$contrib[,1], n = 3)

# Plus grande contribution par les 3  variables au facteur 2
data.frame(res.var$contrib[,2]) %>% slice_max(res.var$contrib[,2], n = 3)

res.var$cos2           # Qualité de représentation 
# Résultats des individus
res.ind <- get_pca_ind(pca)
res.ind$coord          # Coordonnées
res.ind$contrib        # Contributions aux axes
res.ind$cos2           # Qualité de représentation                 


########################################################################################################
#Summary statistics
library(moments)
plot(density(utcsa[,1]) , , col="blue", lwd=3 )
abline(v = mean(utcsa[,1]), col="red", lwd=3, lty=2)
legend("topright",                                     
       legend = c("h = 1", "mean = 0.763") ,title="skewness = 2.75                 kurtosis = 15.5" ,
       col = c("blue", "red"),
       lwd=3, ncol = 2,  cex = 0.72)

# Print skewness of distribution 
skewness(utcsa[,1])

#Positivly skewed = 2.752095 > 0
#the coefficient of skewness is more than 0 i.e. (s > 0), 
#then the graph is said to be positivly skewed with the majority of data values less than mean.
#Most of the values are concentrated on the left side of the graph.

# Print skewness of distribution 
kurtosis(utcsa[,1]) 

#Leptokurtic = 15.49795 > 3
#the coefficient of kurtosis is greater than 3  (k > 3), 
#then the data distribution is leptokurtic and shows a sharp peak on the graph


#IMF plot forecast
library(readxl)
dataset <- read_excel("C:/Users/Lenovo/Desktop/Brouillon/IMF GDP-CPI Projection/Projected GDP CPI TUN IMF.xlsx", 
                      sheet = "Feuil3")
dataset = ts(dataset ,   frequency = 1, start=c(1999), end=c(2020) )

pdf("IMF_Forecast.pdf")
par(mfrow=c(2,2))
plot(dataset[,1],  type = "l" , col = "red",  xlab = "",    ylab = "",
     main = "Ecart au carré entre une prevsions initiale et
     ajustée de croissance de PIB annuel de l'année en cours"  , lwd=3,cex.main=0.85)
abline(a=NULL, b=NULL, h=0, v=NULL, lty=2)
plot(dataset[,2],  type = "l" , col = "red",  xlab = "",    ylab = "",
     main = "Ecart au carré entre une prevsions initiale et
     ajustée de croissance de PIB annuel pour l'année prochaine"  , lwd=3,cex.main=0.85)
abline(a=NULL, b=NULL, h=0, v=NULL, lty=2)
plot(dataset[,3],  type = "l" , col = "blue",  xlab = "",    ylab = "",
     main = "Ecart au carré entre une prevsions initiale et
     ajustée de croissance de IPC annuel de l'année en cours"  , lwd=3,cex.main=0.85)
abline(a=NULL, b=NULL, h=0, v=NULL, lty=2)
plot(dataset[,4],  type = "l" , col = "blue",  xlab = "",    ylab = "",
     main = "Ecart au carré entre une prevsions initiale et
     ajustée de croissance de IPC annuel pour l'année prochaine"  , lwd=3,cex.main=0.85)
abline(a=NULL, b=NULL, h=0, v=NULL, lty=2)

dev.off()
