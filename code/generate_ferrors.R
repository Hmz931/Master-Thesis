# -------------------------------------------------------------------------
#  Matlab code was provided by Jurado et al. (2014) 
#  https://www.aeaweb.org/articles?id=10.1257/aer.20131193
#   This is the R version of that code 
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Generate forecast errors 
# -------------------------------------------------------------------------

# Load data
rm(list=ls())
library(pracma) # package for repmat function and matrix operations
source("factors.R")
source("compute_uf.R")
source("compute_uy.R")
source("nwest.R")

library(readxl)
TNdata_SA_Stationary = read_excel("TNdata_SA_Stationary.xlsx")
datatime = TNdata_SA_Stationary$Date
xt = TNdata_SA_Stationary[,2:(dim(TNdata_SA_Stationary)[2])]
xt = as.matrix(xt)


# Estimate factors
#Ft hat
F_estimate = factors(xt,20,2,2)
e = F_estimate$ehat
fhat = F_estimate$Fhat
lf = F_estimate$lamhat
vf = F_estimate$ve2
#Gt hat
G_estimate = factors(xt^2,20,2,2)
e = G_estimate$ehat
ghat = G_estimate$Fhat
lg = G_estimate$lamhat
vg = G_estimate$ve2

# predictor set
ft = cbind(fhat,fhat[,1]^2 ,ghat[,1])



# Generate forecast errors for yt
yt     = scale(xt, center = TRUE, scale = TRUE) # only the macro data , in my case yt = xt ==> no financial data
T = dim(yt)[1] ; N = dim(yt)[2]

py     = 4
pz     = 2
p      = max(py,pz)
q      = trunc(4*(T/100)^(2/9))

vyt = matrix(0,T-py,N)
ybetas = matrix(0,1+py+pz*(dim(ft)[2]),N)
fmodels = matrix(0,(pz*dim(ft)[2]),N)
for (i in 1:N){
  X = cbind(matrix(1,T,1), mlags(as.matrix(yt[,i]),py) , mlags(ft,pz))
  reg = nwest(  yt[(p+1):dim(yt)[1],i] , X[(p+1):dim(X)[1],] , q)
  pass = abs(reg$tstat[(py+2):length(reg$tstat)]) > 2.575  # hard threshold
  keep = cbind(matrix(1,1,py+1)==1 ,t(pass))
  Xnew = X[,keep]
  reg  = nwest(yt[(p+1):dim(yt)[1],i],Xnew[(p+1):dim(Xnew)[1],],q)
  vyt[,i]       = reg$resid  # forecast errors
  ybetas[keep,i] = reg$beta   
  fmodels[,i]   = pass #chosen predictors
}

#Check the most chosen predictor (4 factors with their lagas) 
Count_Factors = matrix(0, dim(ft)[2]*pz,1 )
rownames(Count_Factors) = c('f_1_t-1',	'f_2_t-1','f_3_t-1','f_4_t-1','f_1_t-2','f_2_t-2','f_3_t-2','f_4_t-2')
for (i in 6:dim(ybetas)[1]){
  Count_Factors[i-5,] = length(which(ybetas[i,] != 0) )
}
Count_Factors

# Generate AR(4) errors for ft
T = dim(ft)[1] ; R = dim(ft)[2]
pf     = 4
q      = trunc(4*(T/100)^(2/9))

vft = matrix(0,T-pf,R)
fbetas = matrix(0,R,pf+1)
for (i in 1:R){
  X   = cbind(matrix(1,T,1),mlags(as.matrix(ft[,i]),pf))
  reg = nwest(ft[(pf+1):dim(ft)[1],i],X[(pf+1):dim(X)[1],],q)
  vft[,i]    = reg$resid
  fbetas[i,] = t(reg$beta)
}


# Save data
T = dim(vyt)[1] ; N = dim(vyt)[2]
ybetas = t(ybetas)


# Also write to .txt file for the next R code
write(t(vyt),file='vyt.txt',ncolumn=dim(vyt)[2])
write(t(vft),file='vft.txt',ncolumn=dim(vft)[2])

save.image("ferrors.RData")

