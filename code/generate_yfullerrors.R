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
  pass = abs(reg$tstat[(py+2):length(reg$tstat)]) > 0  # hard threshold
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






# -------------------------------------------------------------------------
# Estimate a first-order autoregressive stochastic volatility model on the
# forecast errors from the macroeconomic data (no conditional mean)
# -------------------------------------------------------------------------

# Initialization

library(stochvol)
# install.packages("svMisc")
require(svMisc) # fancier text progress
set.seed(0) # for replication
options(digits=17)
vt   = vyt
T    = dim(vt)[1]
N    = dim(vt)[2]
for (i in 1:N){
  if(min(log(vt[,i]^2))== -Inf){
    vt[,i] = vt[,i] + 0.00001 #offset to avoid taking log of zero
  }
}

# Run MCMC algorithm and store draws
S    = 200
burn = 100
m    = matrix(0,T+3,N)
g    = matrix(0,3,N)
for (i in 1:N){
  progress(i*(100/N))
  draws  = svsample(vt[,i],draws=S,burnin=burn,quiet=TRUE,thinpara=10,thinlatent=10)
  all    = cbind(draws$para,draws$latent)
  m[,i]  = colMeans(all)
  g[,i]  = geweke.diag(draws$para)$z
  name   = sprintf('svydraws%d.txt',i)
  #	write(t(all),file=name,ncolumn=dim(all)[2])
  Sys.sleep(0.01)
  if (i == N)
    cat("Done!\n")
}
out = rbind(m,g) #include Geweke statistics
write(t(out),file='svyfullmeans2020.txt',ncolumn=dim(out)[2])

# -------------------------------------------------------------------------
# Estimate a first-order autoregressive stochastic volatility model on the
# forecast errors from an AR(4) model on each predictor (series by series)
# -------------------------------------------------------------------------


library(stochvol)
# install.packages("svMisc")
require(svMisc) # fancier text progress
set.seed(1000) # for replication
options(digits=17)
vt   = vft
T    = dim(vt)[1]
N    = dim(vt)[2]
for (i in 1:N){
  if(min(log(vt[,i]^2))== -Inf){
    vt[,i] = vt[,i] + 0.00001 #offset to avoid taking log of zero
  }
}

# Run MCMC algorithm and store draws
S    = 200
burn = 100
m    = matrix(0,T+3,N)
g    = matrix(0,3,N)
for (i in 1:N){
  progress(i*(100/N))
  draws  = svsample(vt[,i],draws=S,burnin=burn,quiet=TRUE,thinpara=10,thinlatent=10)
  all    = cbind(draws$para,draws$latent)
  m[,i]  = colMeans(all)
  g[,i]  = geweke.diag(draws$para)$z
  name   = sprintf('svfdraws%d.txt',i)
  #	write(t(all),file=name,ncolumn=dim(all)[2])
  Sys.sleep(0.01)
  if (i == N)
    cat("Done!\n")
}
out = rbind(m,g) #include Geweke statistics
write(t(out),file='svffullmeans2020.txt',ncolumn=dim(out)[2])


# -------------------------------------------------------------------------
# Compute matrix of uncertainty estimates for horizons 1 through 12
# -------------------------------------------------------------------------

# Load data
library(pracma) # package for repmat function and matrix operations
svf <- read.table("svffullmeans2020.txt", quote="\"", comment.char="")
svy <- read.table("svyfullmeans2020.txt", quote="\"", comment.char="")

# Compute objects from predictors
h   = 12
fb  = fbetas
thf = rbind(svf[1,]*(1-svf[2,]),svf[2,],svf[3,]^2 )
xf  = svf[4:(dim(svf)[1]-3),]
gf  = svf[((dim(svf)[1]-3)+1):(dim(svf)[1]),]
evf = compute_uf(xf,thf,fb,h)$evf
phif =compute_uf(xf,thf,fb,h)$phif


# Compute uncertainty
T = dim(vyt)[1]
N = dim(vyt)[2]
utfull    = array(0,dim=c(T,N,h))
for (i in 1:N){
  yb  = ybetas[i,]
  thy = rbind(svy[1,i]*(1-svy[2,i]),svy[2,i],svy[3,i]^2 )
  xy  = svy[(4:(dim(svy)[1]-3)),i]
  utfull[,i,] = compute_uy(xy,thy,yb,py,evf,phif)
}

# -------------------------------------------------------------------------
# Generate aggregate uncertainty estimates
# -------------------------------------------------------------------------

# Initialization

T = dim(utfull)[1]
N = dim(utfull)[2]
h = dim(utfull)[3]

# Cross-sectional average
utfullcsa = matrix(0,T,h)
for (i in 1:h){
  utfullcsa[,i] = rowMeans(sqrt(utfull[,,i]))
}


save(utfullcsa, file = "utfullcsa.Rdata")
save(utfull, file = "utfull.Rdata")

