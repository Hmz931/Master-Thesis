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
yt     = ft
T = dim(yt)[1] ; N = dim(yt)[2]

py     = p = 4
pz     = 2
q      = trunc(4*(T/100)^(2/9))

vyt = matrix(0,T-py,N)
ybetas = matrix(0,1+py,N)

for (i in 1:N){
  X = cbind(matrix(1,T,1), mlags(as.matrix(yt[,i]),py) )
  reg = nwest(  yt[(p+1):dim(yt)[1],i] , X[(p+1):dim(X)[1],] , q)
  vyt[,i]       = reg$resid  # forecast errors
  ybetas[,i] = reg$beta   
  
}




# Save data
T = dim(vyt)[1] ; N = dim(vyt)[2]
ybetas = t(ybetas)



# -------------------------------------------------------------------------
# Estimate a first-order autoregressive stochastic volatility model on the
# forecast errors from the macroeconomic data (ar conditional mean only)
# -------------------------------------------------------------------------

# Initialization
library(stochvol)
set.seed(4000) # for replication
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
S    = 5000
burn = 500
m    = matrix(0,T+3,N)
g    = matrix(0,3,N)
for (i in 1:N){
  draws  = svsample(vt[,i],draws=S,burnin=burn,quiet=TRUE,thinpara=10,thinlatent=10)
  all    = cbind(draws$para,draws$latent)
  m[,i]  = colMeans(all)
  g[,i]  = geweke.diag(draws$para)$z
  name   = sprintf('arsvdraws%d.txt',i)
  #write(t(all),file=name,ncolumn=dim(all)[2])
}
out = rbind(m,g) #include Geweke statistics
write(t(out),file='svfarmeans2020.txt',ncolumn=dim(out)[2])



#-------------------------------------------------------------------------
#Compute matrix of uncertainty estimates (ar conditional mean only)
#-------------------------------------------------------------------------

#Load data
svy = read.table("svfarmeans2020.txt", quote="\"", comment.char="");

#Compute uncertainty
# install.packages("svMisc")
require(svMisc) # fancier text progress
library(Matrix)
h = 12;
T = dim(vyt)[1] ; N = dim(vyt)[2]
thy = rbind(svy[1,]*(1-svy[2,]),svy[2,],svy[3,]^2 )
xy  = svy[(4:(dim(svy)[1]-3)),]
gy  = svy[(dim(svy)[1]-(3+1)):(dim(svy)[1]),]
utfar    = array(0,dim=c(T,N,h))
py    = dim(ybetas)[2]-1

for (i in 1:N){
  progress(i*(100/N))
  phi   = Matrix(rbind( ybetas[i,2:(dim(ybetas)[2])],cbind(diag(py-1),rep(0,py-1))), sparse = TRUE)
  a     = thy[1,i];
  b     = thy[2,i];
  t2    = thy[3,i];
  x     = xy[,i];
  evy = rep(list(matrix(NA,T,1)),h)
  
  for (j in 1:h){
    evy[[j]] =  exp(a*(1-b^j)/(1-b)+t2/2*(1-b^(2*j))/(1-b^2)+ b^j*x);
  }
  for(t in 1:T){
    for(j in 1:h){
      ev = Matrix(diag(c(evy[[j]][t]),nrow = py ,ncol = py),sparse = TRUE)
      #ev = sparse(1,1,evy[[j]][t],py,py);
      if (j == 1) u = ev;
      if (j  > 1) u = phi %*% u %*% t(phi) + ev
      utfar[t,i,j] = u[1,1];
    }
  }
  Sys.sleep(0.01)
  if (i == N)
    cat("Done!\n")
}


# -------------------------------------------------------------------------
# Generate aggregate uncertainty estimates
# -------------------------------------------------------------------------

# Initialization

T = dim(utfar)[1]
N = dim(utfar)[2]
h = dim(utfar)[3]

# Cross-sectional average
utfarcsa = matrix(0,T,h)
for (i in 1:h){
  utfarcsa[,i] = rowMeans(sqrt(utfar[,,i]))
}

save(utfarcsa, file = "utfarcsa.Rdata")
save(utfar, file = "utfar.Rdata")





########################################################################################################""
#Or use plot function

load("utfar.Rdata")
ts_utfar=ts(utfar[,,1] ,   frequency = 12, start=c(2000,7), end=c(2020,6)) #h = 1

pdf("FactorsU.pdf")
par(mfrow=c(2,2))


plot(ts_utfar[,1],  type = "l" , col = "blue",  xlab = "",    ylab = "Incertitude",
     main = "Premier facteur"  , lwd=2)  

plot(ts_utfar[,2],  type = "l" , col = "blue",  xlab = "",    ylab = "Incertitude",
     main = "Deuxième facteur"  , lwd=2)    

plot(ts_utfar[,3],  type = "l" , col = "blue",  xlab = "",    ylab = "Incertitude",
     main = "Troisième facteur"  , lwd=2)   

plot(ts_utfar[,4],  type = "l" , col = "blue",  xlab = "",    ylab = "Incertitude",
     main = "Quatrième facteur"  , lwd=2)   

dev.off()








