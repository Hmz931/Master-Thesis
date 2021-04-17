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


# Generate forecast errors for yt
yt     = scale(xt, center = TRUE, scale = TRUE) # only the macro data , in my case yt = xt ==> no financial data
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
write(t(out),file='svyarmeans2020.txt',ncolumn=dim(out)[2])



#-------------------------------------------------------------------------
  #Compute matrix of uncertainty estimates (ar conditional mean only)
#-------------------------------------------------------------------------
  
#Load data
svy = read.table("svyarmeans2020.txt", quote="\"", comment.char="");

#Compute uncertainty
# install.packages("svMisc")
require(svMisc) # fancier text progress
library(Matrix)
h = 12;
T = dim(vyt)[1] ; N = dim(vyt)[2]
thy = rbind(svy[1,]*(1-svy[2,]),svy[2,],svy[3,]^2 )
xy  = svy[(4:(dim(svy)[1]-3)),]
gy  = svy[(dim(svy)[1]-(3+1)):(dim(svy)[1]),]
utar    = array(0,dim=c(T,N,h))
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
      utar[t,i,j] = u[1,1];
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

T = dim(utar)[1]
N = dim(utar)[2]
h = dim(utar)[3]

# Cross-sectional average
utarcsa = matrix(0,T,h)
for (i in 1:h){
  utarcsa[,i] = rowMeans(sqrt(utar[,,i]))
}

save(utarcsa, file = "utarcsa.Rdata")
save(utar, file = "utar.Rdata")



########################################################################################################""
#Or use plot function

ts_uar=ts(utarcsa ,   frequency = 12, start=c(2000,7), end=c(2020,6))

plot(ts_uar[,1],  type = "l" , col = "blue",  xlab = "",    ylab = "Incertitude",
     main = "Evolution de l'indice d'incertitude de 2000 à 2020"  , lwd=3)                                 
lines(ts_uar[,3], type = "l", col = "black"  , lwd=3)                                   
lines(ts_uar[,12], type = "l", col = "red"  , lwd=3)                                
abline(h = (1.65*sd(ts_uar[,1]))+mean(ts_uar[,1]), col = "blue" ,lty=2, lwd=1)        # Add first line
abline(h = (1.65*sd(ts_uar[,3]))+mean(ts_uar[,3]), col = "black",lty=2, lwd=1)        # Add second line
abline(h = (1.65*sd(ts_uar[,12]))+mean(ts_uar[,12]), col = "red",lty=2, lwd=1)        # Add third line

legend("topleft",                                     
       legend = c("h = 1", "h = 3", "h = 12"),
       col = c("blue", "black", "red"),
       lwd=3, ncol = 1,  cex = 0.72)









