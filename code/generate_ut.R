# -------------------------------------------------------------------------
# Compute matrix of uncertainty estimates for horizons 1 through 12
# -------------------------------------------------------------------------
  
# Load data
rm(list=ls())
load('ferrors.RData')
library(pracma) # package for repmat function and matrix operations
svf <- read.table("svfmeans2020.txt", quote="\"", comment.char="")
svy <- read.table("svymeans2020.txt", quote="\"", comment.char="")

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
ut    = array(0,dim=c(T,N,h))
for (i in 1:N){
  yb  = ybetas[i,]
  thy = rbind(svy[1,i]*(1-svy[2,i]),svy[2,i],svy[3,i]^2 )
  xy  = svy[(4:(dim(svy)[1]-3)),i]
  ut[,i,] = compute_uy(xy,thy,yb,py,evf,phif)
}



