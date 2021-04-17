# -------------------------------------------------------------------------
# Estimate a first-order autoregressive stochastic volatility model on the
# forecast errors from the macroeconomic data (no conditional mean)
# -------------------------------------------------------------------------

# Initialization
rm(list=ls())
library(stochvol)
# install.packages("svMisc")
require(svMisc) # fancier text progress
set.seed(0) # for replication
options(digits=17)
vt   = read.table("vyt.txt", quote="\"", comment.char="")
T    = dim(vt)[1]
N    = dim(vt)[2]
for (i in 1:N){
	if(min(log(vt[,i]^2))== -Inf){
		vt[,i] = vt[,i] + 0.00001 #offset to avoid taking log of zero
	}
}

# Run MCMC algorithm and store draws
S    = 20000
burn = 10000
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
write(t(out),file='svymeans2020.txt',ncolumn=dim(out)[2])

