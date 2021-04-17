#################################################################################
# Auxiliary function
expectedvar = function(a,b,t2,x,h){
  # -------------------------------------------------------------------------
  # Compute Et[exp{x(t+h)}] using the AR(1) law of motion for x(t)
  # -------------------------------------------------------------------------
  out = exp(a*(1-b^h)/(1-b)+t2/2*(1-b^(2*h))/(1-b^2)+ b^h*x)
  return(out)
}

#################################################################################

compute_uy = function(xy,thy,yb,py,evf,phif){
  
  # -------------------------------------------------------------------------
  # Compute expected volatility of predictors up to horizon h
  # -------------------------------------------------------------------------
  
  # Initialize parameters
  h  = length(evf)
  r  = dim(evf[[1]])[2]  
  pf = dim(phif)[1]/r
  pz = (length(yb)-1-py)/r
  T  = length(xy)

  # Preallocate variables
  U = matrix(0,T,h)
  if (pf >1)
    evf0 = rep(0,r*(pf-1))
  if (pf==1)
    evf0 = c()
  if (py >1)
    evy0 = rep(0,py-1)
  if (py==1)
    evy0 = c()
  
  # Construct the main phi matrix
  if (pf >pz)
   lambda_topright = rep(0,(pf-pz)*r)
  if (pf==pz)
   lambda_topright = c()
  if( py >1)
    lambda_bottom   = matrix(0,py-1,r*pf)
  if (py==1)
   lambda_bottom   = c()
  lambda   =  rbind(c(yb[(py+2):length(yb)],lambda_topright),lambda_bottom)
  
  phiy_top = yb[2:(py+1)]
  if (py >1)
    phiy_bottom = cbind(  diag(1,py-1,py-1) , diag(0,py-1,1) )
  if (py==1)
    phiy_bottom = c()
  phiy         = rbind(phiy_top,phiy_bottom)  
  phi_topright = matrix(0,r*pf,py)
  phi          = rbind(cbind(phif,phi_topright) , cbind(lambda,phiy))
  phi = matrix(phi, dim(phi)[1],dim(phi)[2])
  
  # Compute uncertainty using the recursion
  alpha = thy[1]
  beta  = thy[2]
  tau2  = thy[3]
  x     = xy
  evy = rep(list(matrix(NA,T,1)),h)
  for (j in 1:h){
    evy[[j]] = expectedvar(alpha,beta,tau2,x,j); #Et[(v^y_t)^2]
  }
  
  for (t in 1:T )
    for (j in 1:h ){
      ev = diag(c(evf[[j]][t,],evf0,evy[[j]][t],evy0),nrow = r*pf+py ,ncol = r*pf+py) #The expectation of the variance (stochastic volatility) of the 4 factors and of the variable yj for h = 1..12 forecast horizon
      if (j == 1)
        u = ev
      if (j  > 1)
        u = phi %*% u %*% t(phi) + ev  #The phi matrix is composed of the autoregressive coefficients of the predictors (AR (4)) and of the variable yj (AR (4) in the forecast equation of the selected predictors)
      U[t,j] = u[r*pf+1,r*pf+1] # select relevant entry
    
 
    }
  return(U)

}

