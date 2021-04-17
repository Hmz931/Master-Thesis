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

compute_uf = function(xf,thf,fb,h){
  
  T = length(xf[,1])
  r   = dim(xf)[2]
  pf  = dim(fb)[2]-1
  
  # Create phif matrix
  phif_top=c()
  for ( j in 2:(pf+1)){
    phif_top = cbind(phif_top,diag(fb[1:r,j]))  }
  if (pf > 1) {
    phif_bot = cbind( diag(1, r*(pf-1),r*(pf-1) ), matrix(0,r*(pf-1),r)  )
    phif     = rbind( phif_top,phif_bot  )
  }else{ 
      phif = phif_top
  }
  # Compute evf
  evf = rep(list(matrix(NA,T,r)),h)
  for ( j in 1:h )
    for ( i in 1:r ){
      alpha       = thf[1,i]
      beta        = thf[2,i]
      tau2        = thf[3,i]
      x           = xf[,i]
      evf[[j]][,i] = expectedvar(alpha,beta,tau2,x,j); #Et[(v^f_t)^2]
    }
    out = list('evf' = evf , 'phif' = phif)

  
  return(out)  
}






