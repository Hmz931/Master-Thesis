library(pracma) # package for repmat function and matrix operations

factors  = function(X,kmax,jj,DEMEAN){
  # -------------------------------------------------------------------------
  # Estimate latent factors from observable data X using PCA
  #   Input:  X      = observable data
  #           kmax   = maximum number of factors to consider
  #           jj     = information criterion from Bai and Ng (2002)
  #           DEMEAN = 0 - no, 1 - yes, 2 - standardize
  #   Output: ehat   = idiosyncratic errors
  #           Fhat   = latent factor estimates
  #           lamhat = factor loadings
  #           ve2    = eigenvalues of data covariance matrix
  # -------------------------------------------------------------------------
  
  
  ic1 = nbplog(X,kmax,jj,DEMEAN)$ic1
  chat = nbplog(X,kmax,jj,DEMEAN)$chat
  fhat = nbplog(X,kmax,jj,DEMEAN)$Fhat
  eigval = nbplog(X,kmax,jj,DEMEAN)$eigval
  
  
  
  icstar    = ic1
  R2_static = sum(eigval[1:icstar])/sum(eigval)
  if (DEMEAN == 2){
    ehat = pc(standard(X),icstar)$ehat
    Fhat = pc(standard(X),icstar)$fhat
    lamhat = pc(standard(X),icstar)$lambda
    ve2 = pc(standard(X),icstar)$ss
  }
  
  if (DEMEAN == 1){
    ehat = pc(X-repmat(mean(X),size(X)[1],1),icstar)$ehat
    Fhat = pc(X-repmat(mean(X),size(X)[1],1),icstar)$fhat
    lamhat = pc(X-repmat(mean(X),size(X)[1],1),icstar)$lambda
    ve2 = pc(X-repmat(mean(X),size(X)[1],1),icstar)$ss
  }
  
  if (DEMEAN == 0){
    ehat = pc(X,icstar)$ehat
    Fhat = pc(X,icstar)$fhat
    lamhat = pc(X,icstar)$lambda
    lamhat = pc(X,icstar)$ss
  }
  
  out = list('ehat' = ehat,'Fhat' = Fhat,'lamhat' = lamhat,'ve2' = ve2)
  return(out)
}

# Auxiliary functions
nbplog = function(x,kmax,jj,DEMEAN){
  T=dim(x)[1]
  N=dim(x)[2]
  NT=N*T
  NT1=N+T
  CT=matrix(0,1,kmax)
  ii=(1:1):kmax
  if (jj ==1)
    CT[1,]=log(NT/NT1)*ii*NT1/NT
  if( jj==2)
    CT[1,]=(NT1/NT)*log(min(N,T))*ii
  GCT=min(N,T);
  if (jj==3)
    CT[1,]=ii*log(GCT)/GCT
  if (jj==4)
    CT[1,]=2*ii/T
  if (jj==5)
    CT[1,]=log(T)*ii/T
  if (jj==6)
    CT[1,]=2*ii*NT1/NT
  if (jj==7)
    CT[1,]=log(NT)*ii*NT1/NT
  if (jj==8)
    CT[1,]= 2*ii*(sqrt(N)+sqrt(T))^2/(NT)  # new modified CP
  
  if (DEMEAN ==2)
    X=scale(x , center = TRUE , scale = TRUE)
  if (DEMEAN ==1)
    X=scale(x , center = TRUE , scale = FALSE)
  if (DEMEAN==0)
    X=x
  
  IC1 = matrix(0,dim(CT)[1] , kmax+1)
  Sigma = matrix(0,1,kmax+1)
  if (T< N){
    ev = svd(X%*%t(X))$v ;  eigval =  diag(svd(X%*%t(X))$d );ev1 = svd(X%*%t(X))$u
    
    sumeigval=cumsum(diag(eigval))/sum(diag(eigval))
    Fhat0=sqrt(T)*ev
    Lambda0=t(X)%*%Fhat0/T
  }else{
    ev = svd(X%*%t(X))$v ;  eigval =  diag(svd(X%*%t(X))$d );ev1 = svd(X%*%t(X))$u
    
    sumeigval=cumsum(diag(eigval))/sum(diag(eigval))
    Lambda0=sqrt(N)*ev
    Fhat0=t(X)%*%t(t(Lambda0/N))
  }
  
  if (jj <= 8)
    for (i in kmax:1){
      Fhat=Fhat0[,1:i]
      #lambda=Fhat'*X
      lambda=Lambda0[,1:i]
      chat=Fhat%*%t(lambda)
      #disp([i sumeigval(i) sum(sum(chat.*chat))/sum(sum(X.*X))])
      ehat=X-t(chat)                       #if T< N ==> ehat=X-(chat)
      Sigma[i]=mean(sum(ehat*ehat/T))
      IC1[,i]=log(Sigma[i])+CT[,i]         #Eq (9) Bai and Ng 2002
    }
  
  Sigma[kmax+1]=mean(sum(X*X/T))
  IC1[,kmax+1]=log(Sigma[kmax+1])
  ic1=t(minindc(t(IC1)))
  ic1=ic1 *(ic1 <= kmax);
  
  if (jj==9){
    for (j in 1:(nrow(sumeigval))){
      if (sumeigval[j] >= .5){
        ic1=j
        break
      }
      
    }
    
  }
  
  Fhat=c()
  Fhat=Fhat0[,1:kmax]
  Lambda=Lambda0[,1:kmax]
  chat=Fhat%*%t(Lambda)
  eigval=diag(eigval)
  
  out = list('ic1' = ic1, 'chat' = chat,'Fhat' = Fhat,'eigval' = eigval)
  return(out)

}


pc = function(y,nfac){
  bigt = dim(y)[1] ; bign = dim(y)[2]
  yy=t(y)%*%y
  Fhat0 = svd(yy)$v ;  eigval =  diag(svd(yy)$d );Fhat1 = svd(yy)$u
  
  
  lambda=Fhat0[,1:nfac]*sqrt(bign)
  fhat=y%*%lambda/bign
  ehat=y-fhat%*%t(lambda)
  
  ve2=t(sum(t(ehat) * t(ehat)   )  )/bign
  ss=diag(eigval)
  
  
  out = list('ehat' = ehat,'fhat' = fhat,'lambda' = lambda,'ss' = ss)
  return(out)
}


minindc = function(x){
  ncols=dim(x)[2]
  nrows=dim(x)[1]
  pos=matrix(0,ncols,1)
  seq=seqa(1,1,nrows)
  for (i in 1:ncols){
    dum=min(x[,i])
    dum1= seq * ( (x[,i]-dum) ==0)
    pos[i]=sum(dum1)
    
  }
  return(pos)
}


#standard = function(y){
# T=dim(y)[1]
# N=dim(y)[2]
# my=repmat(mean(y),T,1)
# sy=repmat(std(y),T,1)
# x=(y-my)/sy
# 
# return(x)
#}

standard =  function(y){
  x = scale(y , center = TRUE , scale = TRUE)
  return(x)
}

seqa = function(a,b,c){
  seq=t(t(((a:b):(a+b*(c-1)))))
  return(seq)
}



mlags = function(x,k,p,v){
  # -------------------------------------------------------------------------
  # Create a matrix of n lags of a vector or matrix
  #   Input:  x = matrix or vector, (nobs x k)
  #           k = number of lags (default = 1)
  #           p = number of last lags to keep (default = k)
  #           v = (optional) initial values (default = 0)
  #   Output: z = matrix (or vector) of lags (nobs x nvar*n)
  # -------------------------------------------------------------------------
  if (missing(k) & missing(p) & missing(v) ){  
    k = 1
    v = 0
    p = k
  }else if (missing(p) & missing(v)) {
    v = 0
    p = k
  }else if (missing(v)) {
    v = 0
  }
  
  
  if (p>k){
    stop('mlags: Not enough lags')
  }
  
  
  nobs = size(as.matrix(x))[1] ; nvar = size(as.matrix(x))[2]
  z=matrix(1,nobs,nvar*k)*v
  
  
  for (j in 1:k)
    z[(j+1):nobs,(nvar*(j-1)+1):(j*nvar)] = x[1:(nobs-j),]
  
  
  z = z[,(ncol(z)-nvar*p+1):ncol(z)]
  
  return(z)
  
}







