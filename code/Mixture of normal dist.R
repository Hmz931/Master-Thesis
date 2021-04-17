#Sample Mixture of 7 normal distributions as Kim et al. 1998

q = c(	0.04395,	0.24566,	0.34001,	0.25750,	0.10556,	0.00002,	0.00730  )
b = c(  2.77786,  1.79518,  0.61942, -1.08819, -3.97281, -8.56686, -10.12999 ) -1.2704
w = c(	0.16735,	0.34023,	0.64009,	1.26261,	2.61369,	5.17950,	5.79596  )

c_q = cumsum(q)

#The number of samples from the mixture distribution
N = 1000            

#Sample N random uniforms U
U =runif(N)

#Variable to store the samples from the mixture distribution                                             
rand.samples = rep(NA,N)
S = c()
#Sampling from the mixture
for(i in 1:N){
  if(U[i] <= c_q[1] ){
    rand.samples[i] = rnorm(1,b[1],w[1])
    S[i] = 1
  }else if(U[i] > c_q[1] & U[i]<= c_q[2]){
    rand.samples[i] = rnorm(1,b[2],w[2])
    S[i] = 2
  }else if (U[i] > c_q[2] & U[i]<= c_q[3]){
    rand.samples[i] = rnorm(1,b[3],w[3])
    S[i] = 3
  }else if(U[i] > c_q[3] & U[i]<= c_q[4]){
    rand.samples[i] = rnorm(1,b[4],w[4])
    S[i] = 4
  }else if(U[i] > c_q[4] & U[i]<= c_q[5]){
    rand.samples[i] = rnorm(1,b[5],w[5])
    S[i] = 5
  }else if(U[i] > c_q[5] & U[i]<= c_q[6]){
    rand.samples[i] = rnorm(1,b[6],w[6])
    S[i] = 6
  }else{(U[i] > c_q[6] & U[i]<= c_q[7])
    rand.samples[i] = rnorm(1,b[7],w[7])
    S[i] = 7
  }
}

x = log(rchisq(N,df = 1))
#Density plot of the random samples
plot(density(rand.samples),main="Density Estimate of the Mixture Model")
lines(density(x),col="red",lwd=2)
legend("topleft",c("True Density","Estimated Density"),col=c("red","black"),lwd=2)

