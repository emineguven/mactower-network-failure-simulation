#generate random numbers using Gompertz distribution 
#by using a technique which is called inverse transform:let U be a uniform random variable in[0,1]
#If X= F^-1(U), then X is a random variable with cumulative density function F_X(x)=F
#and then compare its fitting with Gompertz and Weibull model

 rgompertz = function(lambda,alpha, n){
  x.uniform = runif(n)
  #this inverse function is done based on F_X(x)=1-e^(-lambda.x) 
  #then for every number u generated with a uniform random number genrator,
  #x= F^-1(u) where F=F_X.F^-1(u)=-1/lambda.ln(1-u)
  inverse.gomp.cumulative = function(lambda,alpha,y) {  (1/alpha)*log(1 - (alpha/lambda)*log(1-y)  ) }
  x.gompertz = inverse.gomp.cumulative(0.01,0.2, x.uniform)
  return(x.gompertz)
}

random_values_gompertz<-rgompertz(0.01,0.2,100)

plot(random_values_gompertz, type="o",col="red")
hist(random_values_gompertz,freq="FALSE")

 Weibull <- function(delta,beta,n)
 {
   U <- runif(n)
   X <- delta*(-log(1-U))^(1/beta)
   return(X)
 }
 
 random_values_Weibull <- Weibull(3000,2,100)
 par(new=T)
 plot(random_values_Weibull, type="o",col="blue")
 hist(random_values_Weibull)
 
 #check again; Let’s check if this is reasonable, by superimposing the plot of the density of a Weibull random
 #variable with the same parameters
 n <- 100
 beta <- 2
 delta <- 3000
 X <- Weibull(3000,2,n)
 hist(X,freq=FALSE)
 t <- seq(from=0, to=max(X)*1.5, by=1)
 f_w <- beta/delta*(t/delta)^(beta-1)*exp(-(t/delta)^beta)
 lines(t,f_w,col="blue")
 
 
 
 
 