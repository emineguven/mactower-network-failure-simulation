#generate random numbers using Gompertz distribution 
#by using a technique which is called inverse transform:let U be a uniform random variable in[0,1]
#If X= F^-1(U), then X is a random variable with cumulative density function F_X(x)=F
#and then compare its fitting with Gompertz and Weibull model

 Weibull <- function(delta,beta,n)
 {
   U <- runif(n)
   X <- delta*(-log(1-U))^(1/beta)
   return(X)
 }
 
 
 #random_values_Weibull <- Weibull(3000,2,100)
 #par(new=T)
# plot(random_values_Weibull, type="o",col="blue")
 #hist(random_values_Weibull)
 
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
 #par(new=T)
 plot(t,f_w,col="blue",type="o")
 
 #lambda>0 and alpha>1
 Gompertz <-function(lambda,alpha,n)
 {
   U<-runif(n)
   Y<- -(log(lambda)-log(lambda-log(1-U)*log(alpha)))/log(alpha)
   return(Y)
 }
 
 random_values_Gompertz <- Gompertz(0.01,2,100)
 plot(random_values_Gompertz, type="o",col="red")
 
 #check again; Let’s check if this is reasonable, by superimposing the plot of the density of a Weibull random
 #variable with the same parameters
 n <- 100
 lambda <- 0.01
 alpha<-2
 Y <- Gompertz(0.01,2,n)
 hist(Y,freq=FALSE)
 #hist(random_values_gompertz,freq="FALSE")
 t <- seq(from=0, to=max(Y)*1.5, by=1)
 f_g <- lambda*alpha^t*exp(-lambda*((alpha^t)-1)/log(alpha))
 lines(t,f_g,col="blue")
 plot(t,f_g,col="blue",type="o")
 
 
 
 