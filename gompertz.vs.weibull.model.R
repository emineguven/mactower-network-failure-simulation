#1)generate random numbers using Gompertz distribution 
#by using a technique which is called inverse transform:let U be a uniform random variable in[0,1]
#If X= F^-1(U), then X is a random variable with cumulative density function F_X(x)=F

#2)and then compare its fitting with Gompertz and Weibull model

#3)calculate the mortality rates he mortality rate change over time, and then plot log(mortality rate) ~ time.
#Gompertz model should give a linear form.  
#For Weibull model, log(moretality rate) ~ log(time) give the linear form.


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




summary(Y)
summary(X)


#source("lifespan.r")

#2)and then compare its fitting with Gompertz and Weibull model

#my.data is x.gompertz which is randomly generated data coming from inverse cdf of gompertz
# calculate.s function is taken from HQin lifespan.r script
# there are also other function in there that might be useful for later.

calculate.s = function( lifespan ){
  my.data = sort( lifespan[!is.na(lifespan)] );
  tmptb = table( my.data )
  for( i in 2:length(tmptb)) {
    tmptb[i] = tmptb[i-1] + tmptb[i]      	} 	 
  tot = length(my.data)
  tmptb = tmptb / tot; 
  s = 1 - tmptb
  #list( s=s, t=unique(my.data));
  ret = data.frame( cbind(s, unique(my.data)));
  names(ret) = c("s", "t");
  ret;
}


tb = calculate.s(Y)
plot(tb$s ~ tb$t)

require(flexsurv)
require(flexsurv)
lifespan = Y
lifespanGomp = flexsurvreg(formula = Surv(lifespan) ~ 1, dist = 'gompertz')  
lifespanWeib = flexsurvreg(formula = Surv(lifespan) ~ 1, dist = 'weibull')  

c(lifespanWeib$AIC, lifespanGomp$AIC, lifespanWeib$AIC - lifespanGomp$AIC )


#3)calculate the mortality rates he mortality rate change over time, and then plot log(mortality rate) ~ time.
#Gompertz model should give a linear form.  
#For Weibull model, log(moretality rate) ~ log(time) give the linear form.

#HQin's calculate mortality rate function to calculate the rate of mortality over time
calculate.mortality.rate = function( lifespan ){
  tb = calculate.s(lifespan)
  tb$ds=NA; tb$dt=NA
  #first point
  tb$dt[1] = tb$t[2]
  tb$ds[1] = 1 - tb$s[1]
  tb$mortality.rate[1] = tb$ds[1] / tb$dt[1]
  
  for( j in 2:length(tb[,1])) {
    tb$ds[j] =  tb$s[j-1] - tb$s[j] 
    tb$dt[j] = -tb$t[j-1] + tb$t[j]
    tb$mortality.rate[j] = tb$ds[j] / ( tb$s[j] * tb$dt[j])
  }
  return(tb)
} #end of calculate.mortality.rate()

tb = calculate.mortality.rate(lifespan)



tb = calculate.s(round( Y, digits=1))
head(tb)
tb$ds=NA; tb$dt=NA
tb$dt[1] = tb$t[2] #20130321 correct a bug tb$s -> tb$t
tb$ds[1] = 1 - tb$s[1]
tb$mortality.rate[1] = tb$ds[1] / tb$dt[1]

for( j in 2:length(tb[,1])) {
  tb$ds[j] =  tb$s[j-1] - tb$s[j] 
  tb$dt[j] = -tb$t[j-1] + tb$t[j]
  tb$mortality.rate[j] = tb$ds[j] / ( tb$s[j] * tb$dt[j])
}
plot( tb$s ~ tb$t)
plot( tb$mortality.rate ~ tb$t, typ='l', log='y' )


#then plot log(mortality rate) ~ time.
#For Weibull model, log(moretality rate) ~ log(time) give the linear form.

plot( log10(tb$mortality.rate) ~ tb$t, type='l') #linear for Gompertz, semi-log plot
plot( log10(tb$mortality.rate) ~ log10(tb$t), type='l'  ) #linear for Weibull, log-log plot

#the first point was very low because it is a bug.
#and last point of mortality rate are very low, presumbaly due to boundary effect.


tb2 = tb 
tb2 = tb2[-length(tb2[,1]), ]


summary(lm(log10(tb2$mortality.rate) ~ tb2$t ))

summary(lm(log10(tb2$mortality.rate) ~ log10(tb2$t) ))