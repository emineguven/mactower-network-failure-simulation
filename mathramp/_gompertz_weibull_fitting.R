# generate gompertz random numbers
# fit with flexsurv Gompertz and weibull models

#inverse of gompertz CDF
# see http://hongqinlab.blogspot.com/2013/06/median-lifespan-of-2-parameter-gompertz.html
inverse.gomp.CDF = function(R,G,y) {  (1/G)*log(1 - (G/R)*log(1-y)  ) }

#see generate random number with a given distribution
# http://hongqinlab.blogspot.com/2013/12/generate-gompertz-random-numbers.html

x.uniform = runif(60)
hist(x.uniform)

x.gompertz = inverse.gomp.CDF(0.001,0.2, x.uniform)
hist(x.gompertz)

summary(x.gompertz)

#source("lifespan.r")


#my.data is x.gompertz which is 
calculate.s = function( lifespan ){
  my.data = sort( lifespan[!is.na(lifespan)] );
  tmptb = table( my.data )
  for( i in 2:length(tmptb)) {
    tmptb[i] = tmptb[i-1] + tmptb[i]    		} 	 
  tot = length(my.data)
  tmptb = tmptb / tot; 
  s = 1 - tmptb
  #list( s=s, t=unique(my.data));
  ret = data.frame( cbind(s, unique(my.data)));
  names(ret) = c("s", "t");
  ret;
}


tb = calculate.s(x.gompertz)
plot(tb$s ~ tb$t)

require(flexsurv)
require(flexsurv)
lifespan = x.gompertz
lifespanGomp = flexsurvreg(formula = Surv(lifespan) ~ 1, dist = 'gompertz')  
lifespanWeib = flexsurvreg(formula = Surv(lifespan) ~ 1, dist = 'weibull')  

c(lifespanWeib$AIC, lifespanGomp$AIC, lifespanWeib$AIC - lifespanGomp$AIC )


#calculate mortality rate
calculate.mortality.rate = function( lifespan ){
  tb = calculate.s(lifespan)
  tb$ds=NA; tb$dt=NA
  #first point
  tb$dt[1] = tb$s[1]
  tb$ds[1] = 1 - tb$s[1]
  tb$mortality.rate[1] = tb$ds[1] / tb$dt[1]
  
  for( j in 2:length(tb[,1])) {
    tb$ds[j] =  tb$s[j-1] - tb$s[j] 
    tb$dt[j] = -tb$t[j-1] + tb$t[j]
    tb$mortality.rate[j] = tb$ds[j] / ( tb$s[j] * tb$dt[j])
  }
  
}//end of calculate.mortality.rate()


lifespan = round( tb.ori[,1], digits=1)
table(lifespan)
tb = calculate.s(lifespan)
head(tb)
tb$ds=NA; tb$dt=NA
tb$dt[1] = tb$s[1]
tb$ds[1] = 1 - tb$s[1]
tb$mortality.rate[1] = tb$ds[1] / tb$dt[1]

for( j in 2:length(tb[,1])) {
  tb$ds[j] =  tb$s[j-1] - tb$s[j] 
  tb$dt[j] = -tb$t[j-1] + tb$t[j]
  tb$mortality.rate[j] = tb$ds[j] / ( tb$s[j] * tb$dt[j])
}
plot( tb$s ~ tb$t)
plot( tb$mortality.rate ~ tb$t, typ='l', log='y' )

plot( log10(tb$mortality.rate) ~ tb$t, type='l') #linear for Gompertz, semi-log plot
plot( log10(tb$mortality.rate) ~ log10(tb$t), type='l'  ) #linear for Weibull, log-log plot

#the first and last point of mortality rate are very low, presumbaly due to boundary effect.?!



###############old file

title(datafiles[i])
title(datafiles[i])

tb2 = tb 
tb2 = tb2[-length(tb2[,1]), ]


summary(lm(log10(tb2$mortality.rate) ~ tb2$t ))

summary(lm(log10(tb2$mortality.rate) ~ log10(tb2$t) ))

require(flexsurv)
lifespanGomp = flexsurvreg(formula = Surv(lifespan) ~ 1, dist = 'gompertz') ### Use the flexsurvreg package to fit lifespan data to gompertz or weibull distribution
lifespanWeib = flexsurvreg(formula = Surv(lifespan) ~ 1, dist = 'weibull')  

c(lifespanWeib$AIC, lifespanGomp$AIC, lifespanWeib$AIC - lifespanGomp$AIC )




