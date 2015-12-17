##########################################
# 2015Dec 16. 
# Plan, simulated a networking aging, calculate its mean lifespan, change lambda

# Rscript identify_lambda1.v0.0.R -if1 net1/Degree4N1000_network.csv -if2 net1/Degree4N1000_EssenLookupTb.csv -l1 0.005 -l2 0.0002 -dt 0 -p 1.0 -n 10  -op net1 -od net1 -tLS 30 -ml 20 -cs 1.2

rm(list=ls())
source('network.r')
library(GetoptLong)

popSize = 100; outputdir = getwd(); outputprefix = ''; degreeThreshold = 5; debug = 0;
maxIterations = 20; errorLimit = 1; currentScale = 1.1; 
GetoptLong(c(
  "inNetworkFile|if1=s", "input network file",
  "inLookupTbFile|if2=s", "input node lookuptable file",
  "targetMeanLS|tLS=f", "target mean lifespan",
  "errorLimit|el=f", "error limit for the mean lifespan",
  "maxIterations|ml=i", "number of iterations",
  "currentScale|cs=f", "scale for the jump step",
  "degreeThreshold|dt=i", "node degree threhold to determine lamdba, optional, default 5",
  "lambda1|l1=f", "edge failure rate1 for node with degree >= degreeThreshold ",
  "lambda2|l2=f", "edge failure rate2 for node with degree < degreeThreshold ",
  "probability|p=f", "binomial probability of edges being active ",
  "popSize|n=f", "population size, number of simulated networks, default 100",
  "outputdir|od=s", "output directory, optional, default current directory",
  "outputprefix|op=s", "prefix of outputfiles, optional, default NULL",
  "debug|d=i", "debug"
))
p=probability; 

essenLookupTb = read.csv(inLookupTbFile, row.names=1);
essenLookupTb = as.vector(essenLookupTb[,1]); #20151215 after update to R3.2.3

pairs = read.csv(inNetworkFile); 
names(pairs) = c("No1", "No2")
print(head(pairs))
if(debug==9) {     pairs = pairs[1:1000,]  }

count = 0; 
currentLambda = lambda1; 
currentError = errorLimit* 10;

while (( count <= maxIterations ) && ( abs(currentError) > errorLimit)) {
  count = count + 1;      
  print(paste("(identify_lambda)count=",count))
  network_ages = multiple_network_failure(popSize,currentLambda, lambda2, degreeThreshold, p, pairs, essenLookupTb)
  print ( c("(identify_lambda) current mean netwk ages:", mean(network_ages) ));
  currentError = mean(network_ages) - targetMeanLS
  if(  currentError > 0) { 
    currentLambda = currentLambda * currentScale
  }else {
    currentLambda = currentLambda / currentScale
  }
  print(c("(identify_lambda) currentError", currentError, "currentLambda1", currentLambda))
}# end of while loop


