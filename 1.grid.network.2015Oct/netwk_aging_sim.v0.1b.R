
##########################################
#2015Dec 30, Try to add parallel feature using foreach and doMC.  
# paralle run can be done in GUI but not in commandline version? 

#2015Dec 16, GetoptLong(). change name to 'netwk_aging_sim.v0.1.R'
#2015Dec14 '20151215-netsim-generic.R': a generic network simulation code with files names as input
#2015Oct13, use numeric lookup table for essential genes.

rm(list=ls())
source( 'network.r' )

library(foreach)
library(doMC)
library(GetoptLong)

# Rscript netwk_aging_sim.v0.1b.R -if1 net1/Degree4N1000_network.csv -if2 net1/Degree4N1000_EssenLookupTb.csv -l1 0.006 -l2 0.0002 -dt 0 -p 0.9 -n 50  -op net1 -od net1 -iC 4 -d 1

tmp = "
inNetworkFile = 'net1/Degree4N1000_network.csv'
inLookupTbFile = 'net1/Degree4N1000_EssenLookupTb.csv'
lambda1 = 0.006
lambda2 = lambda1/10
p=1.0
popSize = 100;
inputCores=4
degreeThreshold = 0;
"

popSize = 100;
outputdir = getwd();
outputprefix = '';
degreeThreshold = 0;
debug = 0;
inputCores = 2; 
GetoptLong(c(
  "inNetworkFile|if1=s", "input network file",
  "inLookupTbFile|if2=s", "input node lookuptable file",
  "degreeThreshold|dt=i", "node degree threhold to determine lamdba, optional, default 0 to use only lamdba1",
  "lambda1|l1=f", "edge failure rate1 for node with degree >= degreeThreshold ",
  "lambda2|l2=f", "edge failure rate2 for node with degree < degreeThreshold ",
  "probability|p=f", "binomial probability of edges being active ",
  "popSize|n=i", "population size, number of simulated networks, default 100",
  "outputdir|od=s", "output directory, optional, default current directory",
  "outputprefix|op=s", "prefix of outputfiles, optional, default NULL",
  "debug|d=i", "debug",
  "inputCores|iC=i", "number of CPU cores, default=2"
))
p=probability; 
list.files(path=outputdir )
registerDoMC(cores=inputCores)

essenLookupTb = read.csv(inLookupTbFile, row.names=1);
essenLookupTb = as.vector(essenLookupTb[,1]); #20151215 after update to R3.2.3

pairs = read.csv(inNetworkFile); 
names(pairs) = c("No1", "No2")
print(head(pairs))
if(debug==9) {     pairs = pairs[1:1000,]  }
pairs = pairs[ pairs$No1 != pairs$No2, ]  
# label essential nodes, remove nonesse-nonessen pairs
pairs$essen1 = essenLookupTb[pairs$No1]
pairs$essen2 = essenLookupTb[pairs$No2]
#remove nonessen <-> nonessen intxn because they do not affect aging. 
pairs$remove = ifelse( pairs$essen1==F & pairs$essen2==F, T, F  )
pairs= pairs[! pairs$remove, ]  

#get connectivities per node
degreeTb = data.frame( table(c(pairs$No1, pairs$No2)))
summary(degreeTb); 
degreeTb[1:10,]
summary(degreeTb)

popAges = numeric(popSize)
time1 = date()
j=1; count = 0; 
while ((j <= popSize) && ( count < popSize*5)) {
  myParrallStep = inputCores * 5
  bufferAges = foreach(i=1:myParrallStep, .combine = 'rbind') %dopar% {
    currentNetworkAge = single_network_failure_v2(lambda1, lambda2, degreeThreshold, p, pairs, essenLookupTb)
  }
  goodAges = bufferAges [bufferAges>0] #goodAges() return NULL? 
  count = count + myParrallStep
  if( length(goodAges)> 0 ){ 
    currentEnd = j + length(goodAges)-1
    popAges[j:currentEnd] = goodAges
    j = currentEnd + 1
  } else {
    print(paste("newk_aging_sim:: goodAges", goodAges))
  }
  if(debug>2) { 
    print(paste("netwk_aging_sim::bufferAges=",bufferAges))
  }
  if(debug) { 
    print(paste("netwk_aging_sim::bufferAges=",bufferAges))
    print(paste("netwk_aging_sim::count=",count, "  j=", j))
  } 
}# end of j while-loop, population loop

popAges = popAges[1:popSize]

timestamp = format(Sys.time(), "%Y%b%d_%H%M%S")
age.file.name=paste(outputprefix,"dt", degreeThreshold, "p", p, "L1", lambda1, 
                    "L2", lambda2,'n',popSize, "time",timestamp, "csv", sep="." )
full_age_file = paste( outputdir, '/', age.file.name, sep='')

write.csv( popAges, full_age_file, row.names=F)

