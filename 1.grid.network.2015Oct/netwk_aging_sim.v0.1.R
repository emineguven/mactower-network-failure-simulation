
##########################################
#2015Dec 16, GetoptLong(). change name to 'netwk_aging_sim.v0.1.R'
#2015Dec14 '20151215-netsim-generic.R': a generic network simulation code with files names as input
#2015Oct13, use numeric lookup table for essential genes.

library(GetoptLong)

single_network_failure_v2 = function(lambda1, lambda2=lambda1/10, threshold=4, p, pairs, essenLookupTb ) {
  # single network failure simulation, 20151013Tue
  # lambda1: First exponential constant failure rate for edges with degree > threshold
  # lambda2: Second exponential constant failure rate for edges with degree <= threshold
  # threshold: degree threshold for lambda1 and lambda2
  # pairs: network in pairwide format, using numeric NOs 20151013
  # essenLookupTb: lookup table for essential and nonessential genes, numeric values 
  ## for debug:   lambda1 = 1/50; lambda2= lambda1/10; threshold=4; p=0.8
  
  inpairs = pairs[,3:4] #bookkeeping  
  names(inpairs) = c('No1','No2')
  
  #get connectivities per node
  degreeTb = data.frame( table(c(inpairs$No1, inpairs$No2)))
  names(degreeTb) = c("No", "degree")
  degreeTb$moduleAge = NA;
  
  for( i in 1:length(degreeTb[,1])){
    if ( essenLookupTb[ degreeTb$No[i] ]) { #essential node
      lambda = ifelse( degreeTb$degree[i] >= threshold, lambda1, lambda2)
      age = rexp( degreeTb$degree[i], rate=lambda ) #exponential age
      if(degreeTb$degree[i] >= threshold){
        active = runif(degreeTb$degree[i])  #uniform interaction stochasticity
        active = ifelse( active<=p, 1, NA  ) #pick active interactions
        if( sum(active, na.rm=T) > 0 ){ #there should be at least 1 active intxn
          age = age * active # only active interactions for modular age estimation
          degreeTb$moduleAge[i] = max(age, na.rm=T) #maximum intxn age is the module age
        } else {# when no active intxn is available 
          degreeTb$moduleAge[i] = 0; #this module is born dead.
        }
      } else { # for degree < threshold, no stochasticity is applied. 
        degreeTb$moduleAge[i] = max(age, na.rm=T) #maximum intxn age is the module age
      }
    } else {# non-essential node
      degreeTb$moduleAge[i] = NA 
    }
  }
  
  summary(degreeTb)
  currentNetworkAge = min(degreeTb$moduleAge, na.rm=T)
}
# Rscript netwk_aging_sim.v0.1.R -if1 net1/Degree4N1000_network.csv -if2 net1/net1/Degree4N1000_EssenLookupTb.csv -l1 0.002 -l2 0.0002 -dt 5 -p 1.0 -n 5  -op net1 -od net1

popSize = 1000;
outputdir = getwd();
outputprefix = '';
degreeThreshold = 5;
debug = 0;
GetoptLong(c(
  "inNetworkFile|if1=s", "input network file",
  "inLookupTbFile|if2=s", "input node lookuptable file",
  "degreeThreshold|dt=i", "node degree threhold to determine lamdba, optional, default 5",
  "lambda1|l1=f", "edge failure rate1 for node with degree < degreeThreshold ",
  "lambda2|l2=f", "edge failure rate2 for node with degree > degreeThreshold ",
  "probability|p=f", "binomial probability of edges being active ",
  "popSize|n=f", "population size, number of simulated networks, optional, default 1000",
  "outputdir|od=s", "output directory, optional, default current directory",
  "outputprefix|op=s", "prefix of outputfiles, optional, default NULL",
  "debug|d=i", "debug"
))
p=probability; 
list.files(path=outputdir )

essenLookupTb = read.csv(inNetworkFile);
essenLookupTb = essenLookupTb[,1];

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

popAges = numeric(popSize)
time1 = date()
j=1; count = 0; 
while ((j <= popSize) && ( count < popSize*30)) {
  count = count + 1;      
  print(paste("count=",count))
  currentNetworkAge = single_network_failure_v2(lambda1, lambda2, degreeThreshold, p, pairs, essenLookupTb)
  if (currentNetworkAge > 0) {
    popAges[j] = currentNetworkAge      
    j = j+1
  } 
}# end of j while-loop, population loop

timestamp = format(Sys.time(), "%Y%b%d_%H%M%S")
age.file.name=paste(outputprefix,"dt", degreeThreshold, "p", p, "L1", lambda1, 
                    "L2", lambda2,'n',popSize, "time",timestamp, "csv", sep="." )
full_age_file = paste( outputdir, '/', age.file.name, sep='')

write.csv( popAges, full_age_file, row.names=F)

