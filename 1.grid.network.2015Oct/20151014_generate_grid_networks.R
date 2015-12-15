
##########################################
# generate gride network for simulation and quality control

rm(list=ls())
debug = 0; 

# R -f file --args                         Degree numOfEssenNode outNetworkFile outEssenLookupTbFile 
# R -f 20151014_generate_grid_networks.R --args 4 1000           Degree4N1000_network.csv Degree4N1000_EssenLookupTb.csv

options(echo=TRUE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)
print(args)
degree = as.numeric(args[1]); degree;
numOfEssenNodes = as.numeric(args[2]); numOfEssenNodes;
outNetworkFile = args[3]
outEssenLookupTbFile = args[4]

#for debug 
# degree=4; numOfEssenNodes=1000; outNetworkFile = "__test.csv"; outEssenLookupTb="__EssenLookupTb.csv"

pairs = data.frame(integer(numOfEssenNodes*degree), integer(numOfEssenNodes*degree))
names(pairs) = c("No1","No2")

for( i in 1:numOfEssenNodes) {
  current_i_row = (i-1)*degree + 1
  for( j in 1:degree){
     pairs$No1[current_i_row +j-1] = i;
     pairs$No2[current_i_row +j-1] = current_i_row +j-1 + numOfEssenNodes
   }
}
# check
table( pairs$No1)[1:10]
table( pairs$No2)[1:10]
length(unique(pairs[,1]))
length(unique(pairs[,2]))


write.csv(pairs, outNetworkFile, row.names = F)

outEssenLookupTb = c(rep(1, numOfEssenNodes), rep(0, numOfEssenNodes*degree))
write.csv(outEssenLookupTb, outEssenLookupTbFile, row.names = T)



