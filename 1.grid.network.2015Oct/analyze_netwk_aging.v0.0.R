
##########################################
# 2015Dec 16. 
# Plan, loopover netwk-aging files in a directory and generate a report file
# Rscript analyze_netwk_aging.v0.0.R --inputdir net1 --inputprefix net1 -o __test.csv

rm(list=ls())

source('network.r')
library(GetoptLong)

inputdir = getwd(); 
inputprefix = '';
outputFile = "_tmp_networkaging_summary.csv";
debug = 0;
GetoptLong(c(
  "inputdir|id=s", "input dir of network aging data in csv format. default: getwd()",
  "inputprefix|ip=s", "prefix of input netwk aging csv data files. default empty",
  "outputFile|of=s", "output csv file name",
  "debug|d=i", "debug. default 0"
))

# inputdir = 'net1'; inputprefix = 'net1'; i = 1
myfiles = list.files(inputdir)
myfiles = myfiles[ grep(inputprefix, myfiles) ]

out = summarize_mean_from_files( myfiles, inputdir )
#timestamp = format(Sys.time(), "%Y%b%d_%H%M%S")
write.csv( out, outputFile, row.names=F)

