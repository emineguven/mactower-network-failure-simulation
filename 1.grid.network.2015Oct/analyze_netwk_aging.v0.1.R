
##########################################
# 2015Dec 16. 
# Plan, loopover netwk-aging files in a directory and generate a report file
# Rscript analyze_netwk_aging.v0.1.R --inputdir net1 --inputprefix net1 -of __test.csv -pf __test.pdf

rm(list=ls())

source('network.r')
library(GetoptLong)

inputdir = getwd(); # inputdir = 'net1'
inputprefix = ''; # inputprefix = 'net1'
outputFile = "_tmp_networkaging_summary.csv";
outputplotFile = "_tmp_networkaging_plot.pdf";
debug = 0;
GetoptLong(c(
  "inputdir|id=s", "input dir of network aging data in csv format. default: getwd()",
  "inputprefix|ip=s", "prefix of input netwk aging csv data files. default empty",
  "outputFile|of=s", "output csv file name",
  "outputplotFile|pf=s", "output figure file name",
  "debug|d=i", "debug. default 0"
))

# inputdir = 'net1'; inputprefix = 'net1'; i = 1
myfiles = list.files(inputdir)
myfiles = myfiles[ grep(inputprefix, myfiles) ]

out = summarize_mean_from_files( myfiles, inputdir )
#timestamp = format(Sys.time(), "%Y%b%d_%H%M%S")
write.csv( out, outputFile, row.names=F)

source('lifespan.r')
pdf(outputplotFile)

for( i in 1:length(myfiles)) {
  currentFile = paste( inputdir, '/', myfiles[i], sep='');
  tbLS = read.csv(currentFile)
  tb = calculate.mortality.rate(tbLS[,1])
  plot( tb$s ~ tb$t, main = currentFile)
  plot( tb$mortality.rate ~ tb$t,  main = currentFile )
  plot( tb$mortality.rate ~ tb$t, log='y', main='linear for Gompertz' )
  plot( tb$mortality.rate ~ tb$t, log='yx', main='linear for Weibull' )
}

dev.off()

