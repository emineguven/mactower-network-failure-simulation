rm(list=ls())

source("network.r")

net1 = read.csv("48states.csv")

x = ms02_singlerun_v2(net1, indebug=1 )
y = ms02_singlerun_v2(x, indebug=1 )
z = ms02_singlerun(y, indebug=1 )

tx = table(c( x[,1], x[,2]) )
ty = table(c( y[,1], y[,2]) )
table( tx==ty )

tz = table(c( z[,1], z[,2]) )
table( tz==ty )

write.csv(z, "z.csv")

#double-check

as.matrix(net1)
net2 = t(apply(net1, 1, sort))  #order ids
net2 = data.frame(net2)
net2$tag =  paste(net2[,1], net2[,2], sep="_")
counts = table( net2$tag )
net2$tag_counts = counts[net2$tag]
net2

