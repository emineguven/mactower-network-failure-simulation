20151014_generate_grid_networks.R

net1/ #simulated grid network
Degree4N1000_EssenLookupTb.csv  #containing rowname info
Degree4N1000_network.csv



time R -f 20151014_generate_grid_networks.R --args 5 1000 Degree5N1000_network.csv Degree5N1000_EssenLookupTb.csv
