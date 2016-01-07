

Rscript identify_lambda1.v0.0.R -if1 merged_PPIGIN_Factorized2015Oct132Cols.csv -if2 essentialGeneLookupTable_20151013.csv -l1 0.0023 -l2 0.0002 -dt 4 -p 0.9 -n 100  -op ginppi -od ginppi.ori.out -tLS 30 -ml 20 -cs 1.05 -el 0.5


Rscript netwk_aging_sim.v0.1b.R -if1 merged_PPIGIN_Factorized2015Oct132Cols.csv -if2 essentialGeneLookupTable_20151013.csv -l1 0.0023 -l2 0 -dt 4 -p 0.9 -n 1000  -op ginppi -od ginppi.ori.out -iC 4 -d 1 -h /Users/hqin/github/mactower-network-failure-simulation/1.ori.ginppit.2015Oct

time
Rscript netwk_aging_sim.v0.1b.R -if1 merged_PPIGIN_Factorized2015Oct132Cols.csv -if2 essentialGeneLookupTable_20151013.csv -l1 0.0023 -l2 0 -dt 4 -p 0.9 -n 1000  -op __TEST -od  -iC 4 -d 1 -h /Users/hqin/github/mactower-network-failure-simulation/1.ori.ginppi.2015Oct


Rscript analyze_netwk_aging.v0.1.R --inputdir ginppi.ori.out --inputprefix ginppi -of __ori_out.csv -pf __ori_out.pdf
