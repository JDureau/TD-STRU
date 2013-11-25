# TD

# IMPORTANT: Set your working directory to TD-STRU

library(ggplot2)
library("RJSONIO")
source('RSSM.R')



ROOT = getwd() 

pathSIR = paste(ROOT,"/SIR",sep="")

ssm.plot.hat("/bin/hat_0.csv",pathSIR);
