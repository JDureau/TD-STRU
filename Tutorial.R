# TD

# IMPORTANT: Set your working directory to TD-STRU

library(ggplot2)
library("RJSONIO")
source('RSSM.R')



ROOT = getwd() 

# Set the path of your model
pathSIRAmiens = paste(ROOT,"/SIR-Amiens",sep="")
pathSIRSAmiens = paste(ROOT,"/SIRS-Amiens",sep="")




ssm.plot.X("/bin/X_1.csv",pathSIRAmiens);
ssm.plot.hat("/bin/hat_1.csv",pathSIRAmiens,0);
ssm.plot.trace("/bin/trace_0.csv",pathSIseas);