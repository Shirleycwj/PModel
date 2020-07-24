rm(list = ls())
setwd("/Users/wenjia/Desktop/PhD/20190415_C4/Global/analysis")

c <- read.csv("global_contribution_factor_1.0.csv",header = F)
c <- as.matrix(c)

c[which(c==-5)] <- 0
c[which(c==-4)] <- 0
c[which(c==-2)] <- -3  # tc(-)
c[which(c==-1)] <- -2  # CO2(-)
c[which(c==1)] <- -1  # CO2(+)
c[which(c==2)] <- 1  # tc(+)
c[which(c==3)] <- 2  # fAPAR(+)
c[which(c==4)] <- 3  # ppfd-radiation(+)
c[which(c==5)] <- 4  # soil moistrue(+)

c <- c *0.9

write.table(c,
            "/Users/wenjia/Desktop/PhD/20190415_C4/Global/analysis/global_contrib_2.0.csv",
            sep = ",",na="NaN",row.names = F,col.names = F)
