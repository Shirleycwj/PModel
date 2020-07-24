rm(list = ls())
library(mblm)

setwd("/Users/wenjia/Desktop/PhD/Research/20190415_C4/Global/annual_gpp")

# read annual gpp data
annualgpp_pathway <- getwd()
fileNames <- dir(annualgpp_pathway) 
filePath <- sapply(fileNames, function(x){ 
  paste(annualgpp_pathway,x,sep='/')})   
data <- lapply(filePath, function(x){
  read.csv(x, header=F)})

# build the result grid map
spa_gpp_trend <- matrix(NA,nrow = 360, ncol = 720)
spa_gpp_percent <- matrix(NA,nrow = 360, ncol = 720)

for (r in 1:360) {
  for (c in 1:720) {
    # setting up annual gpp table
    gpp_annual <- data.frame("year" = as.numeric(substr(names(data),1,4)), 
                             "gpp" = 1:35 )
    for (y in 1:35) {
      gpp_annual$gpp[y] <- data[[y]][r,c]
    }
    
    # calculate the trend grid by grid
    if (anyNA(gpp_annual$gpp)==T) {
      spa_gpp_trend[r,c] <- NA
    } else {
      mod <- mblm(gpp~year, data = gpp_annual)
      spa_gpp_trend[r,c] <- summary(mod)$coefficients[2,1]
      
      spa_gpp_percent[r,c] <- as.numeric(mod$fitted.values[35]/mod$fitted.values[1]-1) * 100
    }
  }
}
write.table(spa_gpp_trend,
            "/Users/wenjia/Desktop/PhD/Research/20190415_C4/Global/spatial_gpp_trend_mblm.csv",
            sep = ",",na="NaN",row.names = F,col.names = F)
write.table(spa_gpp_percent,
            "/Users/wenjia/Desktop/PhD/Research/20190415_C4/Global/spatial_gpp_trend_percent.csv",
            sep = ",",na="NaN",row.names = F,col.names = F)
