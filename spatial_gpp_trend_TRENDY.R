rm(list = ls())
library(mblm)
library(stringr)

fname <- dir("/Users/wenjia/Desktop/TRENDY-v8_result")
nmod <- fname[which(str_sub(fname,-6,-1)=="S2_gpp")]

for (i in 1:length(nmod)) {
  path <- paste("/Users/wenjia/Desktop/TRENDY-v8_result/",nmod[i],sep = "")
  setwd(path)
  
  # read annual gpp data
  annualgpp_pathway <- getwd()
  fileNames <- dir(annualgpp_pathway)[(length(dir(annualgpp_pathway))-34):length(dir(annualgpp_pathway))] 
  filePath <- sapply(fileNames, function(x){ 
    paste(annualgpp_pathway,x,sep='/')})   
  data <- lapply(filePath, function(x){
    read.csv(x, header=F)})
  
  # build the result grid map
  nr = nrow(data[[1]])
  nc = ncol(data[[1]])
  spa_gpp_trend <- matrix(NA,nrow = nr, ncol = nc)
  # spa_gpp_percent <- matrix(NA,nrow = nr, ncol = nc)
  
  for (r in 1:nr) {
    for (c in 1:nc) {
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
        
        # spa_gpp_percent[r,c] <- as.numeric(mod$fitted.values[35]/mod$fitted.values[1]-1) * 100
      }
    }
  }
  write.table(spa_gpp_trend,
              paste("/Users/wenjia/OneDrive - Imperial College London/PhD results/TRENDY_v8/trend/spatial/",str_sub(nmod[i],1,-5),"_mblm.csv",sep = ""),
              sep = ",",na="NaN",row.names = F,col.names = F)
  # write.table(spa_gpp_percent,
  #             paste("/Users/wenjia/OneDrive - Imperial College London/PhD results/trendy_v6/TRENDY_spatial_trend/gpp_percent_8216/", nmod[i],"_mblm_pct.csv",sep = ""),
  #             sep = ",",na="NaN",row.names = F,col.names = F)
}




