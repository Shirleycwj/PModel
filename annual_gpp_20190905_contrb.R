rm(list = ls());
setwd("/Users/wenjia/Desktop/PhD/Research/sensitivity_GPP/GPP_VPD/mon_cli_gpp")
fn <- dir()

for (f in seq(from=1,to=409,by=12)) {
  path <- "/Users/wenjia/Desktop/PhD/Research/sensitivity_GPP/GPP_VPD/mon_cli_gpp"
  fileNames <- fn[f:(f+11)]
  ##生成读取文件路径
  filePath <- sapply(fileNames, function(x){ 
    paste(path,x,sep='/')})  
  ##读取数据，结果为list
  data <- lapply(filePath, function(x){
    read.csv(x, header=F)}) 
  
  gpp_annual <- matrix(rep(0,259200),nrow = 360,ncol = 720)

  for (r in 1:360) {
    for (c in 1:720) {
      an <- c(data[[1]][r,c],data[[2]][r,c],data[[3]][r,c],data[[4]][r,c],
              data[[5]][r,c],data[[6]][r,c],data[[7]][r,c],data[[8]][r,c],
              data[[9]][r,c],data[[10]][r,c],data[[11]][r,c],data[[12]][r,c])
      if (sum(is.na(an))<12) {
        an[an=="NaN"] <- 0
        gpp_annual[r,c] <- sum(an) * 30
      } else if (sum(is.na(an))==12) {
        gpp_annual[r,c] <- sum(an) * 30
      }

    }
  }
  
  # gpp_annual <- (Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec) *30
  
  out_file <- paste("/Users/wenjia/Desktop/PhD/Research/sensitivity_GPP/GPP_VPD/ann_cli_gpp/",substr(fn[f],1,4),".csv",sep = "")
  write.table(gpp_annual,out_file,sep = ",",na="NaN",row.names = F,col.names = F)
  
  print(paste(substr(fn[f],1,4),Sys.time(),sep = ",")) 
}
  