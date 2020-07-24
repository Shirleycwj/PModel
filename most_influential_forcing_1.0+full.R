rm(list = ls())
setwd("/Users/wenjia/Desktop/PhD/Research/20190415_C4/Global/annual_gpp")

CO2 <- read.csv("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/annual_co2.csv",header = T)

# read the data----
# gpp----
path <- "/Users/wenjia/Desktop/PhD/Research/20190415_C4/Global/annual_gpp"
fileNames <- dir()
##生成读取文件路径
filePath <- sapply(fileNames, function(x){ 
  paste(path,x,sep='/')})  
##读取数据，结果为list
gpp <- lapply(filePath, function(x){
  read.csv(x, header=F)})  
# tc----
path <- "/Volumes/兔子/研究生学习/Projects/20180620_global/Data/Tmp/annual"
fileNames <- dir()
##生成读取文件路径
filePath <- sapply(fileNames, function(x){ 
  paste(path,x,sep='/')})  
##读取数据，结果为list
tc <- lapply(filePath, function(x){
  read.csv(x, header=F)}) 
# fAPAR----
path <- "/Volumes/兔子/研究生学习/Projects/20180620_global/Data/fAPAR/annual"
fileNames <- dir()
##生成读取文件路径
filePath <- sapply(fileNames, function(x){ 
  paste(path,x,sep='/')})  
##读取数据，结果为list
fAPAR <- lapply(filePath, function(x){
  read.csv(x, header=F)}) 
# ppfd----
path <- "/Volumes/兔子/研究生学习/Projects/20180620_global/Data/ppfd/annual_ppfd"
fileNames <- dir()
##生成读取文件路径
filePath <- sapply(fileNames, function(x){ 
  paste(path,x,sep='/')})  
##读取数据，结果为list
ppfd <- lapply(filePath, function(x){
  read.csv(x, header=F)}) 
# sw_stress----
path <- "/Users/wenjia/Desktop/PhD/Research/20190415_C4/Global/annual_sms"
fileNames <- dir()
##生成读取文件路径
filePath <- sapply(fileNames, function(x){ 
  paste(path,x,sep='/')})  
##读取数据，结果为list
sw_stress <- lapply(filePath, function(x){
  read.csv(x, header=F)}) 
# vpd----
path <- "/Users/wenjia/Desktop/PhD/Research/sensitivity_GPP/GPP_VPD/ann_vpd"
fileNames <- dir()
##生成读取文件路径
filePath <- sapply(fileNames, function(x){ 
  paste(path,x,sep='/')})  
##读取数据，结果为list
vpd <- lapply(filePath, function(x){
  read.csv(x, header=F)}) 


# initiate result map----
result <- matrix(rep(0,259200),nrow = 360,ncol = 720)

fac <- c("CO2","tc","vpd","fAPAR","ppfd","sw_stress","Residuals")

# start grid-run----
for (r in 1:360) {
  for (c in 1:720) {
    # initiate matrix for multiple regression
    g_data <- matrix(rep(0,245),nrow = 35,ncol = 7)
    g_data <- as.data.frame(g_data)
    names(g_data) <- c("gpp","CO2","tc","vpd","fAPAR","ppfd","sw_stress")
    for (i in 1:35) {
      g_data$gpp[i] <- gpp[[i]][r,c]
      g_data$CO2[i] <- CO2$V2[i]
      g_data$tc[i] <- tc[[i]][r,c]
      g_data$vpd[i] <- vpd[[i]][r,c]
      g_data$fAPAR[i] <- fAPAR[[i]][r,c]
      g_data$ppfd[i] <- ppfd[[i]][r,c]
      g_data$sw_stress[i] <- sw_stress[[i]][r,c]
    }
    
    if (anyNA(g_data)==F) {
      
      fit <- lm(gpp~CO2+tc+fAPAR+ppfd+vpd+sw_stress,data = g_data)
      
      # most important forcing----
      af <- anova(fit)
      afss <- af$"Sum Sq"
      af <- cbind(af,PctExp=afss/sum(afss)*100)
      
      if (row.names(af)[which(af$PctExp==max(af$PctExp))]=="Residuals"){
        
        no_maxfac <- which(fac==row.names(af)[which(af$PctExp==max(af$PctExp[1:(nrow(af)-1)]))])
        
        if (summary(fit)$coefficients[which(names(summary(fit)$coefficients[,1])==fac[no_maxfac]),1]<0) {
          no_maxfac <- no_maxfac * (-1)
        }
      } else {
        no_maxfac <- which(fac==row.names(af)[which(af$PctExp==max(af$PctExp))])
        if (summary(fit)$coefficients[which(names(summary(fit )$coefficients[,1])==fac[no_maxfac]),1]<0) {
          no_maxfac <- no_maxfac * (-1)
        }
      }
      result[r,c] <- no_maxfac
      
    } else {
      result[r,c] <- NA
    }
    
  }
  print(paste(r,Sys.time(),sep = ","))
}

write.table(result,
            "/Users/wenjia/Desktop/PhD/Research/paper/global_contribution_factor_full_1.0.csv",
            sep = ",",na="NaN",row.names = F,col.names = F)

