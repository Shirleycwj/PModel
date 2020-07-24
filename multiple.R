rm(list = ls())
setwd("/Users/shirley/Desktop/Projects/20180620_global/Results/annual_gpp_NA")

CO2 <- read.csv("/Users/shirley/Desktop/Projects/20180620_global/Data/annual_co2.csv",header = T)
fac <- c("CO2","tc","fAPAR","ppfd","sw_stress","Residuals")
full_fac <- read.csv("/Users/shirley/Desktop/Projects/20180620_global/Analysis/regrssion/multiple_regression_0806/multiple_regression.csv")
full_fac <- as.matrix(full_fac)
for (i in 1:259200) {
  if (is.na(full_fac[i,1])==F) {
    if (full_fac[i,1]<0) {
      full_fac[i,1] <- full_fac[i,1] * -1
    }
  }
}

# read the data----
  # gpp----
    path <- "/Users/shirley/Desktop/Projects/20180620_global/Results/annual_gpp_NA"
    fileNames <- dir()
    ##生成读取文件路径
    filePath <- sapply(fileNames, function(x){ 
    paste(path,x,sep='/')})  
    ##读取数据，结果为list
    gpp <- lapply(filePath, function(x){
    read.csv(x, header=F)})  
  # tc----
    path <- "/Users/shirley/Desktop/Projects/20180620_global/Data/Tmp/annual"
    fileNames <- dir()
    ##生成读取文件路径
    filePath <- sapply(fileNames, function(x){ 
      paste(path,x,sep='/')})  
    ##读取数据，结果为list
    tc <- lapply(filePath, function(x){
      read.csv(x, header=F)}) 
  # fAPAR----
    path <- "/Users/shirley/Desktop/Projects/20180620_global/Data/fAPAR/annual"
    fileNames <- dir()
    ##生成读取文件路径
    filePath <- sapply(fileNames, function(x){ 
      paste(path,x,sep='/')})  
    ##读取数据，结果为list
    fAPAR <- lapply(filePath, function(x){
      read.csv(x, header=F)}) 
  # ppfd----
    path <- "/Users/shirley/Desktop/Projects/20180620_global/Data/ppfd/annual_ppfd"
    fileNames <- dir()
    ##生成读取文件路径
    filePath <- sapply(fileNames, function(x){ 
      paste(path,x,sep='/')})  
    ##读取数据，结果为list
    ppfd <- lapply(filePath, function(x){
      read.csv(x, header=F)}) 
  # sw_stress----
    path <- "/Users/shirley/Desktop/Projects/20180620_global/Data/SPLASH/wn_alpha/annual_stress"
    fileNames <- dir()
    ##生成读取文件路径
    filePath <- sapply(fileNames, function(x){ 
      paste(path,x,sep='/')})  
    ##读取数据，结果为list
    sw_stress <- lapply(filePath, function(x){
      read.csv(x, header=F)}) 


      
# initiate result map----
  result <- matrix(rep(0,259200),nrow = 360,ncol = 720)
  # 
  # np_CO2 <- matrix(rep(0,259200),nrow = 360,ncol = 720)
  # np_tc <- matrix(rep(0,259200),nrow = 360,ncol = 720)
  # np_fAPAR <- matrix(rep(0,259200),nrow = 360,ncol = 720)
  # np_ppfd <- matrix(rep(0,259200),nrow = 360,ncol = 720)
  # np_drought <- matrix(rep(0,259200),nrow = 360,ncol = 720)
  # np_d_p <- matrix(rep(0,259200),nrow = 360,ncol = 720)
  # np_list <- list(CO2=np_CO2,tc=np_tc,fAPAR=np_fAPAR,ppfd=np_ppfd,sw_stress=np_drought,sw_p=np_d_p)
    
  # full_fac <- matrix(rep(NA,2851200),nrow = 259200,ncol = 11)
  # full_fac <- as.data.frame(full_fac)
  # names(full_fac) <- c("cd_s","cd_p","tc_s","tc_p","f_s","f_p","pp_s","pp_p","sw_s","sw_p","R2")
 
# start grid-run----
  for (r in 1:360) {
    for (c in 1:720) {
      # initiate matrix for multiple regression
      g_data <- matrix(rep(0,210),nrow = 35,ncol = 6)
      g_data <- as.data.frame(g_data)
      names(g_data) <- c("gpp","CO2","tc","fAPAR","ppfd","sw_stress")
      for (i in 1:35) {
        g_data$gpp[i] <- gpp[[i]][r,c]
        g_data$CO2[i] <- CO2$V2[i]
        g_data$tc[i] <- tc[[i]][r,c]
        g_data$fAPAR[i] <- fAPAR[[i]][r,c]
        g_data$ppfd[i] <- ppfd[[i]][r,c]
        g_data$sw_stress[i] <- sw_stress[[i]][r,c]
      }
      
      if (anyNA(g_data)==F) {
        
        fit <- lm(gpp ~ CO2+tc+fAPAR+ppfd+sw_stress,data = g_data)
        
        # Full_fac----
        # full_fac[((r-1)*720+c),1] <- summary(fit)$coefficients[2,1]
        # full_fac[((r-1)*720+c),2] <- summary(fit)$coefficients[2,4]
        # 
        # full_fac[((r-1)*720+c),3] <- summary(fit)$coefficients[3,1]
        # full_fac[((r-1)*720+c),4] <- summary(fit)$coefficients[3,4]
        # 
        # full_fac[((r-1)*720+c),5] <- summary(fit)$coefficients[4,1]
        # full_fac[((r-1)*720+c),6] <- summary(fit)$coefficients[4,4]
        # 
        # full_fac[((r-1)*720+c),7] <- summary(fit)$coefficients[5,1]
        # full_fac[((r-1)*720+c),8] <- summary(fit)$coefficients[5,4]
        # 
        # if (nrow(summary(fit)$coefficients)==6) {
        #   full_fac[((r-1)*720+c),9] <- summary(fit)$coefficients[6,1]
        #   full_fac[((r-1)*720+c),10] <- summary(fit)$coefficients[6,4]
        # } else {
        #   full_fac[((r-1)*720+c),9] <- NA
        #   full_fac[((r-1)*720+c),10] <- NA
        # }
        # 
        # full_fac[((r-1)*720+c),11] <- summary(fit)$adj.r.squared
        
        # most important forcing----
        af <- anova(fit)
        afss <- af$"Sum Sq"
        af <- cbind(af,PctExp=afss/sum(afss)*100)
        
        if (row.names(af)[which(af$PctExp==max(af$PctExp))]=="Residuals"){
          result[r,c] <- which(fac==row.names(af)[which(af$PctExp==max(af$PctExp[1:(nrow(af)-1)]))])
          
        }else {
          result[r,c] <- which(fac==row.names(af)[which(af$PctExp==max(af$PctExp))])
        }
        
        a <- result[r,c]
        if (full_fac[((r-1)*720+c),(2*a-1)]<0) {
          result[r,c] <- result[r,c] * (-1)
        } 
        
        result[r,c] <- result[r,c] * 0.9
      
        } else {
        result[r,c] <- NA
      }
      
      # if (anyNA(g_data)==T) {
      #   result[r,c] <- NA
      # } else {
      #   # conduct multiple regression
      #   fit <- lm(gpp ~ CO2+tc+fAPAR+ppfd+sw_stress,data = g_data)
      #   af <- anova(fit)
      #    afss <- af$"Sum Sq"
      #    af <- cbind(af,PctExp=afss/sum(afss)*100)
      # 
      #    if (row.names(af)[which(af$PctExp==max(af$PctExp))]=="Residuals"){
      #      result[r,c] <- which(fac==row.names(af)[which(af$PctExp==max(af$PctExp[1:(nrow(af)-1)]))]) * 1.1
      #    }else {
      #      result[r,c] <- which(fac==row.names(af)[which(af$PctExp==max(af$PctExp))]) * 1.1
      #    }
        
        #  if (nrow(af)==6) {
        #   np_list[[names(fit[[1]])[2]]][r,c] <- fit$coefficients[[2]]
        #   np_list[[names(fit[[1]])[3]]][r,c] <- fit$coefficients[[3]]
        #   np_list[[names(fit[[1]])[4]]][r,c] <- fit$coefficients[[4]]
        #   np_list[[names(fit[[1]])[5]]][r,c] <- fit$coefficients[[5]]
        #   np_list[[names(fit[[1]])[6]]][r,c] <- fit$coefficients[[6]]
        #   np_list[["sw_p"]][r,c] <- summary(fit)$coefficients[6,4]
        # } else if (nrow(af)==5) {
        #   np_list[[names(fit[[1]])[2]]][r,c] <- fit$coefficients[[2]]
        #   np_list[[names(fit[[1]])[3]]][r,c] <- fit$coefficients[[3]]
        #   np_list[[names(fit[[1]])[4]]][r,c] <- fit$coefficients[[4]]
        #   np_list[[names(fit[[1]])[5]]][r,c] <- fit$coefficients[[5]]
        # }
        

      
    }
    print(paste(r,Sys.time(),sep = ","))
  }

  # write.csv(full_fac,"/Users/shirley/Desktop/multiple_regression.csv",row.names = F)
   write.table(result,
               "/Users/shirley/Desktop/Projects/20180620_global/Analysis/regrssion/multiple_regression/global_grid_modified_noresiduals_direction_2.1.csv",
               sep = ",",na="NaN",row.names = F,col.names = F)

    
  # np_CO2 <- np_list[["CO2"]]
  # np_tc <- np_list[["tc"]]
  # np_fAPAR <- np_list[["fAPAR"]]
  # np_ppfd <- np_list[["ppfd"]]
  # np_drought <- np_list[["sw_stress"]]
  # np_drought_p <- np_list[["sw_p"]]
  # 
  # write.table(np_drought_p,
   #           "/Users/shirley/Desktop/np_drought_p.csv",
    #          sep = ",",na="NaN",row.names = F,col.names = F)
  # 
 
  # np_tc[np_tc==0] <- NA 
  # write.table(np_tc,
  #             "/Users/shirley/Desktop/np_tc.csv",
  #             sep = ",",na="NaN",row.names = F,col.names = F)
  
  
  # zstates<-as.data.frame(scale(global))#scale()标准化
  # zfit<-lm(gpp~CO2+tc+fAPAR+ppfd+sw_stress,data = zstates)
  # coef(zfit)
  