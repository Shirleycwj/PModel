rm(list=ls())
setwd("/Users/wenjia/Desktop/PhD/20190415_C4/Global/code_20190703")
# source("splash_Main_point.R")
source("soil_moisture_function.R")
library(data.table)

ele <- read.csv("/Users/wenjia/Desktop/PhD/20190415_C4/Global/SPLASH_data/ele.csv")
grass_cover <- read.csv("/Users/wenjia/Desktop/PhD/20190415_C4/Global/SPLASH_data/grassland_cover.csv",header = F)

fn <- dir("/Users/wenjia/Desktop/PhD/20190415_C4/Global/SPLASH_data/SW_IN")

for (i in fn[20:35]) {
  # Initiate source SPLASH data
  SW_IN_pathway <- paste("/Users/wenjia/Desktop/PhD/20190415_C4/Global/SPLASH_data","SW_IN",i,sep = "/")
  Tair_pathway <- paste("/Users/wenjia/Desktop/PhD/20190415_C4/Global/SPLASH_data","Tair",i,sep = "/")
  Pre_pathway <- paste("/Users/wenjia/Desktop/PhD/20190415_C4/Global/SPLASH_data","Pre",i,sep = "/")
  
  my_SW_IN <- fread(SW_IN_pathway)
  my_Tair <- fread(Tair_pathway)
  my_Pre <- fread(Pre_pathway)
  
  start_year <- as.double(substr(i,1,4)) 
  end_year <- start_year
  
  # Initiate result table
  SP_result_monthly <- matrix(NA,nrow = 259200,ncol = 12)
 
  Sys.time()
  for (j in 1:nrow(my_SW_IN)) {
    # read and run SPLASH grid by grid
    # SP_data <- data.frame(SW_IN=t(my_SW_IN[j,]),
    #                     Tair=t(my_Tair[j,]),
    #                     Pre=t(my_Pre[j,]),
    #                     row.names = seq(1:length(my_SW_IN)))
    # 
    # sw_in <- SP_data$SW_IN
    # tc <- SP_data$Tair-273.15
    # pn <- SP_data$Pre
    sw_in <- t(my_SW_IN[j,])
    tc <- (t(my_Tair[j,]))-273.15
    pn <- t(my_Pre[j,])
    lat <- ele$Var2[j]
    elev <- ele$pre.mat[j]
    g_cover <- grass_cover[(361-ceiling(j/720)),((j-1)%%720+1)]
    
    condition <- anyNA(sw_in || tc || pn)==T
    
    if (condition) {
      SP_result_monthly[j,] <- NA
    } else {
      SP_result_mon <- drought(sw_in,tc,pn,lat,elev,g_cover)
      SP_result_monthly[j,] <- t(SP_result_mon)
    }
  }
  Sys.time()
    
  my_SP_result <- list()
  for (num in 1:12) {
    # my_SP_result[[num]] <- matrix(SP_result_monthly[,num],nrow=360,byrow=T)
    
    mon <- SP_result_monthly[, num]
    my_SP_result[[num]] <- matrix(NA,nrow = 360,ncol = 720)
    for (r in 1:360) {
      my_SP_result[[num]][(361-r),1:720] <- mon[((r*720)-719):(r*720)]
    }
    SP_filename <- paste("/Users/wenjia/Desktop/PhD/20190415_C4/Global/SPLASH_result/",
                         start_year,sprintf("%02d",num),".csv",sep = "")
    write.table(my_SP_result[[num]],SP_filename,sep = ",",row.names = F,col.names = F)

  }
  
}




# for (i in site_info$Site) {
#   SP_fname <- paste("/Users/shirley/Desktop/REALM/C4_2:3/Site_FLUXNET2015_20190427/site_data_FLUXNET2015/SPLASH/",
#                       i,".csv",sep = "")
#   SP_data <- read.csv(SP_fname)
#   start_year <- as.double(substr(SP_data$Time[1],1,4)) 
#   end_year <- as.double(substr(SP_data$Time[length(SP_data$Time)],1,4))
#   
#   sw_in <- SP_data$SW_IN
#   tc <- SP_data$tc
#   pn <- SP_data$Pn
#   lat <- site_info$Lat[which(site_info$Site==i)]
#   elev <- site_info$elev[which(site_info$Site==i)]
#   
#   SP_result_daily <- splash(sw_in,tc,pn, lat, elev)
#   for (r in 1:nrow(SP_result_daily)) {
#     if (is.na(SP_result_daily$pet[r])==T) {
#       SP_result_daily$pet[r] <- 0
#     }
#     if (is.na(SP_result_daily$aet[r])==T) {
#       SP_result_daily$aet[r] <- 0
#     }
#   }
#   
#   # process one year's result into monthly result
#   nm <- matrix()
#   for (y in start_year:end_year) {
#     j=y-start_year+1
#     for (m in 1:12) {
#       nm[12*j-12+m] <- julian_day(y,m+1,1)-julian_day(y,m,1)
#     }
#   }
#   nm_peryear <- cumsum(nm)
#   
#   pd <- seq(from=start_year,to=end_year)
#   ny <- julian_day(pd + 1, 1, 1) - julian_day(pd, 1, 1)
#   ny_peryear <- cumsum(ny)
#   
#   SP_result_mon <- as.data.frame(matrix(NA,nrow=length(pd)*12,ncol=3))
#   names(SP_result_mon) <- c("wn","alpha","re_f")
#   for (mon in 1:length(nm_peryear)) {
#     SP_result_mon$wn[mon] <-mean(SP_result_daily$wn[(nm_peryear[mon]-nm[mon]+1):nm_peryear[mon]])/150
#   }
#   for (yr in 1:length(pd)) {
#     SP_result_mon$alpha[(yr*12-11):(yr*12)] <- mean(SP_result_daily$aet[(ny_peryear[yr]-ny[yr]+1):ny_peryear[yr]])/mean(SP_result_daily$pet[(ny_peryear[yr]-ny[yr]+1):ny_peryear[yr]])
#   }
#   
#   for (k in 1:nrow(SP_result_mon)) {
#     if (SP_result_mon$wn[k] > 0.9) {
#       SP_result_mon$re_f[k] <- 1
#     } else if (SP_result_mon$wn[k] <= 0.9) {
#       phi_0 <- 0.179 + 0.45*SP_result_mon$alpha[k]
#       beta <- (phi_0 - 1) / 0.81
#       SP_result_mon$re_f[k] <- beta * (SP_result_mon$wn[k]-0.9)^2 +1
#     }
#   }
#   outfile <- paste("/Users/shirley/Desktop/REALM/C4_2:3/Site_FLUXNET2015_20190427/site_data_FLUXNET2015/sm_function/",
#                    i,".csv",sep = "")
#   write.csv(SP_result_mon,outfile,row.names = F)
# }


