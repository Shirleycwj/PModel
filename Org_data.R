rm(list=ls())
setwd("/Users/shirley/Desktop/REALM/C4_2:3/Site_FLUXNET2015_20190427")

gpp_rawdata_path <- paste(getwd(),"/FLUXNET2015_rawdata/GPP",sep = "")
SPLASH_rawdata_path <- paste(getwd(),"/FLUXNET2015_rawdata/SPLASH",sep = "")

for (i in 1:21) {
  
  gpp_rawdata <- read.csv(paste(gpp_rawdata_path,dir(gpp_rawdata_path)[i],sep = "/"))
  SPLASH_rawdata <- read.csv(paste(SPLASH_rawdata_path,dir(SPLASH_rawdata_path)[i],sep = "/"))
  
  if (as.integer(substr(gpp_rawdata$TIMESTAMP[1],1,4))>2003) {
    gpp_start_line <- 1
    SPLASH_start_line <- 1
  } else {
    gpp_start_line <- which(gpp_rawdata$TIMESTAMP==200301)
    SPLASH_start_line <- which(SPLASH_rawdata$TIMESTAMP==20030101)
  }
  
  gpp_end_line <- which(gpp_rawdata$TIMESTAMP==201112)
  SPLASH_end_line <- which(SPLASH_rawdata$TIMESTAMP==20111231)
  
  gpp_cal_data <- as.data.frame(matrix(NA, nrow=(gpp_end_line-gpp_start_line+1),ncol=8))
  names(gpp_cal_data) <- c("Time",
                           "tc",  # air temperature
                           "SW_IN",  # solar radiation
                           "VPD",  # vapour pressure deficit
                           "Pa",  # pressure at given elevation
                           "Pn",  # precipitation
                           "CO2",  # CO2 concentration
                           "fAPAR"  # MERIS fAPAR 
  )
  
  SPLASH_caldata <- as.data.frame(matrix(NA, nrow=(SPLASH_end_line-SPLASH_start_line+1),ncol=4))
  names(SPLASH_caldata) <- c("Time",
                             "tc",  # air temperature
                             "SW_IN",  # solar radiation
                             "Pn"  # precipitation
  )
  
  gpp_cal_data$Time <- gpp_rawdata$TIMESTAMP[gpp_start_line:gpp_end_line]
  gpp_cal_data$tc <- gpp_rawdata$TA_F[gpp_start_line:which(gpp_rawdata$TIMESTAMP==201112)]
  gpp_cal_data$SW_IN <- gpp_rawdata$SW_IN_F[gpp_start_line:which(gpp_rawdata$TIMESTAMP==201112)]
  gpp_cal_data$VPD <- gpp_rawdata$VPD_F[gpp_start_line:which(gpp_rawdata$TIMESTAMP==201112)]
  gpp_cal_data$Pa <- gpp_rawdata$PA_F[gpp_start_line:which(gpp_rawdata$TIMESTAMP==201112)]
  gpp_cal_data$Pn <- gpp_rawdata$P_F[gpp_start_line:which(gpp_rawdata$TIMESTAMP==201112)]
  gpp_cal_data$CO2 <- gpp_rawdata$CO2_F_MDS[gpp_start_line:which(gpp_rawdata$TIMESTAMP==201112)]

  SPLASH_caldata$Time <- SPLASH_rawdata$TIMESTAMP[SPLASH_start_line:SPLASH_end_line]
  SPLASH_caldata$tc <- SPLASH_rawdata$TA_F[SPLASH_start_line:which(SPLASH_rawdata$TIMESTAMP==20111231)]
  SPLASH_caldata$SW_IN <- SPLASH_rawdata$SW_IN_F[SPLASH_start_line:which(SPLASH_rawdata$TIMESTAMP==20111231)]
  SPLASH_caldata$Pn <- SPLASH_rawdata$P_F[SPLASH_start_line:which(SPLASH_rawdata$TIMESTAMP==20111231)]
  
  gpp_fname <- paste("/Users/shirley/Desktop/REALM/C4_2:3/Site_FLUXNET2015_20190427/site_data_FLUXNET2015/GPP/",
                     substr(dir(gpp_rawdata_path)[i],5,10),".csv",sep = "")
  SPLASH_fname <- paste("/Users/shirley/Desktop/REALM/C4_2:3/Site_FLUXNET2015_20190427/site_data_FLUXNET2015/SPLASH/",
                        substr(dir(gpp_rawdata_path)[i],5,10),".csv",sep = "")
  write.csv(gpp_cal_data,gpp_fname,row.names = F)
  write.csv(SPLASH_caldata,SPLASH_fname,row.names = F)
}
