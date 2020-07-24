rm(list = ls());
setwd("/Users/wenjia/Desktop/PhD/Research/20190415_C4/Global/code_20190703")
source("Constant_global_20190905.R");
source("viscosity_190905.R")
source("density_h2o_190905.R")
library(rpmodel)

fn <- dir("/Users/wenjia/Desktop/PhD/Research/20190415_C4/Global/SPLASH_result")

# read data csv file
my_co2ppm <- read.csv("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/co2_monthly_82_16.csv",header = F)
my_elv <- read.csv("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/elevation/ele_map.csv",header = F)
my_c4_percent <- read.csv("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/c4_percent_map.csv",header = F)

for (f in 1:420) {
  
  my_Rad <- read.csv(paste("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/ppfd/map_convert_unit/",fn[f],sep = ""),header = F)
  my_ppfd <- my_Rad * kfFEC * (1.0e-3);
  
  my_tc <- read.csv(paste("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/Tmp/convert_map/",fn[f],sep = ""),header = F)
  my_Tmax <- read.csv(paste("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/Tmx/convert_map/",fn[f],sep = ""),header = F)
  my_Tmin <- read.csv(paste("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/Tmn/convert_map/",fn[f],sep = ""),header = F)
  my_vap <- read.csv(paste("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/Vap/convert_map/",fn[f],sep = ""),header = F)
  my_fAPAR <- read.csv(paste("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/fAPAR/after_merging/",fn[f],sep = ""),header = F)
  my_soil_stress <- read.csv(paste("/Users/wenjia/Desktop/PhD/Research/20190415_C4/Global/SPLASH_result/",fn[f],sep = ""),header = F)
 
  co2ppm <- my_co2ppm$V2[f]

  my_temp <- my_tc # in degree
  
  # Initiate results
  gpp <- matrix(rep(0,259200),nrow = 360,ncol = 720)
  
  for (d_row in 1:nrow(gpp)) {
    
    for (d_col in 1:ncol(gpp)) {
      # Initiate data to calculate
      elv <- my_elv[d_row,d_col]
      ppfd <- my_ppfd[d_row,d_col]
      tc <- my_tc[d_row,d_col]
      Tmax <- my_Tmax[d_row,d_col]
      Tmin <- my_Tmin[d_row,d_col]
      vap <- my_vap[d_row,d_col]
      fAPAR <- my_fAPAR[d_row,d_col]
      c4_percent <- my_c4_percent[d_row,d_col]
      soil_stress <- my_soil_stress[d_row,d_col]
     
      
      if (anyNA(c(elv,ppfd,tc,Tmax,Tmin,vap,fAPAR,c4_percent))==T) {
        gpp[d_row,d_col] <- NA
        
      } else {
        
        # Begin calculation
        # Calculate the pressure of given elevation
        p <- kPo*(1 - kL*elv/kTo)^(kG*kMa/(kR*kL));
        pw <- density_h2o(tc,p)
        mu <- viscosity_h2o(tc,p)
        
        # ************************************************************************
        # Calculate vpd under given temperature and vapour pressure
        # Tmax_min <- (8.635 * (Tmax + Tmin))/(0.5 * (Tmax + Tmin) + 237.3)
        # vpd_hpa <- (es0 * exp(Tmax_min) - (0.10 * vap)) * 10. # kPa --> hPa
        vap_elev <- vap * (p/kPo)
        es1 <- es0 * exp((17.27 * Tmax)/(Tmax+273.3))
        vpd1_hpa <- (es1 - 0.1 * vap_elev) * 10 # kPa --> hPa
        es2 <- es0 * exp((17.27 * Tmin)/(Tmin+273.3))
        vpd2_hpa <- (es2 - 0.1 * vap_elev) * 10 # kPa --> hPa
        vpd_hpa <- (vpd1_hpa + vpd2_hpa)/2 #calculating vpd by average the two
        vpd <- vpd_hpa * 100 # convert to Pa
        
        if (c4_percent>=0.5) {
          c4_stat <- TRUE
        } else {
          c4_stat <- FALSE
        }
        
        output <- rpmodel(
          tc = tc,
          vpd = vpd,
          co2 = co2ppm,
          fapar = fAPAR,
          ppfd = ppfd,
          elv = elv,
          method_optci   = "prentice14",
          method_jmaxlim = "wang17", 
          do_ftemp_kphio = TRUE, 
          do_soilmstress = FALSE,
          c4 = c4_stat
        )
        
        gpp[d_row,d_col] <- output$gpp * soil_stress
        
      }
    }
    
  }
  report <- paste(fn[f],"is finished", Sys.time())
  print(report)
  
  out_file <- paste("/Users/wenjia/Desktop/Beni_gpp/mon_gpp/",fn[f],sep = "")
  write.table(gpp,out_file,sep = ",",na="NaN",row.names = F,col.names = F)
  
}







