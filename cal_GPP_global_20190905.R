rm(list = ls());
setwd("/Users/wenjia/Desktop/PhD/20190415_C4/Global/code_20190703")
source("Constant_global_20190905.R");
source("viscosity_190905.R")
source("density_h2o_190905.R")

fn <- dir("/Users/wenjia/Desktop/PhD/20190415_C4/Global/SPLASH_result")

# read data csv file
my_co2ppm <- read.csv("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/co2_monthly_82_16.csv",header = F)
my_elv <- read.csv("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/elevation/ele_map.csv",header = F)
my_c4_percent <- read.csv("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/c4_percent_map.csv",header = F)

for (f in 1:420) {
  # r <- f%%12
  # if (r==0) {
  #   r <- 12
  # }
  
  my_Rad <- read.csv(paste("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/ppfd/map_convert_unit/",fn[f],sep = ""),header = F)
  my_ppfd <- my_Rad * kfFEC * (1.0e-3);
  
  my_tc <- read.csv(paste("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/Tmp/convert_map/",fn[f],sep = ""),header = F)
  my_Tmax <- read.csv(paste("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/Tmx/convert_map/",fn[f],sep = ""),header = F)
  my_Tmin <- read.csv(paste("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/Tmn/convert_map/",fn[f],sep = ""),header = F)
  my_vap <- read.csv(paste("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/Vap/convert_map/",fn[f],sep = ""),header = F)
  my_fAPAR <- read.csv(paste("/Volumes/兔子/研究生学习/Projects/20180620_global/Data/fAPAR/after_merging/",fn[f],sep = ""),header = F)
  my_soil_stress <- read.csv(paste("/Users/wenjia/Desktop/PhD/20190415_C4/Global/SPLASH_result/",fn[f],sep = ""),header = F)
 
  co2ppm <- my_co2ppm$V2[f]

  my_k_temp <- my_tc+273.15; #convert from degree to kelvin
  
  # Initiate results
  gpp <- matrix(rep(0,259200),nrow = 360,ncol = 720)
  
  # Initiate data to calculate
  # co2ppm <- my_co2ppm$V2[my_co2ppm$V1==substr(fname,1,6)]
  
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
      k_temp <- my_k_temp[d_row,d_col]
      
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
        
        # ************************************************************************
        # Calculate temperature pressure-dependent photorespiratory
        # compensation point, Gamma star
        ttg <- ((k_temp - t_25) * Ha)/(R * k_temp * t_25)
        Gamma_star <- gamma_25 * exp(ttg)
        
        # ************************************************************************
        # Calculate the temperature & pressure dependent Michaelis-Menten
        # coefficient, K (Pascals).
        tempFrac <- ((k_temp - t_25) * Ha_kc) / (R * k_temp * t_25)
        Kc <- kc25 * exp(tempFrac)
        
        tempFracK0 <- ((k_temp - t_25) * Ha_ko) / (R * k_temp * t_25)
        K0 <- ko25 * exp(tempFracK0)
        
        p0= kco * (1e-6) * p
        
        K= Kc * (1 + ( p0 / K0))
        
        # ************************************************************************
        # Calculate the substrate limitation term m
        
        # CO2_ppm data input
        ca<- co2ppm*1e-6*kPo;
        
        # Calculate the vapour pressure deficit D [Pa]
        vpd <- vpd_hpa * 100;
        if (vpd < 0) {
          vpd <- 0
        }
        
        # Calculate the relative viscosity
        eta_star <- viscosity_h2o(tc,p)/viscosity_h2o(25,kPo);
        
        # Define the standardized cost ratio
        beta <- 240;
        
        # Calculate the substrate limitation term, m [dimensionless].
        m <-(ca-Gamma_star)/(ca+2*Gamma_star+3*Gamma_star*sqrt(1.6*eta_star*vpd*(beta*(K+Gamma_star))^-1));
        
        # ************************************************************************
        # Calculate GPP
        
        m_sqrt <- sqrt(1-((c_star/m)^(2/3)));
        
        if (tc<0) {LUE<-0}
        else {
          phi_psII <- 0.352 + 0.022 * tc - 3.4 * 10^(-4) * (tc)^2
          C3_phi0 <- phi_psII / 8
          C3_phi0 <- C3_phi0 * c_molmass
          
          C4_phi0 <- -0.008 + 0.00375 * tc - 0.58 * 10^(-4) * (tc)^2
          C4_phi0 <- C4_phi0 * c_molmass
          if (C4_phi0 <0) {C4_phi0 <- 0}
          
          LUE <- C3_phi0*(1-c4_percent) * m * m_sqrt + C4_phi0*c4_percent * 1 * (sqrt(1-((c_star/1)^(2/3))))
          }
        
        
        Iabs<-fAPAR*0.8*absG*ppfd;  
        
        gpp[d_row,d_col] <- LUE * Iabs * soil_stress
        
      }
    }
    
  }
  report <- paste(fn[f],"is finished", Sys.time())
  print(report)
  
  out_file <- paste("/Users/wenjia/Desktop/PhD/20190415_C4/Global/mon_gpp/",fn[f],sep = "")
  write.table(gpp,out_file,sep = ",",na="NaN",row.names = F,col.names = F)
  
}







