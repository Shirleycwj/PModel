rm(list = ls())
setwd("/Users/shirley/Desktop/REALM/C4_2:3/Site_FLUXNET2015_20190427/code_20190503")
source("soil_moisture_function.R")
source("Constant_gpp.R")
source("P_model.R")
source("density_h2o.R")
source("viscosity.R")

site_info <- read.csv("/Users/shirley/Desktop/REALM/C4_2:3/Site_FLUXNET2015_20190427/site_data_FLUXNET2015/site_info_20190501.csv")
gpp_pathway <- "/Users/shirley/Desktop/REALM/C4_2:3/Site_FLUXNET2015_20190427/site_data_FLUXNET2015/GPP/"
SPLASH_pathway <- "/Users/shirley/Desktop/REALM/C4_2:3/Site_FLUXNET2015_20190427/site_data_FLUXNET2015/SPLASH/"

for (s in site_info$Site) {


   g_data <- read.csv(paste(gpp_pathway,s,".csv",sep = ""))
   SP_data <- read.csv(paste(SPLASH_pathway,s,".csv",sep = ""))
   
   start_year <- as.double(substr(SP_data$Time[1],1,4)) 
   end_year <- as.double(substr(SP_data$Time[length(SP_data$Time)],1,4))
   
   sw_in <- SP_data$SW_IN  # incoming solar radiation (W/m2)
   tc <- SP_data$tc  # daily temperature (degree celcius)
   pn <- SP_data$Pn  # daily precipitation (mm)
   lat <- site_info$Lat[which(site_info$Site==s)] # latitude
   elev <- site_info$elev[which(site_info$Site==s)]  # elevation of site
   
   tc_mon <- g_data$tc  # monthly temperature (mm)
   sw_in_mon <- g_data$SW_IN  # monthly incoming solar radiation (W/m2)
   ppfd <- sw_in_mon * 86.4 * kfFEC * (1.0e-3)  # converted to ppfd (µmol/m2 day)
   VPD <- g_data$VPD  # vapour pressure deficit (hPa)
   Pa <- g_data$Pa * (1.0e3)  # atmosperic pressure at site altitude (kPa) then coverted to Pa
   CO2 <- g_data$CO2  # site atmosperic carbon dioxide concentration (missing data were replaced by CO2 data from Moana Loa Labotory)
   fAPAR <- g_data$fAPAR  # only non-site-specific data from MERIS resolution 1km
   
   smf <- drought(sw_in, tc, pn, lat,elev)
   
   # calculate density and viscosity of water at given temperature and pressure
   pw <- density_h2o(tc_mon,Pa)
   #-------calculate relative viscosity
   tk_ast = 647.096      # Kelvin
   rho_ast = 322.0       # kg/m^3
   mu_ast = (1e-6)       # Pa s
   
   # Get the density of water, kg/m^3
   rho = pw;
   
   # Calculate dimensionless parameters:
   tbar = (tc_mon + 273.15)/tk_ast
   tbarx = tbar^(0.5)
   tbar2 = tbar^2
   tbar3 = tbar^3
   rbar = rho/rho_ast
   
   # Calculate mu0 (Eq. 11 & Table 2, Huber et al., 2009):
   mu0 = 1.67752
   mu0 = mu0+ 2.20462/tbar
   mu0= mu0+0.6366564/tbar2
   mu0= mu0-0.241605/tbar3
   mu0 = 1e2*tbarx/mu0
   
   # Create Table 3, Huber et al. (2009):
   hj0 = c(0.520094,0.0850895, -1.08374, -0.289555, 0., 0.)
   hj1 = c(0.222531, 0.999115, 1.88797, 1.26613, 0., 0.120573)
   hj2 = c(-0.281378, -0.906851, -0.772479, -0.489837, -0.257040, 0.)
   hj3 = c(0.161913,  0.257399, 0., 0., 0., 0.)
   hj4 = c(-0.0325372, 0., 0., 0.0698452, 0., 0.)
   hj5 = c(0., 0., 0., 0., 0.00872102, 0.)
   hj6 = c(0., 0., 0., -0.00435673, 0., -0.000593264)
   
   h_array = t(cbind(hj0 , hj1 , hj2 ,hj3 ,hj4 , hj5 , hj6))
   
   # Calculate mu1 (Eq. 12 & Table 3, Huber et al., 2009):
   mu1 = matrix(0,nrow=length(pw),ncol=1)
   ctbar = (1./tbar) - 1.
   
   for (k in 1:length(tc_mon)) {
     for (i in 1:6){
       coef1 = sum(ctbar[k]^(i-1))
       coef2=0
       for (j in 1:7){
         
         coef2 = coef2+(h_array[j,i]*(rbar[k] - 1)^(j-1))
       }
       mu1[k] = mu1[k]+coef1*coef2
       
     }
     
     mu1[k] = exp(rbar[k]*mu1[k])
   }  
   # Calculate mu_bar (Eq. 2, Huber et al., 2009)
   #   assumes mu2 = 1
   mu_bar = mu0*mu1
   
   # Calculate mu (Eq. 1, Huber et al., 2009)
   mu = mu_bar*mu_ast    # Pa s
   mu<-as.numeric(mu)
   #-------
   #mu <- viscosity_h2o(tc_mon,Pa)
   
   # calculate monthly GPP
   monthly_gpp <- run_Pmodel(tc_mon,ppfd,VPD,Pa,CO2,fAPAR)
   
   # calculate annual GPP
   gpp_annual <- as.data.frame(matrix(0,nrow=nrow(g_data)/12,ncol = 2))
   names(gpp_annual) <- c("Year","annual_gpp")
   gpp_annual$Year <- seq(from=substr(g_data$Time[1],1,4),to=substr(g_data$Time[length(g_data$Time)],1,4))
   
   nm <- matrix()
   for (y in start_year:end_year) {
     j=y-start_year+1
     for (m in 1:12) {
       nm[12*j-12+m] <- julian_day(y,m+1,1)-julian_day(y,m,1)
     }
   }
   monthly_total <- monthly_gpp$monthly_gpp * nm
   for (ny in 1:length(gpp_annual$Year)) {
     gpp_annual$annual_gpp[ny] <- sum(monthly_total[((ny*12)-11):(ny*12)])
   }
   monthly_path <- paste("/Users/shirley/Desktop/REALM/C4_2:3/Site_FLUXNET2015_20190427/result/monthly/",
                        s,".csv",sep = "")
   annual_path <- paste("/Users/shirley/Desktop/REALM/C4_2:3/Site_FLUXNET2015_20190427/result/annual/",
                        s,".csv",sep = "")
   
   write.csv(monthly_gpp,monthly_path,row.names = F)
   write.csv(gpp_annual,annual_path,row.names = F)
}







