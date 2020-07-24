run_Pmodel <- function(tc_mon,ppfd,VPD,Pa,CO2,fAPAR) {
  source("Constant_gpp.R")
  source("density_h2o.R")
  source("viscosity.R")
  
  gpp_result <- as.data.frame(matrix(0,nrow=nrow(g_data),ncol = 2))
  names(gpp_result) <- c("time","monthly_gpp")
  gpp_result$time <- g_data$Time
  
  sm_stress <- smf$re_f
  k_temp <- tc_mon+273.15; #convert from degree to kelvin
  
  # Calculate temperature pressure-dependent photorespiratory
  # compensation point, Gamma star
  ttg <- ((k_temp - t_25) * Ha)/(R * k_temp * t_25)
  Gamma_star <- gamma_25 * exp(ttg)
  
  # Calculate the temperature & pressure dependent Michaelis-Menten
  # coefficient, K (Pascals).
  tempFrac <- ((k_temp - t_25) * Ha_kc) / (R * k_temp * t_25)
  Kc <- kc25 * exp(tempFrac)
  
  tempFracK0 <- ((k_temp - t_25) * Ha_ko) / (R * k_temp * t_25)
  K0 <- ko25 * exp(tempFracK0)
  
  p0= kco * (1e-6) * Pa
  
  K= Kc * (1 + ( p0 / K0))
  
  # Calculate the substrate limitation term m
  # CO2_ppm data input
  ca<- CO2*1e-6*kPo;
  
  # Calculate the vapour pressure deficit D [Pa]
  vpd <- VPD * 100;
  if (any(vpd<0)) {
    vpd[vpd<0] <- 0
  }
  
  # Calculate the relative viscosity
  # calculate density and viscosity of water at given temperature and pressure
  # pw <- density_h2o(tc_mon,Pa)
  # mu <- viscosity_h2o(tc_mon,Pa)

  # pw <- density_h2o(25,kPo)
  # viscosity_25_kPo <- viscosity_h2o(25,kPo)
  viscosity_25_kPo <- 0.0008900093
  
  eta_star <- mu/viscosity_25_kPo;
  
  # Define the standardized cost ratio
  beta <- 240;
  
  # Calculate the substrate limitation term, m [dimensionless].
  m <-(ca-Gamma_star)/(ca+2*Gamma_star+3*Gamma_star*sqrt(1.6*eta_star*vpd*(beta*(K+Gamma_star))^-1));
  
  # Calculate GPP
  m_sqrt <- sqrt(1-((c_star/m)^(2/3)));
  
  LUE <- matrix(data=0, nrow = nrow(gpp_result), ncol = 1)
  for (l in 1:nrow(LUE)) {
    if (tc_mon[l]<0) {LUE[l]<-0}
    else {
      # Temperature dependent phi_0
      phi_psII <- 0.352 + 0.022 * tc_mon[l] - 3.4 * 10^(-4) * (tc_mon[l])^2
      C3_phi0 <- phi_psII / 8
      C3_phi0 <- C3_phi0 * c_molmass
      
      # C4_phi0 <- -0.008 + 0.00375 * tc - 0.58 * 10^(-4) * (tc)^2
      # C4_phi0 <- C4_phi0 * c_molmass
      # if (C4_phi0 <0) {C4_phi0 <- 0}
      
      LUE[l] <- C3_phi0 * m[l] * m_sqrt[l]
      # if there's mixed C3 and C4 plants, use following code
      #LUE <- C3_phi0*(1-c4_percent) * m * m_sqrt + C4_phi0*c4_percent * 1 * (sqrt(1-((c_star/1)^(2/3))))
    }
    
  }
  
  Iabs<-fAPAR*absG*ppfd;
  gpp_result$monthly_gpp <- LUE * Iabs * sm_stress
  
  return(gpp_result)
  
}