viscosity_h2o <- function(tc_mon, Pa) {
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
  return(mu)
}
