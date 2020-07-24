density_h2o <- function(tc,p) {
  # Calculate density of water at 1 atm, g/cm^3
  po <- 0.99983952 +
    (6.788260e-5)*tc +
    -(9.08659e-6)*tc*tc +
    (1.022130e-7)*tc*tc*tc +
    -(1.35439e-9)*tc*tc*tc*tc +
    (1.471150e-11)*tc*tc*tc*tc*tc +
    -(1.11663e-13)*tc*tc*tc*tc*tc*tc +
    (5.044070e-16)*tc*tc*tc*tc*tc*tc*tc +
    -(1.00659e-18)*tc*tc*tc*tc*tc*tc*tc*tc
  
  # Calculate the bulk modulus of water at 1 atm, atm
  ko <- 19652.17 +
    148.1830*tc +
    -2.29995*tc*tc +
    0.01281*tc*tc*tc +
    -(4.91564e-5)*tc*tc*tc*tc +
    (1.035530e-7)*tc*tc*tc*tc*tc
  
  # Calculate temperature-dependend coefficients
  ca <- 3.26138 +
    (5.223e-4)*tc +
    (1.324e-4)*tc*tc +
    -(7.655e-7)*tc*tc*tc +
    (8.584e-10)*tc*tc*tc*tc
  
  cb <- (7.2061e-5) +
    -(5.8948e-6)*tc +
    (8.69900e-8)*tc*tc +
    -(1.0100e-9)*tc*tc*tc +
    (4.3220e-12)*tc*tc*tc*tc
  
  # Convert pressure to bar (1 bar = 100000 Pa)
  pbar <- (1e-5)*p;
  pw <- (1e3)*po*(ko + ca*pbar + cb*pbar^2)/(ko + ca*pbar + cb*pbar^2 - pbar);
  
  return(pw)
}