density_h2o <- function(t,p) {
  # Calculate density of water at 1 atm, g/cm^3
  po <- 0.99983952 +
    (6.788260e-5)*t +
    -(9.08659e-6)*t*t +
    (1.022130e-7)*t*t*t +
    -(1.35439e-9)*t*t*t*t +
    (1.471150e-11)*t*t*t*t*t +
    -(1.11663e-13)*t*t*t*t*t*t +
    (5.044070e-16)*t*t*t*t*t*t*t +
    -(1.00659e-18)*t*t*t*t*t*t*t*t
  
  # Calculate the bulk modulus of water at 1 atm, atm
  ko <- 19652.17 +
    148.1830*t +
    -2.29995*t*t +
    0.01281*t*t*t +
    -(4.91564e-5)*t*t*t*t +
    (1.035530e-7)*t*t*t*t*t
  
  # Calculate temperature-dependend coefficients
  ca <- 3.26138 +
    (5.223e-4)*t +
    (1.324e-4)*t*t +
    -(7.655e-7)*t*t*t +
    (8.584e-10)*t*t*t*t
  
  cb <- (7.2061e-5) +
    -(5.8948e-6)*t +
    (8.69900e-8)*t*t +
    -(1.0100e-9)*t*t*t +
    (4.3220e-12)*t*t*t*t
  
  # Convert pressure to bar (1 bar = 100000 Pa)
  pbar <- (1e-5)*p;
  pw <- (1e3)*po*(ko + ca*pbar + cb*pbar^2)/(ko + ca*pbar + cb*pbar^2 - pbar);
  
  return(pw)
}
