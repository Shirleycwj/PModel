# evap.R
#
# VERSION: 1.0-r2
# LAST UPDATED: 2016-09-11
#
# ~~~~~~~~
# license:
# ~~~~~~~~
# Copyright (C) 2016 Prentice Lab
#
# This file is part of the SPLASH model.
#
# SPLASH is free software: you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 2.1 of the License, or
# (at your option) any later version.
#
# SPLASH is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with SPLASH.  If not, see <http://www.gnu.org/licenses/>.
#
# ~~~~~~~~~
# citation:
# ~~~~~~~~~
# T. W. Davis, I. C. Prentice, B. D. Stocker, R. J. Whitley, H. Wang, B. J.
# Evans, A. V. Gallego-Sala, M. T. Sykes, and W. Cramer, Simple process-led
# algorithms for simulating habitats (SPLASH): Robust indices of radiation
# evapo-transpiration and plant-available moisture, Geoscientific Model
# Development, 2016 (in progress)
#
# ~~~~~~~~~~~~
# description:
# ~~~~~~~~~~~~
# This script contains functions to calculate daily radiation, condensation,
# and evapotranspiration, i.e.:
#   berger_tls(double n, double N)
#   density_h2o(double tc, double pa)
#   dcos(double d)
#   dsin(double d)
#   elev2pres(double z)
#   enthalpy_vap(double tc)
#   evap(double lat, double n, double elev=0, double y=0, double sw_in=1,
#        double tc=23.0, double sw=1.0)
#   julian_day(double y, double m, double i)
#   psychro(double tc, double pa)
#   sat_slope(double tc)
#   specific_heat(double tc)
#
# ~~~~~~~~~~
# changelog:
# ~~~~~~~~~~
# - fixed Cooper's and Spencer's declination angle equations [14.11.25]
# - replaced simplified_kepler with full_kepler method [14.11.25]
# - added berger_tls function [15.01.13]
# - updated evap function (similar to stash.py EVAP class) [15.01.13]
# - added missing variables to evap list [16.02.17]
# - updated documentation [16.05.27]
# - addressed specific heat limitation [16.09.11]
#
#### IMPORT SOURCES ##########################################################
# source("const.R")
# source("solar.R")


#### DEFINE FUNCTIONS ########################################################

# ************************************************************************
# Name:     density_h2o
# Inputs:   - double (tc), air temperature, degrees C
#           - double (pa), atm pressure, Pa
# Returns:  double, kg/m^3
# Features: This function calculates the temperature and pressure
#           dependent density of pure water
# * Ref:    Chen, C.T., R.A. Fine, and F.J. Millero (1977), The equation
#             of state of pure water determined from sound speeds, The
#             Journal of Chemical Physics 66, 2142;
#             doi:10.1063/1.434179
# ************************************************************************
density_h2o <- function(tc, pa) {
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
	pbar <- (1e-5)*pa
	
	pw <- (1e3)*po*(ko + ca*pbar + cb*pbar^2)/(ko + ca*pbar + cb*pbar^2 - pbar)
	return(pw)
}


# ************************************************************************
# Name:     elev2pres
# Inputs:   double (z), meters
# Returns:  double, Pa
# Features: Calculates atmospheric pressure for a given elevation
# Depends:  - kPo ............ base pressure, Pa
#           - kTo ............ base temperature, C
#           - kL ............. temperature lapse rate, K/m
#           - kR ............. universal gas constant, J/mol/K
#           - kMa ............ molecular weight of dry air, kg/mol
#           - kG ............. gravity, m/s^2
# Ref:      Allen et al. (1998)
# ************************************************************************
elev2pres <- function(z) {
	kPo*(1 - kL*z/kTo)^(kG*kMa/(kR*kL))
}


# ************************************************************************
# Name:     viscosity_h2o
# Input:    - float, ambient temperature (tc), degrees C
#           - float, ambient pressure (p), Pa
# Return:   float, viscosity of water (mu), Pa s
# Features: Calculates viscosity of water at a given temperature and
# pressure.
# Depends:  density_h2o
# Ref:      Huber, M. L., R. A. Perkins, A. Laesecke, D. G. Friend, J. V.
# Sengers, M. J. Assael, ..., K. Miyagawa (2009) New
# international formulation for the viscosity of H2O, J. Phys.
# Chem. Ref. Data, Vol. 38(2), pp. 101-125.
# Define reference temperature, density, and pressure values:
# ************************************************************************
viscosity_h2o <- function(tc, p) {
	tk_ast = 647.096      # Kelevin
	rho_ast = 322.0       # kg/m^3
	mu_ast = (1e-6)       # Pa s
	
	# Get the density of water, kg/m^3
	rho = density_h2o(tc, p)
	
	# Calculate dimensionless parameters:
	tbar = (tc + 273.15)/tk_ast
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
	mu1 = 0
	ctbar = (1./tbar) - 1.
	
	for (i in 1:6){
		coef1 = sum(ctbar^(i-1))
		coef2=0
		for (j in 1:7){
			
			coef2 = coef2+(h_array[j,i]*(rbar - 1)^(j-1))
		}
		mu1 = mu1+coef1*coef2
		
	}
	
	mu1 = exp(rbar*mu1)
	
	
	# Calculate mu_bar (Eq. 2, Huber et al., 2009)
	#   assumes mu2 = 1
	mu_bar = mu0*mu1
	
	# Calculate mu (Eq. 1, Huber et al., 2009)
	mu = mu_bar*mu_ast    # Pa s
	mu<-as.numeric(mu)
	return(mu)
}

# ************************************************************************
# Name:     enthalpy_vap
# Inputs:   double (tc), air temperature, degrees C
# Returns:  double, J/kg
# Features: This function calculates the temperature-dependent enthalpy
#           of vaporization (latent heat of vaporization)
# Ref:      Eq. 8, Henderson-Sellers (1984), A new formula for latent heat
#             of vaporization of water as a function of temperature, Quarterly
#             Journal of the Royal Meteorological Society, vol. 110, pp. 1186--
#             1190.
# ************************************************************************
enthalpy_vap <- function(tc) {
	1.91846e6*((tc + 273.15)/(tc + 273.15 - 33.91))^2
}


# ************************************************************************
# Name:     psychro
# Inputs:   - double (tc), air temperature, degrees C
#           - double (pa), atm pressure, Pa
# Returns:  double, Pa/K
# Features: This function calculates the temperature and pressure
#           dependent psychrometric constant
# Depends:  - enthalpy_vap
#           - specific_heat
# Ref:      Allen, R.G., L.S. Pereira, D. Raes, M. Smith (1998),
#           'Meteorological data,' Crop evapotranspiration - Guidelines
#           for computing crop water requirements - FAO Irrigation and
#           drainage paper 56, Food and Agriculture Organization of the
#           United Nations, Available:
#           http://www.fao.org/docrep/x0490e/x0490e07.htm
# ************************************************************************
psychro <- function(tc, pa) {
	# Calculate the specific heat capacity of water, J/kg/K
	cp <- specific_heat(tc)
	
	# Calculate latent heat of vaporization, J/kg
	lv <- enthalpy_vap(tc)
	
	# Calculate psychrometric constant, Pa/K
	return(cp*kMa*pa/(kMv*lv))
}


# ************************************************************************
# Name:     specific_heat
# Inputs:   double (tc), air temperature, degrees C
# Returns:  double, specific heat of moist air, J/kg/K
# Features: This function calculates the spefic heat of moist air
# Ref:      Tsilingris (2008), Thermophysical and transport properties of
#           humid air at temperature range between 0 and 100 °C, Energy
#           Conversion and Management, vol. 49, pp. 1098--1110.
# ************************************************************************
specific_heat <- function(tc) {
	if (tc < 0) {
		tc <- 0
	} else if (tc > 100) {
		tc <- 100
	}
	cp <- 1.0045714270 +
	(2.050632750e-3)*tc -
	(1.631537093e-4)*tc*tc +
	(6.212300300e-6)*tc*tc*tc -
	(8.830478888e-8)*tc*tc*tc*tc +
	(5.071307038e-10)*tc*tc*tc*tc*tc
	cp <- (1e3)*cp
	
	return(cp)
}


# ************************************************************************
# Name:     sat_slope
# Inputs:   double (tc), degrees C
# Returns:  double, Pa/K
# Features: This function calculates the temperature-dependent slope of
#           the saturation pressure temperature curve using the
#           methodology presented in the eMast energy.cpp script
# Ref:      - Eq. 6, Prentice et al. (1993);
#           - Eq. 13, Allen et al. (1998)
# ************************************************************************
sat_slope <- function(tc) {
	(17.269)*(237.3)*(610.78)*exp(17.269*tc/(237.3 + tc))/(237.3 + tc)^2
}


# ************************************************************************
# Name:     calc_daily_evap
# Inputs:   - double, latitude, degrees (lat)
#           - double, day of year (n)
#           - double, elevation (elev)  *optional
#           - double, year (y)         *optional
#           - double, fraction of sunshine hours (sw_in)        *optional
#           - double, mean daily air temperature, deg C (tc) *optional
#           - double, evaporative supply rate, mm/hr (sw)    *optional
# Returns:  list object (evap)
#             $nu_deg ............ true anomaly, degrees
#             $lambda_deg ........ true longitude, degrees
#             $dr ................ distance factor, unitless
#             $delta_deg ......... declination angle, degrees
#             $hs_deg ............ sunset angle, degrees
#             $ra_j.m2 ........... daily extraterrestrial radiation, J/m^2
#             $tau ............... atmospheric transmittivity, unitless
#             $ppfd_mol.m2 ....... daily photosyn photon flux density, mol/m^2
#             $hn_deg ............ net radiation hour angle, degrees
#             $rn_j.m2 ........... daily net radiation, J/m^2
#             $rnn_j.m2 .......... daily nighttime net radiation, J/m^2
#             $econ_m3.j ......... water to energy conversion, m^3/J
#             $cond_mm ........... daily condensation, mm
#             $eet_mm ............ daily equilibrium evapotranspiration, mm
#             $pet_mm ............ daily potential evapotranspiration, mm
#             $hi_deg ............ intersection hour angle, degrees
#             $aet_mm ............ daily actual evapotranspiration, mm
# Features: This function calculates daily radiation, condensation, and
#           evaporation fluxes.
# Depends:  - kw ............. entrainment factor for PET
#           - calc_daily_solar daily radiation fluxes
#           - dcos() ......... cos(x*pi/180), where x is in degrees
#           - dsin() ......... sin(x*pi/180), where x is in degrees
#           - density_h2o() .. density of water
#           - elev2pres() ..... elevation dependent atm. pressure
#           - enthalpy_vap() . latent heat of vaporization
#           - psychro() ...... psychrometric constant
#           - sat_slope() .... slope of sat. pressure temp curve
# ************************************************************************
calc_daily_evap <- function(lat, n, elev=0, y=0, sw_in, tc=23.0, sw=1.0) {
	# ~~~~~~~~~~~~~~~~~~~~~~~~ FUNCTION WARNINGS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
	# if (!is.na(lat) > 90 | !is.na(lat) < -90) {
	# 	stop("Warning: Latitude outside range of validity (-90 to 90)!")
	# }
	# if (n < 1 || n > 366) {
	# 	stop("Warning: Day outside range of validity (1 to 366)!")
	# }
	
	# ~~~~~~~~~~~~~~~~~~~~~~~ FUNCTION VARIABLES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
	evap <- list()
	
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# 1. Calculate radiation fluxes
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	solar <- calc_daily_solar(lat, n, elev, y, sw_in, tc)
	ru <- solar$ru;
	rv <- solar$rv;
	rw <- solar$rw;
	rnl <- solar$rnl_w.m2;
	hn <- solar$hn_deg;
	rn_d <- solar$rn_j.m2;
	rnn_d <- solar$rnn_j.m2;
	evap$ra_j.m2  <- solar$ra_j.m2
	evap$rn_j.m2 <- solar$rn_j.m2
	evap$ppfd_mol.m2 <- solar$ppfd_mol.m2
	
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# 2. Calculate water-to-energy conversion (econ), m^3/J
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Atmospheric pressure, Pa
	patm <- elev2pres(elev)
	evap$patm_pa <- patm
	
	# Slope of saturation vap press temp curve, Pa/K
	s <- sat_slope(tc)
	evap$s_pa.k <- s
	
	# Enthalpy of vaporization, J/kg
	lv <- enthalpy_vap(tc)
	evap$lv_j.kg <- lv
	
	# Density of water, kg/m^3
	pw <- density_h2o(tc, patm)
	evap$pw_kg.m3 <- pw
	
	# Psychrometric constant, Pa/K
	gam <- psychro(tc, patm)
	evap$gam_pa.k <- gam
	
	econ <- s/(lv*pw*(s + gam))
	evap$econ_m3.j <- econ
	
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# 3. Calculate daily condensation (cn), mm
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	cn <- (1e3)*econ*abs(rnn_d)
	evap$cond_mm <- cn
	
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# 4. Estimate daily equilibrium evapotranspiration (eet_d), mm
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	eet_d <- (1e3)*econ*rn_d
	evap$eet_mm <- eet_d
	
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# 5. Estimate daily potential evapotranspiration (pet_d), mm
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	pet_d <- (1 + kw)*eet_d
	evap$pet_mm <- pet_d
	
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# 6. Calculate variable substitute (rx), (mm/hr)/(W/m^2)
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	rx <- (3.6e6)*(1 + kw)*econ
	evap$rx <- rx
	
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# 7. Calculate the intersection hour angle (hi), degrees
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	cos_hi <- sw/(rw*rv*rx) + rnl/(rw*rv) - ru/rv
	#     print( paste( "in evap: sw =", sw))
	#     print( paste( "in evap: ru =", ru))
	#     print( paste( "in evap: rv =", rv))
	#     print( paste( "in evap: rw =", rw))
	#     print( paste( "in evap: rx =", rx))
	#     print( paste( "in evap: rnl =", rnl))
	#     print( paste( "in evap: cos_hi =", cos_hi))
	if (cos_hi >= 1.0 || is.na(cos_hi)) {
		hi <- 0.0       # supply exceeds demand
	} else if (cos_hi <= -1.0) {
		hi <- 180.0     # supply limits demand everywhere
	} else {
		hi <- acos(cos_hi)
		hi <- hi/pir
	}
	evap$hi_deg <- hi
	
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# 10. Estimate daily actual evapotranspiration (aet_d), mm
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	aet_d <- (24/pi)*(
		sw*hi*pir +
		rx*rw*rv*(dsin(hn) - dsin(hi)) +
		(rx*rw*ru - rx*rnl)*(hn - hi)*pir
	)
	
	
	evap$aet_mm <- aet_d
	
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~ RETURN VALUES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
	return(evap)
}
