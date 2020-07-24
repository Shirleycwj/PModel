# splash-original.R
#
# VERSION: 1.0
# LAST UPDATED: 2016-02-19
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
# This script contains functions for running SPLASH for point-based data, i.e.:
#   spin_up(list mdat, list dtot)
#   quick_run(double lat, double elev, double n, double y, double wn, double sw_in,
#             double tc, double pn)
#   run_one_day(double lat, double elev, double n, double y, double wn,
#               double sw_in, double tc, double pn)
#
# ~~~~~~~~~~
# changelog:
# ~~~~~~~~~~
# - added fix to daily soil moisture when n and ny are 365 [15.01.27]
#
#### IMPORT SOURCES ##########################################################
# source("const.R")
# source("evap.R")


#### DEFINE FUNCTIONS ########################################################
# ************************************************************************
# Name:     cum.interp, avg.interp
# Inputs:   
#               x.month ..... double, variable monthly value
#               y ....... year
# Returns:  double, daily values
# Features: Interpolate monthly values to daily values
# Depends:  julian day
# ************************************************************************
cum.interp<-function(x.months,y){
	ny <- julian_day(y + 1, 1, 1) - julian_day(y, 1, 1)
	ndaysmonth<-rep(NA,12)
	for(i in 1: 12){ndaysmonth[i]<-julian_day(y,i+1,1)-julian_day(y,i,1)}
	x.days<-x.months/ndaysmonth
	ind.month<-seq(as.Date(paste(y,1,sep="-"),format="%Y-%j"),as.Date(paste(y,ny,sep="-"),format="%Y-%j"), by="month")
	ind.day<-seq(as.Date(paste(y,1,sep="-"),format="%Y-%j"),as.Date(paste(y,ny,sep="-"),format="%Y-%j"), by="day")
	if (sum(!is.na(x.months)) < 2) {return(rep(NA, ny))} 
	else {approx(ind.month, x.days, ind.day, method = "linear", rule=2)$y}
}

avg.interp<-function(x.months,y){
	ny <- julian_day(y + 1, 1, 1) - julian_day(y, 1, 1)
	ind.month<-seq(as.Date(paste(y,1,sep="-"),format="%Y-%j"),as.Date(paste(y,ny,sep="-"),format="%Y-%j"), by="month")
	ind.day<-seq(as.Date(paste(y,1,sep="-"),format="%Y-%j"),as.Date(paste(y,ny,sep="-"),format="%Y-%j"), by="day")
	if (sum(!is.na(x.months)) < 2) {return(rep(NA, ny))} 
	else {approx(ind.month, x.months, ind.day, method = "linear", rule=2)$y}
}




# ************************************************************************
# Name:     spin_up
# Inputs:   - list, meteorological data (mdat)
#               $num_lines ..... double, length of meteorol. variable lists
#               $lat_deg ....... double latitude (degrees)
#               $elev_m ......... double, elevation (m)
#               $year .......... double, year
#               $sw_in ............ list, fraction of sunshine hours
#               $tair .......... list, mean daily air temperature (deg. C)
#               $pn ............ list, precipitation (mm/d)
#           - list, daily totals (dtot)
#               $wm ............ list, daily soil moisture (mm)
# Returns:  list, daily totals
# Features: Updates the soil moisture in daily totals until equilibrium
# Depends:  quick_run
# ************************************************************************
spin_up <-function(lat,elev, sw_in, tc, pn, y) {
	# Run one year:
	ny <- julian_day(y + 1, 1, 1) - julian_day(y, 1, 1)
	
	wn<-rep(0,ny)
	if(length(sw_in)==12){sw_in<-avg.interp(sw_in,y)}
	if(length(tc)==12){tc<-avg.interp(tc,y)}
	if(length(pn)==12){pn<-cum.interp(pn,y)}
	
	for(n in 1:ny){
		
		if (n == 1) {
			wn[n] <- wn[ny]
		} else {
			wn [n]<- wn[n - 1]
		}
		wn[n]<- quick_run(lat, elev, n, y, wn[n], sw_in[n], tc[n], pn[n])
		
		
	}
	
	# Calculate the change:
	start_sm <- wn[1]
	end_vals <- quick_run(lat, elev, 1, y,
		wn[ny],sw_in[1], tc[1], pn[1])
	diff_sm <- abs(end_vals - start_sm)
	
	# # Equilibrate:
	spin_count <- 1
	while (diff_sm > 1.0) { 
		for(n in 1:ny){
			
			if (n == 1) {
				wn[n] <- wn[ny]
			} else {
				wn[n]<- wn[n - 1]
			}
			wn[n]<- quick_run(lat, elev, n, y, wn[n], sw_in[n], tc[n], pn[n])
			
			
		}
		# Calculate the change:
		start_sm <- wn[1]
		end_vals <- quick_run(lat, elev, 1, y,
			wn[ny],sw_in[1], tc[1], pn[1])
		diff_sm <- abs(end_vals - start_sm)
		
		spin_count <- spin_count + 1
		# cat(paste("Spun", spin_count, "years\n"))
		# return(dtot)
	}
	return(wn)
}


# ************************************************************************
# Name:     quick_run
# Inputs:   - double, latitude, deg (lat)
#           - double, elevation, m (elev)
#           - double, day of year (n)
#           - double, year (y)
#           - double, daily soil moisture content, mm (wn)
#           - double, daily fraction of bright sunshine (sw_in)
#           - double, daily air temperature, deg C (tc)
#           - double, daily precipitation, mm (pn)
# Returns:  list
#             $sm - soil moisture, mm
#             $ro - runoff, mm
# Features: Returns daily soil moisture and runoff
# Depends:  evap
# ************************************************************************
quick_run <- function(lat, elev, n, y, wn, sw_in, tc, pn) {
	# Calculate evaporative supply (mm/hr)
	sw <- kCw*wn/kWm
	
	# Compute daily radiation and evaporations values:
	ET <- calc_daily_evap(lat, n, elev, y, sw_in, tc, sw)
	
	# Update daily soil moisture:
	if (is.na(ET$cond_mm-ET$aet_mm)==T) {
	  sm <- wn + pn
	} else {
	  sm <- wn + pn + ET$cond_mm - ET$aet_mm
	}
	
	if (sm > kWm) {
		# Bucket is full:
		# - set soil moisture to capacity
		# - add remaining water to runoff
		ro <- sm - kWm
		sm <- kWm
	} else if (sm < 0) {
		# Bucket is empty:
		# - set runoff and soil moisture equal to zero
		ro <- 0
		sm <- 0
	} else {
		ro <- 0
	}
	
	rval <- list()
	rval$sm <- sm
	rval$ro <- ro
	return(sm)
}


# ************************************************************************
# Name:     run_one_day
# Inputs:   - double, latitude, deg (lat)
#           - double, elevation, m (elev)
#           - double, day of year (n)
#           - double, year (y)
#           - double, daily soil moisture content, mm (wn)
#           - double, daily fraction of bright sunshine (sw_in)
#           - double, daily air temperature, deg C (tc)
#           - double, daily precipitation, mm (pn)
# Returns:  list
#             $ho - daily solar irradiation, J/m2
#             $hn - daily net radiation, J/m2
#             $ppfd - daily PPFD, mol/m2
#             $cond - daily condensation water, mm
#             $eet - daily equilibrium ET, mm
#             $pet - daily potential ET, mm
#             $aet - daily actual ET, mm
#             $wn - daily soil moisture, mm
#             $ro - daily runoff, mm
# Features: Runs SPLASH at a single location for one day.
# Depends:  evap
# ************************************************************************
run_one_day <- function(lat, elev, n, y, wn, sw_in, tc, pn) {
	# Return values
	rvals <- list()
	
	# Calculate evaporative supply (mm/hr)
	sw <- kCw*wn/kWm
	
	# Compute daily radiation and evaporations values:
	ET <- calc_daily_evap(lat, n, elev, y, sw_in, tc, sw)
	rvals$ho <- ET$ra_j.m2
	rvals$hn <- ET$rn_j.m2
	rvals$ppfd <- ET$ppfd_mol.m2
	rvals$cond <- ET$cond_mm
	rvals$eet <- ET$eet_mm
	rvals$pet <- ET$pet_mm
	rvals$aet <- ET$aet_mm
	
	# Update daily soil moisture:
	if (is.na(ET$cond_mm-ET$aet_mm)==T) {
	  sm <- wn + pn
	} else {
	  sm <- wn + pn + ET$cond_mm - ET$aet_mm
	}
	#sm <- wn + pn + ET$cond_mm - ET$aet_mm
	
	# print( paste( "in run_one_day: pn =", pn))
	
	if (sm > kWm) {
		# Bucket is full:
		# - set soil moisture to capacity
		# - add remaining water to runoff
		ro <- sm - kWm
		sm <- kWm
	} else if (sm < 0) {
		# Bucket is empty:
		# - reduce actual ET by discrepancy amount
		# - set runoff and soil moisture equal to zero
		rvals$aet <- rvals$aet + sm
		ro <- 0
		sm <- 0
	} else {
		ro <- 0
	}
	
	rvals$wn <- sm
	rvals$ro <- ro
	return(rvals)
}
# ************************************************************************
# Name:     run_one_year
# Inputs:   - double, latitude, deg (lat)
#           - double, elevation, m (elev)
#           - double, day of year (n)
#           - double, year (y)
#           - double, daily soil moisture content, mm (wn)
#           - double, daily fraction of bright sunshine (sw_in)
#           - double, daily air temperature, deg C (tc)
#           - double, daily precipitation, mm (pn)
# Returns:  list
#             $ho - daily solar irradiation, J/m2
#             $hn - daily net radiation, J/m2
#             $ppfd - daily PPFD, mol/m2
#             $cond - daily condensation water, mm
#             $eet - daily equilibrium ET, mm
#             $pet - daily potential ET, mm
#             $aet - daily actual ET, mm
#             $wn - daily soil moisture, mm
#             $ro - daily runoff, mm
# Features: Runs SPLASH at a single location/pixel for one year.
# Depends:  run_one_day
# ************************************************************************
run_one_year <- function(lat,elev, sw_in, tc, pn,wn,y) {
	ny <- julian_day(y + 1, 1, 1) - julian_day(y, 1, 1)
	if(length(sw_in)==12){sw_in<-avg.interp(sw_in,y)}
	if(length(tc)==12){tc<-avg.interp(tc,y)}
	if(length(pn)==12){pn<-cum.interp(pn,y)}
	if(length(wn)==12){wn<-avg.interp(wn,y)}
	
	daily_totals <- matrix(data=rep(0, 9*ny), nrow=ny, ncol=9)
	daily_totals <- as.data.frame(daily_totals)
	names(daily_totals) <- c("ho",   # daily solar irradiation, J/m2
		"hn",   # daily net radiation, J/m2
		"ppfd",   # daily PPFD, mol/m2
		"cond",   # daily condensation, mm
		"wn",   # daily soil moisture, mm
		"ro",   # daily runoff, mm
		"eet", # daily equilibrium ET, mm
		"pet", # daily potential ET, mm
		"aet"# daily actual ET, mm
		) # daily baseflow mm
	
	for(n in 1:ny){
		
		if (n == 1) {
			daily_totals$wn[n] <- wn[ny]
		} else {
			daily_totals$wn[n]<- daily_totals$wn[n - 1]
		}
		rvals<- run_one_day(lat, elev, n, y, daily_totals$wn[n], sw_in[n], tc[n], pn[n])
		daily_totals$wn[n]<-rvals$wn
		daily_totals$ho[n]<-rvals$ho 
		daily_totals$hn[n]<-rvals$hn 
		daily_totals$ppfd[n]<-rvals$ppfd 
		daily_totals$cond[n]<-rvals$cond 
		daily_totals$eet[n]<-rvals$eet
		daily_totals$pet[n]<-rvals$pet 
		daily_totals$aet[n]<-rvals$aet 
		daily_totals$ro[n]<-rvals$ro
		
	}
	# close(pb)
	return(daily_totals)
}


