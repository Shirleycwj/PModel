# splash Main point
#
# VERSION: 1.0
# LAST UPDATED: 2012-02-19
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
# This script runs the SPLASH model for one year.
#
# ~~~~~~~~~~
# changelog:
# ~~~~~~~~~~
# - updated monthly and daily results & process [15.01.13]
# - updated plots of results [15.01.16]
# - added example data CSV file [15.01.16]
# - fixed Cramer-Prentice alpha definition [15.01.16]
# - updated monthly results plot [15.01.22]
# - added write out for daily/monthly results [15.01.27]
# - added example of yearly looping [15.01.27]
# - updated year extraction from filename in yearly loop example [15.01.30]
# - fixed plots for Figs. 3 & 4 of manuscript [15.11.23]
# - fixed directory paths [16.02.17]
# - working with any grid datasets [16.02.18]
# - spin-up pixels in parallel [16.02.18]
# - call topmodel R package to calculate topo idx, streams and upslope area  [16.02.18]
# - extract latitude from the data  [16.02.18]
# - calculates slope and aspect from "raster" R package  [16.02.18]
# - calculates streamflow from runoff and baseflow grids

splash<-function(sw_in, tc, pn, lat,elev,outfolder=getwd()){
	source("const.R")
	source("evap.R")
	source("solar.R")
	source("splash.R")
	
	require(compiler)
	enableJIT(3)
	require(xts)
	# Extract time info from data
	tm <- seq(as.Date(paste(start_year,"01","01",sep = "/")),as.Date(paste(start_year,"12","31",sep = "/")),by="days")
	y<-as.numeric(unique(format(tm,'%Y')))
	ny <- julian_day(y + 1, 1, 1) - julian_day(y, 1, 1)
	ztime<-tm
	time.freq<-abs(as.numeric(ztime[1]-ztime[2], units = "days"))
	
	
	if (time.freq<2){		
		
		if (length(y)==1){
			initial<-spin_up(lat,elev, sw_in, tc, pn, y[1])
			
			result<-run_one_year(lat,elev, sw_in, tc, pn,initial,y[1])
			result<-xts(result,ztime)
		}
		else if(length(y)>1){
			
			end<-cumsum(ny)
			start<-end+1
			result<-list()
			initial<-spin_up(lat,elev, sw_in[1:ny[1]], tc[1:ny[1]], pn[1:ny[1]], y[1])
			
			result[[1]]<-run_one_year(lat,elev, sw_in[1:ny[1]], tc[1:ny[1]], pn[1:ny[1]],initial,y[1])
			
			for (i in 2:length(y)){
				stidx<-i-1
				# correct for leap years	
				if(ny[stidx]<ny[i]){
					result[[i]]<-run_one_year(lat,elev, sw_in[start[stidx]:end[i]], tc[start[stidx]:end[i]], pn[start[stidx]:end[i]],c(result[[stidx]]$wn,result[[stidx]]$wn[1]),y[i])
					
				}
				else if(ny[stidx]>ny[i]){
					result[[i]]<-run_one_year(lat,elev, sw_in[start[stidx]:end[i]], tc[start[stidx]:end[i]], pn[start[stidx]:end[i]],result[[stidx]]$wn[1:365],y[i])
				}
				else if(ny[stidx]==ny[i]){
					result[[i]]<-run_one_year(lat,elev, sw_in[start[stidx]:end[i]], tc[start[stidx]:end[i]], pn[start[stidx]:end[i]],result[[stidx]]$wn,y[i])
				}	
				
			}
			# order results as time series
			result<-Reduce(rbind,result)
			result<-xts(result,ztime)	
			
			
		}
		
		
		
		
		
	}
	
	else if (time.freq>20){		
		ztime.days<-seq(as.Date(paste(y[1],1,sep="-"),format="%Y-%j"),as.Date(paste(y[length(y)],ny[length(y)],sep="-"),format="%Y-%j"), by="day")
		if (length(y)==1){
			initial<-spin_up(lat,elev, sw_in, tc, pn, y[1])
			
			result<-run_one_year(lat,elev, sw_in, tc, pn,initial,y[1])
			result<-xts(result,ztime)
		}
		else if(length(y)>1){
			nm <- rep(12,length(y))
			end<-cumsum(nm)
			start<-end-11
			
			result<-list()
			initial<-spin_up(lat,elev, sw_in[1:end[1]], tc[1:end[1]], pn[1:end[1]], y[1])
			
			result[[1]]<-run_one_year(lat,elev, sw_in[1:end[1]], tc[1:end[1]], pn[1:end[1]],initial,y[1])
			
			for (i in 2:length(y)){
				stidx<-i-1
				# correct for leap years	
				if(ny[stidx]<ny[i]){
					result[[i]]<-run_one_year(lat,elev, sw_in[start[i]:end[i]], tc[start[i]:end[i]], pn[start[i]:end[i]],c(result[[stidx]]$wn,result[[stidx]]$wn[1]),y[i])
					
				}
				else if(ny[stidx]>ny[i]){
					result[[i]]<-run_one_year(lat,elev, sw_in[start[i]:end[i]], tc[start[i]:end[i]], pn[start[i]:end[i]],result[[stidx]]$wn[1:365],y[i])
				}
				else if(ny[stidx]==ny[i]){
					result[[i]]<-run_one_year(lat,elev, sw_in[start[i]:end[i]], tc[start[i]:end[i]], pn[start[i]:end[i]],result[[stidx]]$wn,y[i])
				}	
				
			}
			# order results as time series
			result<-Reduce(rbind,result)
			result<-xts(result,ztime.days)	
			
			
		}
		
		
		
		
		
	}
	return(result)	
}


