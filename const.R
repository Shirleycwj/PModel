
# const.R
#
# VERSION: 1.0-r1
# LAST UPDATED: 2017-11-11
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
# This script contains the global constants defined in SPLASH.
#
# NOTE: orbital parameters: eccentricity, obliquity, and longitude of the
# perihelion, are assumed constant while they infact vary slightly over time.
# There are methods for their calculation (e.g., Meeus, 1991). Eccentricity
# varies 0.005--0.072 and is decreasing at rate of 0.00004 per century.
# Obliquity varies 22.1--24.5 degrees with a period of ~41000 years.
#
# ~~~~~~~~~~
# changelog:
# ~~~~~~~~~~
# - updated values and references for ka and kR [14.10.31]
# - reduced list of constants [15.01.13]
# - updated kR and kTo values and references [15.03.24]
# - added Photosynthesis constants from p model python version [15.03.24]
#
kA <- 170          # constant for Rl (Monteith & Unsworth, 1990)
# kalb_sw <- 0.17     # shortwave albedo (Federer, 1968)
kalb_sw <- 0.17     # shortwave albedo (Federer, 1968)
kalb_vis <- 0.03    # visible light albedo (Sellers, 1985)
kb <- 0.2         # constant for Rl (Linacre, 1968; Kramer, 1957)
kc <- 0.25          # constant for Rs (Linacre, 1968)
kCw <- 1.05        # supply constant, mm/hr (Federer, 1982)
kd <- 0.50          # constant for Rs (Linacre, 1968)
kfFEC <- 2.04       # from-flux-to-energy, umol/J (Meek et al., 1984)
kfus <- 334000      # latent heat of fusion J/Kg (Monteith & Unsworth, 1990)
kG <- 9.80665       # gravitational acceleration, m/s^2 (Allen, 1973)
kGsc <- 1360.8      # solar constant, W/m^2 (Kopp & Lean, 2011)
kL <- 0.0065        # adiabatic lapse rate, K/m (Cavcar, 2000)
kMa <- 0.028963     # molecular weight of dry air, kg/mol (Tsilingiris, 2008)
kMv <- 0.01802      # mol. weight of water vapor, kg/mol (Tsilingiris, 2008)
kSecInDay <- 86400  # number of seconds in a day
kp_snow<- 250       # snow density kg/m3 (Bonan et al., 2002)
kPo <- 101325       # standard atmosphere, Pa (Allen, 1973)
kR <- 8.31447       # universal gas constant, J/mol/K (Moldover et al., 1988)
kTo <- 288.15       # base temperature, K (Berberan-Santos et al., 1997)
kWm <- 150          # soil moisture capacity, mm (Cramer-Prentice, 1988)
kw <- 0.26          # PET entrainment, (1+kw)*EET (Priestley-Taylor, 1972)
pir <- pi/180       # pi in radians
fluidity<-35187037  # fluidity as defined en Hillel (1998), mm^2 (density*gravity)/viscosity
# Paleoclimate variables:
ke <- 0.01670       # eccentricity of earth's orbit, 2000CE (Berger 1978)
keps <- 23.44       # obliquity of earth's elliptic, 2000CE (Berger 1978)
komega <- 283       # lon. of perihelion, degrees, 2000CE (Berger, 1978)

# Photosynthesis constants
###############################################################################

# kc = 0.41       # Jmax cost coefficient
# kphio = 0.093   # quantum efficiency (Long et. al., 1993)
gs25 = 4.220    # gamma-star at 25C, Pa (assuming 25 deg C & 98.716 kPa)
dha = 37830     # gamma-star activation energy, J/mol
kc25 = 39.97    # Michaelis-Menten Kc, Pa (assuming 25 deg C & 98.716 kPa)
ko25 = 2.748e4  # Michaelis-Menten, Pa (assuming 25 deg C & 98.716 kPa)
dhac = 79430    # Michaelis-Menten CO2 activation energy, J/mol
dhao = 36380    # Michaelis-Menten O2 activation, J/mol
kco = 2.09476e5   # atmos. CO2 concentration, ppm (US Standard Atmosphere)
