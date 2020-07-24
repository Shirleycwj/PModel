# Constant possibly used in TerrA-P Model
# Created on 20180317 1203

###############################################################################
# GLOBAL CONSTANTS:
###############################################################################
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
# Evans, A. V. Gallego-Sala, M. T. Sykes, and W. Cramer, Simple process-
# led algorithms for simulating habitats (SPLASH): Robust indices of radiation,
# evapotranspiration and plant-available moisture, Geoscientific Model
# Development, 2016 (in progress)

kA = 107.        # constant for Rnl (Monteith & Unsworth, 1990)
kalb_sw = 0.17   # shortwave albedo (Federer, 1968)
kalb_vis = 0.03  # visible light albedo (Sellers, 1985)
kb = 0.20        # constant for Rnl (Linacre, 1968)
kc = 0.25        # cloudy transmittivity (Linacre, 1968)
kCw = 1.05       # supply constant, mm/hr (Federer, 1982)
kd = 0.50        # angular coefficient of transmittivity (Linacre, 1968)
ke = 0.0167      # eccentricity for 2000 CE (Berger, 1978)
keps = 23.44     # obliquity for 2000 CE, degrees (Berger, 1978)
kfFEC = 2.04     # from flux to energy conversion, umol/J (Meek et al., 1984)
kG = 9.80665     # gravitational acceleration, m/s^2 (Allen, 1973)
kGsc = 1360.8    # solar constant, W/m^2 (Kopp & Lean, 2011)
kL = 0.0065      # temperature lapse rate, K/m (Allen, 1973)
kMa = 0.028963   # molecular weight of dry air, kg/mol (Tsilingiris, 2008)
kMv = 0.01802    # molecular weight of water vapor, kg/mol (Tsilingiris, 2008)
kPo = 101325     # standard atmosphere, Pa (Allen, 1973)
kR = 8.31447     # universal gas constant, J/mol/K (Moldover et al., 1988)
kTo = 298.15     # base temperature, K (Berberan-Santos et al., 1997)
kWm = 150.       # soil moisture capacity, mm (Cramer & Prentice, 1988)
kw = 0.26        # entrainment factor (Lhomme, 1997; Priestley & Taylor, 1972)
komega = 283.0   # longitude of perihelion for 2000 CE, degrees (Berger, 1978)
pir = (pi/180.0)

###############################################################################
# TERRAP CONSTANTS:
###############################################################################
alpha = 1.0

gamma_25 = 4.220 # Pa  Gamma* at 25C
t_25 = 298.15 # K 25C in Kelvin
Ha = 37830.0 #J/mol Activiation energy for Gamma*
R = 8.3145 # J/mol/K Universal gas constant
eta_const = -0.0227 # No unit  Constant for viscosity of water relative to its value at 25C (See Wang 2015)
kco =  2.09476e5 #ppm. US standard pressure. (From Beni's code - Ref Bernacchi et al 2001)
kPo = 101325.0  # Standard atmopsheric pressure (Pa), Allen 1973

Ha_ko = 36380 #J/mol  Bernacchi 2001 energy of activation for oxygenation
Ha_kc = 79430 # J/mol Bernacchi 2001 energy of activation for carboxylation

kc25 = 39.97 # Pa at 25 deg C and 98.716KPa
ko25 = 27480 # Pa, at 25 deg C and 98.716KPa
es0 = 0.611

beta = 240.0 # Email from Wang Han # No unit  Ratio of carboxylation and transpiration costs at 25C--> for ci:ca calcualtions (least cost hypothesis. Using approximate value = 240 
# used in m_calculation

c_star = 0.406 # unit of carbon cost for maintenance of electron transport capacity (obs Jmax:Vc max) (Wang Han emails)
a_hat = 4.4 # Standard value for c13 discrimination - diffusion component (Wang 2015 Eq s42)
b_hat = 27.0 # Standard value for c13 discrimination - biochemical component (Wang 2015 Eq s42)

c_molmass =  12.0107  # g C / mol C

# C3_phi0 = 0.088 * c_molmass # # mol/mol From Skillman 2008 & Long et al. 1993 # gC/mol Intrinisic quantum yield of photosynthesis for C3 plants
# C3_phi0 updated on 20180626 after calibration
# C4_phi0 = 0.053 * c_molmass # # mol/mol From Skillman 2008 & Long et al. 1993 # gC/mol Intrinisic quantum yield of photosynthesis for C4 plants
# C4_phi0 updated on 20180626 after calibration

absG = 1.0 # Muliptying facotr for fAPAR sometimes
kfFEC = 2.04     # from flux to energy conversion, umol/J (Meek et al., 1984) # From Tyler/Beni

k2c = 273.15