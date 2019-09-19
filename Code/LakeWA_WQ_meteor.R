########################
# Lake Washington WQ and Meteorological data
########################
# Plan ------------------------

# 1. convert solar radiation from meteorological data file to PAR using conversion factors
# in Holtgrieve et al 2010
#   a. Find if there are other conversion factors for cloudy days or smoke

# 2. QAQC water quality data using website thresholds to make sure there aren't negative values
# for things like chl a (e.g. convert values between 0 and -2 to 0; values less than -2 change to NA)
# see website: https://green2.kingcounty.gov/lake-buoy/Parameters.aspx

# 3. Calculate productivity using Vadeboncoeur equations from file RD_functions.R

# Load libraries ---------------
library(dplyr)
source('Code/RD_functions.R')
library(tidyr)
library(rLakeAnalyzer) # has function thermo.depth() to find thermocline with some bs check for unique depths

# packages chron or lubridate function yday
library(lubridate)

# Import data ------------------
WQ <- read.csv("Data/Lake_WA_WQ.csv", header = T) 
meteor <- read.csv("Data/Lake_WA_meteor.csv", header = T)
AquaPAR <- read.csv("Data/SeattleAquar_PAR.csv", header = T) # use this because lake surface buoy readings seem strange
  # NOTE: AquarPAR readings are in micro moles while PAR from GlobalIrradiance PAR are micro Einsteins
# but apparently that is the same unit. Also, this is in 15 minute intervals
PARcs_df <- readRDS("Output/PAR_clear_sky.RDS") # comes from file "GlobalIrradiance.R"

# cone <- read.csv("Data/lw_tenth_mbins.csv", header = T) # Bathymetry data
cone <- read.csv("Data/Lake_WA_area_vol.csv", header = T)

str(cone)
#colnames(cone) <- c("depth", "radius", "surf_area", "volume", "side_area")

AOD <- readRDS("Data/2019-04-05aerosol_dat.rds")
str(AOD)
str(WQ)
str(meteor)

# 1. Convert solar radiation to PAR at surface of lake 
# from watts per meter squared to particle flux (micro Einsteins per second per meter squared)
meteor.QC <- meteor
colnames(meteor.QC) <- c("date", "rel_humidity", "solar_rad_Wsqm", "atm_press", "wind_speed", "wind_dir", "bat_volts",
                         "air_temp", "pamout")
meteor.QC$PAR_surf_E <- meteor.QC$solar_rad_Wsqm*0.45*4.57

###
# PAR PAR ############################################
###
plot(PARcs_df$hhour, PARcs_df$PAR_particleFlux)

# Join PAR clear sky to PAR from Seattle Aquarium PAR ---------------------------------------------------------------
str(AquaPAR)

AquaPARs <- AquaPAR
colnames(AquaPARs) <- c("Date", "PAR_surf_Flux", "Qual_Surf_PAR")
AquaPARs$Date <- as.POSIXct(strptime(AquaPARs$Date, "%m/%d/%y %H:%M")) #, tz = "America/Los Angeles")

# Get hour and minutes in format to join with PARcs_df
AquaPAR.dates <- AquaPARs %>% mutate(doy = yday(Date), year = year(Date), hour = hour(Date), minute = minute(Date)/60)
str(AquaPAR.dates)
AquaPARs <- AquaPAR.dates %>% mutate(hhour = hour + minute)

# make recognizable date format
PARcs_df2 <- PARcs_df %>% mutate(Date = with_tz(with(PARcs_df, ymd_hm(paste(year, month, day, paste(floor(PARcs_df$hhour), (PARcs_df$hhour - floor(PARcs_df$hhour)) * 60, sep = ':'), sep= '-')), tz = "UTC")), tzone =  "America/Los_Angeles")
plot(hour(PARcs_df2$Date), PARcs_df$PAR_particleFlux)

PAR_all <- inner_join(AquaPARs, PARcs_df2, by = "Date")
plot(PAR_all$PAR_particleFlux, PAR_all$PAR_surf_Flux)

# 2. QAQC WQ data
WQ.QC <- WQ

# rename columns to short names
colnames(WQ.QC) <- c("date", "depth", "temp", "sp_cond", "DO_conc", "DO_sat", "pH", "chla", "NTU", "phyco", "prov",
                     "end_check", "parmout", "sonde", "prof_number")
str(WQ.QC)

# QC chla
# Replacing negative values with zero because negative values are outside the real range of chlorophyll values
# Changes in values to min and max thresholds comes from the buoy sonde expected range values published on-line
WQ.QC$chla[WQ.QC$chla <= 0 & WQ.QC$chla >= -2] <- 0 # units: micrograms/liter
WQ.QC$chla[WQ.QC$chla < (-2)] <- NA
WQ.QC$chla[WQ.QC$chla >= 50]

WQ.QC$depth # units: meters

WQ.QC$pH[WQ.QC$pH < 4] <- 4
WQ.QC$pH[WQ.QC$pH > 10]

WQ.QC$DO_conc[WQ.QC$DO_conc < 0.01] <- NA # units: milligrams/liter
WQ.QC$DO_conc[WQ.QC$DO_conc > 22]

WQ.QC$DO_sat[WQ.QC$DO_sat < 0.10] # %
WQ.QC$DO_sat[WQ.QC$DO_sat > 190]

WQ.QC$NTU[WQ.QC$NTU < (-5)]
WQ.QC$NTU[WQ.QC$NTU > (50)]

str(WQ.QC)
# Plug and chug
# Eqn 4 Phytoplankton productivity (units in Vadeboncouer mgCarbon/m^3*h) ---------------------
WQ.short2 <- WQ.QC[!is.na(WQ.QC$temp),]
WQ.short2 <- WQ.short2 %>% mutate(PP_max = PPmax_func(chla))
str(WQ.short2)

# Eqn 5 Find thermocline ---------------------------



# Eqn 6 Light attenuation function (Kd) ------------------
WQ.short <- WQ.short2 %>% mutate(Kd = Light_atten_func(chla))
str(WQ.short)
WQ.short[WQ.short$depth >= 1 & WQ.short$depth <= 2,]

# Eqn 7 Light at depth z time t --------------------------
# Need to join I0t and WQ data base so the vectors will line up
WQ.short$date <- as.POSIXct(strptime(WQ.short$date, "%m/%d/%Y %H:%M"))
WQ.short <- WQ.short %>% mutate(doy = yday(date), year = year(date), hour = hour(date), date2 = round_date(date, "15 minutes")) # minute = minute(date)/60

table(WQ.short$doy, WQ.short$year) # some years have missing days of observation 
unique(WQ.short$doy[WQ.short$year==2015])
# Light without accounting for anything will be PAR_all$PAR_surf_Flux

# Join WQ to PAR
str(WQ.short)
str(PAR_all)
WQ_PAR <- inner_join(WQ.short, PAR_all, by = c("date2" = "Date"))
str(WQ_PAR) # small df because aquarium only has PAR values from 2015 on

WQ_PAR <- WQ_PAR %>% mutate(Izt = Izt_func(I0t = PAR_surf_Flux, Kd = Kd, z = depth))
WQ_PAR[WQ_PAR$depth2 == 1,]
# Eqn 9 daily phytoplankton primary production at depth z
# Must have joined PARcs_df to WQ_PAR by date/time to get daylengths
# Need to join to bathymetry data to have volume at depths
WQ_PAR <- WQ_PAR %>% mutate(depth2 = round(depth)) # b/c "cone" is in 1m bins

WQ_PAR_cone <- inner_join(WQ_PAR, cone, by = c("depth2" = "DEPTH_BIN_M"))
  # Bathymetry LW_1 m bins for Vzdel and Azdel 

str(WQ_PAR_cone)
WQ_PAR_cone[WQ_PAR_cone$depth2 >= 1 & WQ_PAR_cone$depth2 <= 10,]
WQ_PAR_cone$PP_max

WQ_PAR_cone <- WQ_PAR_cone  %>% mutate(PP_depth = PPdepth_func(PPmax = PP_max, daylength = daylight,
                                                              Izt = Izt, Vzdel = VOL_M3_CROSS_SECTION),
                                       BP_depth = BPz_func(Izt = Izt, Azdel = AREA_SQ_M))

# Eq 10 and 12 ----------------------------------
PP_WQ_PAR_cone <- WQ_PAR_cone %>% group_by(doy.x, year.x) %>% mutate(TPP = TPP_func(PPdepth = PP_depth), # units are smaller than partial lake estimates b/c in mg C per meter squared
                                                                      TBP = TBP_func(BPz = BP_depth)) # units are smaller than partial lake estimates b/c in mg C per meter squared
str(PP_WQ_PAR_cone)
PP_WQ_PAR_cone$TPP # supposedly mg C/m^2
max(PP_WQ_PAR_cone$doy.y)

# Plot some example results. Just using first day of record as example.-------------------------------
plot(PP_WQ_PAR_cone$doy.y[PP_WQ_PAR_cone$depth2 == 1 & PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250],
     PP_WQ_PAR_cone$TPP[PP_WQ_PAR_cone$depth2 == 1 & PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250]/1000,
     type = "l", ylab = "Daily phytoplankton production (g C m-2)", xlab = "Dates",
     ylim = c(0.01,3))
lines(PP_WQ_PAR_cone$doy.y[PP_WQ_PAR_cone$depth2 == 1 & PP_WQ_PAR_cone$year == 2016 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250],
     PP_WQ_PAR_cone$TPP[PP_WQ_PAR_cone$depth2 == 1 & PP_WQ_PAR_cone$year == 2016 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250]/1000,
     col = "blue")
lines(PP_WQ_PAR_cone$doy.y[PP_WQ_PAR_cone$depth2 == 1 & PP_WQ_PAR_cone$year == 2017 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250],
      PP_WQ_PAR_cone$TPP[PP_WQ_PAR_cone$depth2 == 1 & PP_WQ_PAR_cone$year == 2017 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250]/1000,
      col = "red")


plot(PP_WQ_PAR_cone$date[PP_WQ_PAR_cone$depth2 == 1 & PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250],
     PP_WQ_PAR_cone$PP_max[PP_WQ_PAR_cone$depth2 == 1 & PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250],
     type = "l", ylab = "Daily phytoplankton production (g C m-2)", xlab = "Dates",
     ylim = c(-0.01,10))
lines(PP_WQ_PAR_cone$date[PP_WQ_PAR_cone$depth2 == 10 & PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250],
      PP_WQ_PAR_cone$PP_max[PP_WQ_PAR_cone$depth2 == 10 & PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250],
      col = "blue")
lines(PP_WQ_PAR_cone$date[PP_WQ_PAR_cone$depth2 == 30 & PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250],
      PP_WQ_PAR_cone$PP_max[PP_WQ_PAR_cone$depth2 == 30 & PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250],
      col = "red")

max(WQ.QC$depth)

plot(PP_WQ_PAR_cone$depth2[PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y == 182][35:89],
     PP_WQ_PAR_cone$PP_max[PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y == 182][35:89],
     type = "l", ylab = "Daily phytoplankton production (g C m-2)", xlab = "Depth",
     ylim = c(0,20))
plot(PP_WQ_PAR_cone$depth2[PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y == 182][90:144],
     PP_WQ_PAR_cone$PP_max[PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y == 182][90:144],
     type = "l", ylab = "Carbon uptake (mg C m-3)", xlab = "Depth",
     ylim = c(0,20))

plot(PP_WQ_PAR_cone$doy.y[PP_WQ_PAR_cone$depth2 == 1 & PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250],
     PP_WQ_PAR_cone$TPP[PP_WQ_PAR_cone$depth2 == 1 & PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250]/1000,
     type = "l", ylab = "Daily phytoplankton production (g C m-2)", xlab = "Dates",
     ylim = c(0.01,0.5))
lines(PP_WQ_PAR_cone$doy.y[PP_WQ_PAR_cone$depth2 == 1 & PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250],
     PP_WQ_PAR_cone$NTU[PP_WQ_PAR_cone$depth2 == 1 & PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250]/1000,
     type = "l", ylab = "Daily phytoplankton production (g C m-2)", xlab = "Dates",
     ylim = c(0.01,0.5))

PP_WQ_PAR_cone$date[PP_WQ_PAR_cone$depth2 == 1 & PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250]
PP_WQ_PAR_cone$TPP[PP_WQ_PAR_cone$depth2 == 1 & PP_WQ_PAR_cone$year == 2015 & PP_WQ_PAR_cone$doy.y >= 182 & PP_WQ_PAR_cone$doy.y <= 250]

