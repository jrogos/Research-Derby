#######################
## Global Irradiance ##
#######################

#Full spectrum irradiance (W/m2) to PAR (W/m2) is 0.45 
  #Citations for this method are Gates, 1966; Jellison and Melack, 1993

#PAR (W/m2) to PAR (uE/s/m2) is 4.57
  #Citation for this method is McCree, 1972


#Load the packages
# install.packages("fishmethods") #For astrocalc4r 
#This function calculates
  #Solar zenith
  #Azimuth and declination angles
  #Time at sunrise
  #Local noon and sunset
  #Day length
  #PAR (photosynthetically available radiation, 400-700 nm)**
#Under clear skies and average atmospheric conditions (marine or continental) anywhere on the surface of the earth based on date, time, and location
#Enclose multiple observations in c()
library(fishmethods)
??fishmethods


#01 Jul 2013 through 31 Aug 2013
#2013 through 2018 #Determine number of days in each
numberOfDays <- function(date) {
  m <- format(date, format="%m")
  
  while (format(date, format="%m") == m) {
    date <- date + 1
  }
  
  return(as.integer(format(date - 1, format="%d")))
}

date <- as.Date("2014-08-01", "%Y-%m-%d")
numberOfDays(date) #But Jul and Aug always have 31 d ;)

grid <- expand.grid(hour=seq(1,24,0.25), day=1:31, month=c(5, 7, 8, 10), year=2013:2018, 
                    timezone=-8, lat=47.6292991, lon=-122.254822)# every 15 mins to match aquarium data
grid2 <- expand.grid(hour=seq(1,24,0.25), day=1:30, month=c(6, 9), year=2013:2018, 
                    timezone=-8, lat=47.6292991, lon=-122.254822)# every 15 mins to match aquarium data

??atrocalc4r
data_MJAO <- astrocalc4r(grid$day, grid$month, grid$year, grid$hour, grid$timezone, grid$lat, grid$lon, withinput = TRUE, #
            seaorland = "continental", acknowledgment = FALSE)
data_JS <- astrocalc4r(grid2$day, grid2$month, grid2$year, grid2$hour, grid2$timezone, grid2$lat, grid2$lon, withinput = TRUE, #
                    seaorland = "continental", acknowledgment = FALSE)

data$PAR #Currently in W/m2 #Need in particle flux of uE/s/m2
data_MJAO$PAR_particleFlux <- data_MJAO$PAR * 4.57
data_JS$PAR_particleFlux <- data_JS$PAR * 4.57

colnames(data) #Just to check...
length(data_JS$PAR) #33480
length(data_MJAO$PAR) #69192
length(data_JS$PAR_particleFlux) #33480
data$PAR #Obs 997 = 1.50165708e+02
data$PAR_particleFlux #Obs 997 = 6.86257287e+02

# calculate day length in seconds. Not necessary because daylight is already the time in hours and this creates much errors joining
# with the accumulation of differences in "dayLength" within the same day.
    # data$dayLength <- (data$sunset - data$sunrise) * 60 * 60
    # data$dayLength
    # colnames(data) #Just to check...
data_MJJASO <- union(data_MJAO, data_JS) 

PARcs_df <- data_MJJASO # name used in LakeWA_WQ_meteor.R file for now

saveRDS(PARcs_df,"Output/PAR_clear_sky.RDS")

#PAR ratio correction for smoky days
0.47-(0.035*(2.76*(2^-0.8))) #0.414517869 #Kobayashi et al., 2004





