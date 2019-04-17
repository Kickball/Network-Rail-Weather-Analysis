#--------------------------------------------------------
#-------------------- LOAD LIBRARIES --------------------
#--------------------------------------------------------

library("XML")
library("RgoogleMaps")
library("RJSONIO")
library("RANN")
library("geosphere")

#--------------------------------------------------------
#----------------- INITIALISE VARIABLES -----------------
#--------------------------------------------------------

# i <- 0
# googleAPIKey = "key=xxxx"
# 
# #https://stackoverflow.com/questions/13442461/populating-a-data-frame-in-r-in-a-loop
# #https://stats.stackexchange.com/questions/5253/how-do-i-get-the-number-of-rows-of-a-data-frame-in-r
# #https://www.dummies.com/programming/r/how-to-name-matrix-rows-and-columns-in-r/
# 
# stationLocations <- matrix(ncol=3, nrow=0)
# colnames(stationLocations) <- c("postcode", "HIGH_PRCN_LAT", "HIGH_PRCN_LON")

#--------------------------------------------------------
#-------------------- LOAD DATASETS ---------------------
#--------------------------------------------------------

weatherStationDetails <- read.csv("C:\\Users\\Edward\\OneDrive\\Documents\\University\\4th Year\\Dissertation (BMAN31260)\\Data\\Weather Station\\Station Details.csv")
nrWeatherIncident <- read.csv("C:\\Users\\Edward\\OneDrive\\Documents\\University\\4th Year\\Dissertation (BMAN31260)\\Data\\Network Rail Incidents\\National Rail - Weather Incident Export.csv")
rain <- read.csv("C:\\Users\\Edward\\OneDrive\\Documents\\University\\4th Year\\Dissertation (BMAN31260)\\Data\\Rain\\midas_rainhrly_200601-200612.txt")
soil <- read.csv("C:\\Users\\Edward\\OneDrive\\Documents\\University\\4th Year\\Dissertation (BMAN31260)\\Data\\Soil\\midas_soiltemp_200601-200612.txt")
weather <- read.csv("C:\\Users\\Edward\\OneDrive\\Documents\\University\\4th Year\\Dissertation (BMAN31260)\\Data\\Weather (H)\\midas_wxhrly_200601-200612.txt")
wind <- read.csv("C:\\Users\\Edward\\OneDrive\\Documents\\University\\4th Year\\Dissertation (BMAN31260)\\Data\\Wind\\midas_wind_200601-200612.txt")
#postCodes <- read.csv("C:\\Users\\Edward\\OneDrive\\Documents\\University\\4th Year\\Dissertation (BMAN31260)\\Data\\Network Rail Incidents\\Postcodes.csv")

#--------------------------------------------------------
#-------------------- TRIM DATASETS ---------------------
#--------------------------------------------------------

rain <- subset(rain, select = -c(ID, ID_TYPE, OB_HOUR_COUNT, VERSION_NUM, MET_DOMAIN_NAME, REC_ST_IND, PRCP_DUR, PRCP_AMT_Q, PRCP_DUR_Q, METO_STMP_TIME, MIDAS_STMP_ETIME, PRCP_AMT_J, X))
soil <- subset(soil, select = -c(ID, ID_TYPE, REC_ST_IND, MET_DOMAIN_NAME, VERSION_NUM, METO_STMP_TIME, MIDAS_STMP_ETIME, Q5CM_SOIL_TEMP_Q, Q10CM_SOIL_TEMP_Q, Q20CM_SOIL_TEMP_Q, Q30CM_SOIL_TEMP_Q, Q50CM_SOIL_TEMP_Q, Q100CM_SOIL_TEMP_Q, Q5CM_SOIL_TEMP_J, Q10CM_SOIL_TEMP_J, Q20CM_SOIL_TEMP_J, Q30CM_SOIL_TEMP_J, Q50CM_SOIL_TEMP_J, Q100CM_SOIL_TEMP_J, X))
weather <- subset(weather, select = -c(ID, ID_TYPE, MET_DOMAIN_NAME, VERSION_NUM, REC_ST_IND, WIND_SPEED_UNIT_ID, PRST_WX_ID, PAST_WX_ID_1, PAST_WX_ID_2, CLD_AMT_ID_1, CLOUD_TYPE_ID_1, CLD_BASE_HT_ID_1, CLD_AMT_ID_2, CLOUD_TYPE_ID_2, CLD_BASE_HT_ID_2, CLD_AMT_ID_3, CLOUD_TYPE_ID_3, CLD_BASE_HT_ID_3, CLD_AMT_ID_4, CLOUD_TYPE_ID_4, CLD_BASE_HT_ID_4, GROUND_STATE_ID, CAVOK_FLAG, CS_HR_SUN_DUR, WIND_DIRECTION_Q, WIND_SPEED_Q, PRST_WX_ID_Q, PAST_WX_ID_1_Q, PAST_WX_ID_2_Q, CLD_TTL_AMT_ID_Q, LOW_CLD_TYPE_ID_Q, MED_CLD_TYPE_ID_Q, HI_CLD_TYPE_ID_Q, CLD_BASE_AMT_ID_Q, CLD_BASE_HT_Q, VISIBILITY_Q, MSL_PRESSURE_Q, AIR_TEMPERATURE_Q, DEWPOINT_Q, WETB_TEMP_Q, GROUND_STATE_ID_Q, CLD_AMT_ID_1_Q, CLOUD_TYPE_ID_1_Q, CLD_BASE_HT_ID_1_Q, CLD_AMT_ID_2_Q, CLOUD_TYPE_ID_2_Q, CLD_BASE_HT_ID_2_Q, CLD_AMT_ID_3_Q, CLOUD_TYPE_ID_3_Q, CLD_BASE_HT_ID_3_Q, CLD_AMT_ID_4_Q, CLOUD_TYPE_ID_4_Q, CLD_BASE_HT_ID_4_Q, VERT_VSBY_Q, STN_PRES_Q, ALT_PRES_Q, Q10MNT_MXGST_SPD_Q, CS_HR_SUN_DUR_Q, WMO_HR_SUN_DUR_Q, METO_STMP_TIME, MIDAS_STMP_ETIME, WIND_DIRECTION_J, WIND_SPEED_J, PRST_WX_ID_J, PAST_WX_ID_1_J, PAST_WX_ID_2_J, CLD_AMT_ID_J, CLD_HT_J, VISIBILITY_J, MSL_PRESSURE_J, AIR_TEMPERATURE_J, DEWPOINT_J, WETB_TEMP_J, VERT_VSBY_J, STN_PRES_J, ALT_PRES_J, Q10MNT_MXGST_SPD_J, RLTV_HUM_J, SNOW_DEPTH_Q, DRV_HR_SUN_DUR, DRV_HR_SUN_DUR_Q, X))
wind <- subset(wind, select = -c(ID_TYPE, ID, OB_HOUR_COUNT, MET_DOMAIN_NAME, VERSION_NUM, REC_ST_IND, MAX_GUST_CTIME, MEAN_WIND_DIR_Q, MEAN_WIND_SPEED_Q, MAX_GUST_DIR_Q, MAX_GUST_SPEED_Q, MAX_GUST_CTIME_Q, METO_STMP_TIME, MIDAS_STMP_ETIME, MEAN_WIND_DIR_J, MEAN_WIND_SPEED_J, MAX_GUST_DIR_J, MAX_GUST_SPEED_J))

#--------------------------------------------------------
#---------------- ADD LOCATION COLUMNS ------------------
#--------------------------------------------------------

#https://stackoverflow.com/questions/18214395/add-empty-columns-to-a-dataframe-with-specified-names-from-a-vector

gpsColumns <- c("HIGH_PRCN_LAT", "HIGH_PRCN_LON")
nrWeatherIncident[ , gpsColumns] <- NA
rain[ , gpsColumns] <- NA
soil[ , gpsColumns] <- NA
weather[ , gpsColumns] <- NA
wind[ , gpsColumns] <- NA

#--------------------------------------------------------
#-------------- POPULATE LOCATION COLUMNS ---------------
#--------------------------------------------------------

#https://stackoverflow.com/questions/55713509/r-conditional-updating-coordinate-column-in-dataframe

#create separate variable(?) containing a list of which rows are complete
ind <- complete.cases(nrWeatherIncident[,17])

#populate rows with a two Lat/Lons with great circle middle of both values
nrWeatherIncident[ind, c("HIGH_PRCN_LON","HIGH_PRCN_LAT")] <- 
  with(nrWeatherIncident[ind,,drop=FALSE],
       do.call(rbind, geosphere::gcIntermediate(cbind.data.frame(stanoxSplit1PostCodeLon, stanoxSplit1PostCodeLat),
                                                cbind.data.frame(stanoxSplit2PostCodeLon, stanoxSplit2PostCodeLat), n = 1)))

#populate rows with one Lat/Lon with those values
nrWeatherIncident[!ind, c("HIGH_PRCN_LAT","HIGH_PRCN_LON")] <- nrWeatherIncident[!ind, c("stanoxSplit1PostCodeLat","stanoxSplit1PostCodeLon")]

#https://stackoverflow.com/questions/21712384/updating-column-in-one-dataframe-with-value-from-another-dataframe-based-on-matc

rain$HIGH_PRCN_LAT<-weatherStationDetails[match(rain$SRC_ID, weatherStationDetails$SRC_ID),4]
rain$HIGH_PRCN_LON<-weatherStationDetails[match(rain$SRC_ID, weatherStationDetails$SRC_ID),5]

soil$HIGH_PRCN_LAT<-weatherStationDetails[match(soil$SRC_ID, weatherStationDetails$SRC_ID),4]
soil$HIGH_PRCN_LON<-weatherStationDetails[match(soil$SRC_ID, weatherStationDetails$SRC_ID),5]

weather$HIGH_PRCN_LAT<-weatherStationDetails[match(weather$SRC_ID, weatherStationDetails$SRC_ID),4]
weather$HIGH_PRCN_LON<-weatherStationDetails[match(weather$SRC_ID, weatherStationDetails$SRC_ID),5]

wind$HIGH_PRCN_LAT<-weatherStationDetails[match(wind$SRC_ID, weatherStationDetails$SRC_ID),4]
wind$HIGH_PRCN_LON<-weatherStationDetails[match(wind$SRC_ID, weatherStationDetails$SRC_ID),5]

#--------------------------------------------------------
#--------- DELETE WEATHER DATA WITHOUT LOCATION  --------
#--------------------------------------------------------

#https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame

rain <- rain[complete.cases(rain[,4:5]),]
soil <- soil[complete.cases(soil[,9:10]),]
weather <- weather[complete.cases(weather[,24:25]),]
wind <- wind[complete.cases(wind[,7:8]),]

#--------------------------------------------------------
#----------------- ADD STATION ID COLUMN ----------------
#--------------------------------------------------------

#https://stackoverflow.com/questions/18214395/add-empty-columns-to-a-dataframe-with-specified-names-from-a-vector

stationColumn <- c("SRC_ID")
nrWeatherIncident[ , stationColumn] <- NA

#--------------------------------------------------------
#------------- POPULATE STATION ID COLUMN ---------------
#--------------------------------------------------------

#https://stackoverflow.com/questions/32618956/find-the-nearest-x-y-coordinate-using-r

nrWeatherIncident$stationColumn<-weatherStationDetails[,1]

#https://www.rforexcelusers.com/vlookup-in-r/ - for merging stuff


#--------------------------------------------------------
#----------- CONVERT POSTCODES TO COORDINATES -----------
#--------------------------------------------------------

#https://allthingsr.blogspot.com/2012/01/geocode-your-data-using-r-json-and.html

# getGeoCode <- function(gcStr, googleAPIKey)
# {
#   gcStr <- gsub(' ','%20',gcStr) #Encode URL Parameters, by replacing spaces with %20
#   #Open Connection
#   #print(gcStr)
#   #https://stackoverflow.com/questions/7201341/how-can-two-strings-be-concatenated
#   gcStr <- paste(gcStr, googleAPIKey, sep="&")
#   #print(gcStr)
#   connectStr <- paste('https://maps.google.com/maps/api/geocode/json?sensor=false&address=',gcStr, sep="")
#   #print(connectStr)
#   con <- url(connectStr)
#   data.json <- fromJSON(paste(readLines(con), collapse=""))
#   close(con)
#   #print(data.json)
#   #Flatten the received JSON
#   data.json <- unlist(data.json)
#   lat <- data.json["results.geometry.location.lat"]
#   lng <- data.json["results.geometry.location.lng"]
#   gcodes <- c(lat, lng)
#   names(gcodes) <- c("Lat", "Lng")
#   return (gcodes)
# }

#https://stackoverflow.com/questions/13442461/populating-a-data-frame-in-r-in-a-loop
#https://stackoverflow.com/questions/39556911/r-how-to-increment-the-incrementing-variable-within-a-for-loop
#http://www.endmemo.com/program/R/rbind.php

# for (postcode in postCodes$Postcodes){
#   i <- i + 1
#   coordinates <- getGeoCode(postcode, googleAPIKey)
#   #print(coordinates)
#   postcodeLocation <- data.frame(postcode, coordinates["Lat"], coordinates["Lng"])
#   rownames(postcodeLocation) <- c(i)
#   #print(postcodeLocation)
#   colnames(postcodeLocation) <- c("postcode", "HIGH_PRCN_LAT", "HIGH_PRCN_LON")
#   stationLocations <- rbind(stationLocations, postcodeLocation)
# }

#https://stackoverflow.com/questions/15956183/how-to-save-a-data-frame-as-csv-to-a-user-selected-location-using-tcltk

#write.csv(file="Location Reference Data.csv", x=stationLocations)
