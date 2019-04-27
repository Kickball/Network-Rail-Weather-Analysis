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

#Variables used to determine whether it is an initial run of the code on fresh data or if parts of the code has already been run on the code.

initialI <- 0
initialW <- 1

# i <- 0
# googleAPIKey = "key="
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

#https://stackoverflow.com/questions/13706188/importing-csv-file-into-r-numeric-values-read-as-characters

#nrWeatherIncident <- read.csv("", stringsAsFactors = FALSE, na.strings = c("", "#N/A", "NA", "N/A"))
nrWeatherIncident <- read.csv("", stringsAsFactors = FALSE, na.strings = c("", "#N/A", "NA", "N/A"))
weatherStationDetails <- read.csv("")
rain <- read.csv("")
soil <- read.csv("")
weather <- read.csv("")
wind <- read.csv("")
#postCodes <- read.csv("")

#--------------------------------------------------------
#---------------- TRIM DATASET COLUMNS ------------------
#--------------------------------------------------------

if (initialI > 0) {
  nrWeatherIncident <- subset(nrWeatherIncident, select = -c(Yr.Period))
}

#https://stackoverflow.com/questions/55883598/r-grep-include-na-values-in-results

if (initialW > 0) {
  rain <- rain[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", rain$PRCP_AMT_Q)|is.na(rain$PRCP_AMT_Q),]
  rain <- subset(rain, select = -c(ID, ID_TYPE, OB_HOUR_COUNT, VERSION_NUM, MET_DOMAIN_NAME, REC_ST_IND, PRCP_DUR, PRCP_AMT_Q, PRCP_DUR_Q, METO_STMP_TIME, MIDAS_STMP_ETIME, PRCP_AMT_J, X))
  soil <- soil[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", soil$Q5CM_SOIL_TEMP_Q)|is.na(soil$Q5CM_SOIL_TEMP_Q),]
  soil <- soil[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", soil$Q10CM_SOIL_TEMP_Q)|is.na(soil$Q10CM_SOIL_TEMP_Q),]
  soil <- soil[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", soil$Q20CM_SOIL_TEMP_Q)|is.na(soil$Q20CM_SOIL_TEMP_Q),]
  soil <- soil[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", soil$Q30CM_SOIL_TEMP_Q)|is.na(soil$Q30CM_SOIL_TEMP_Q),]
  soil <- soil[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", soil$Q50CM_SOIL_TEMP_Q)|is.na(soil$Q50CM_SOIL_TEMP_Q),]
  soil <- soil[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", soil$Q100CM_SOIL_TEMP_Q)|is.na(soil$Q100CM_SOIL_TEMP_Q),]
  soil <- subset(soil, select = -c(ID, ID_TYPE, REC_ST_IND, MET_DOMAIN_NAME, VERSION_NUM, METO_STMP_TIME, MIDAS_STMP_ETIME, Q5CM_SOIL_TEMP_Q, Q10CM_SOIL_TEMP_Q, Q20CM_SOIL_TEMP_Q, Q30CM_SOIL_TEMP_Q, Q50CM_SOIL_TEMP_Q, Q100CM_SOIL_TEMP_Q, Q5CM_SOIL_TEMP_J, Q10CM_SOIL_TEMP_J, Q20CM_SOIL_TEMP_J, Q30CM_SOIL_TEMP_J, Q50CM_SOIL_TEMP_J, Q100CM_SOIL_TEMP_J, X))
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$WIND_DIRECTION_Q)|is.na(weather$WIND_DIRECTION_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$WIND_SPEED_Q)|is.na(weather$WIND_SPEED_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$CLD_TTL_AMT_ID_Q)|is.na(weather$CLD_TTL_AMT_ID_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$LOW_CLD_TYPE_ID_Q)|is.na(weather$LOW_CLD_TYPE_ID_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$MED_CLD_TYPE_ID_Q)|is.na(weather$MED_CLD_TYPE_ID_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$HI_CLD_TYPE_ID_Q)|is.na(weather$HI_CLD_TYPE_ID_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$CLD_BASE_AMT_ID_Q)|is.na(weather$CLD_BASE_AMT_ID_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$CLD_BASE_HT_Q)|is.na(weather$CLD_BASE_HT_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$VISIBILITY_Q)|is.na(weather$VISIBILITY_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$MSL_PRESSURE_Q)|is.na(weather$MSL_PRESSURE_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$AIR_TEMPERATURE_Q)|is.na(weather$AIR_TEMPERATURE_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$DEWPOINT_Q)|is.na(weather$DEWPOINT_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$WETB_TEMP_Q)|is.na(weather$WETB_TEMP_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$STN_PRES_Q)|is.na(weather$STN_PRES_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$ALT_PRES_Q)|is.na(weather$ALT_PRES_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$GROUND_STATE_ID_Q)|is.na(weather$GROUND_STATE_ID_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$Q10MNT_MXGST_SPD_Q)|is.na(weather$Q10MNT_MXGST_SPD_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$WMO_HR_SUN_DUR_Q)|is.na(weather$WMO_HR_SUN_DUR_Q),]
  weather <- weather[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", weather$SNOW_DEPTH_Q)|is.na(weather$SNOW_DEPTH_Q),]
  weather <- subset(weather, select = -c(ID, ID_TYPE, MET_DOMAIN_NAME, VERSION_NUM, REC_ST_IND, WIND_SPEED_UNIT_ID, PRST_WX_ID, PAST_WX_ID_1, PAST_WX_ID_2, CLD_AMT_ID_1, CLOUD_TYPE_ID_1, CLD_BASE_HT_ID_1, CLD_AMT_ID_2, CLOUD_TYPE_ID_2, CLD_BASE_HT_ID_2, CLD_AMT_ID_3, CLOUD_TYPE_ID_3, CLD_BASE_HT_ID_3, CLD_AMT_ID_4, CLOUD_TYPE_ID_4, CLD_BASE_HT_ID_4, VERT_VSBY, CAVOK_FLAG, CS_HR_SUN_DUR, WIND_DIRECTION_Q, WIND_SPEED_Q, PRST_WX_ID_Q, PAST_WX_ID_1_Q, PAST_WX_ID_2_Q, CLD_TTL_AMT_ID_Q, LOW_CLD_TYPE_ID_Q, MED_CLD_TYPE_ID_Q, HI_CLD_TYPE_ID_Q, CLD_BASE_AMT_ID_Q, CLD_BASE_HT_Q, VISIBILITY_Q, MSL_PRESSURE_Q, AIR_TEMPERATURE_Q, DEWPOINT_Q, WETB_TEMP_Q, GROUND_STATE_ID_Q, CLD_AMT_ID_1_Q, CLOUD_TYPE_ID_1_Q, CLD_BASE_HT_ID_1_Q, CLD_AMT_ID_2_Q, CLOUD_TYPE_ID_2_Q, CLD_BASE_HT_ID_2_Q, CLD_AMT_ID_3_Q, CLOUD_TYPE_ID_3_Q, CLD_BASE_HT_ID_3_Q, CLD_AMT_ID_4_Q, CLOUD_TYPE_ID_4_Q, CLD_BASE_HT_ID_4_Q, VERT_VSBY_Q, STN_PRES_Q, ALT_PRES_Q, Q10MNT_MXGST_SPD_Q, CS_HR_SUN_DUR_Q, WMO_HR_SUN_DUR_Q, METO_STMP_TIME, MIDAS_STMP_ETIME, WIND_DIRECTION_J, WIND_SPEED_J, PRST_WX_ID_J, PAST_WX_ID_1_J, PAST_WX_ID_2_J, CLD_AMT_ID_J, CLD_HT_J, VISIBILITY_J, MSL_PRESSURE_J, AIR_TEMPERATURE_J, DEWPOINT_J, WETB_TEMP_J, VERT_VSBY_J, STN_PRES_J, ALT_PRES_J, Q10MNT_MXGST_SPD_J, RLTV_HUM_J, SNOW_DEPTH_Q, DRV_HR_SUN_DUR, DRV_HR_SUN_DUR_Q, X))
  wind <- wind[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", wind$MEAN_WIND_DIR_Q)|is.na(wind$MEAN_WIND_DIR_Q),]
  wind <- wind[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", wind$MEAN_WIND_SPEED_Q)|is.na(wind$MEAN_WIND_SPEED_Q),]
  wind <- wind[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", wind$MAX_GUST_DIR_Q)|is.na(wind$MAX_GUST_DIR_Q),]
  wind <- wind[grepl("(\\b\\d{1}\\b)|([0-9]{1}[0]{3}[0-9]{1})", wind$MAX_GUST_SPEED_Q)|is.na(wind$MAX_GUST_SPEED_Q),]
  wind <- subset(wind, select = -c(ID_TYPE, ID, OB_HOUR_COUNT, MET_DOMAIN_NAME, VERSION_NUM, REC_ST_IND, MAX_GUST_CTIME, MEAN_WIND_DIR_Q, MEAN_WIND_SPEED_Q, MAX_GUST_DIR_Q, MAX_GUST_SPEED_Q, MAX_GUST_CTIME_Q, METO_STMP_TIME, MIDAS_STMP_ETIME, MEAN_WIND_DIR_J, MEAN_WIND_SPEED_J, MAX_GUST_DIR_J, MAX_GUST_SPEED_J))
}

#--------------------------------------------------------
#----------------- TRIM DATASET ROWS --------------------
#--------------------------------------------------------

#https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame

if (initialI > 0) {
  #remove rows without a postcode for the first location
  nrWeatherIncident <- nrWeatherIncident[complete.cases(nrWeatherIncident[,8]),]
  #remove rows with a location for the second location but not a postcode
  nrWeatherIncident <- nrWeatherIncident[!(complete.cases(nrWeatherIncident[,9]) & !complete.cases(nrWeatherIncident[,10])),]
}

#--------------------------------------------------------
#---------------- ADD LOCATION COLUMNS ------------------
#--------------------------------------------------------

#https://stackoverflow.com/questions/18214395/add-empty-columns-to-a-dataframe-with-specified-names-from-a-vector

gpsColumns <- c("HIGH_PRCN_LAT", "HIGH_PRCN_LON")


if (initialI > 0) {
  nrWeatherIncident[ , gpsColumns] <- NA
}
  

if (initialW > 0) {
  rain[ , gpsColumns] <- NA
  soil[ , gpsColumns] <- NA
  weather[ , gpsColumns] <- NA
  wind[ , gpsColumns] <- NA
}

remove(gpsColumns)

#--------------------------------------------------------
#-------------- POPULATE LOCATION COLUMNS ---------------
#--------------------------------------------------------

#https://stackoverflow.com/questions/55713509/r-conditional-updating-coordinate-column-in-dataframe

if (initialI > 0) {
  #create separate variable(?) containing a list of which rows are complete
  ind <- complete.cases(nrWeatherIncident[,17])
  
  #populate rows with a two Lat/Lons with great circle middle of both values
  nrWeatherIncident[ind, c("HIGH_PRCN_LON","HIGH_PRCN_LAT")] <- 
    with(nrWeatherIncident[ind,,drop=FALSE],
         geosphere::midPoint(cbind.data.frame(stanoxSplit1PostCodeLon, stanoxSplit1PostCodeLat),
                                            cbind.data.frame(stanoxSplit2PostCodeLon, stanoxSplit2PostCodeLat)
                                            )
    )
  
  #populate rows with one Lat/Lon with those values
  nrWeatherIncident[!ind, c("HIGH_PRCN_LAT","HIGH_PRCN_LON")] <- nrWeatherIncident[!ind, c("stanoxSplit1PostCodeLat","stanoxSplit1PostCodeLon")]
  
  remove(ind)
}
  
#https://stackoverflow.com/questions/21712384/updating-column-in-one-dataframe-with-value-from-another-dataframe-based-on-matc
#This section is no longer needed as the rail incidents and weather will be matched based on station ID, however this code still services to identify weather records without a corresponsding weather station and its metadata.

if (initialW > 0) {
  rain$HIGH_PRCN_LAT<-weatherStationDetails[match(rain$SRC_ID, weatherStationDetails$SRC_ID),4]
  rain$HIGH_PRCN_LON<-weatherStationDetails[match(rain$SRC_ID, weatherStationDetails$SRC_ID),5]
  
  soil$HIGH_PRCN_LAT<-weatherStationDetails[match(soil$SRC_ID, weatherStationDetails$SRC_ID),4]
  soil$HIGH_PRCN_LON<-weatherStationDetails[match(soil$SRC_ID, weatherStationDetails$SRC_ID),5]
  
  weather$HIGH_PRCN_LAT<-weatherStationDetails[match(weather$SRC_ID, weatherStationDetails$SRC_ID),4]
  weather$HIGH_PRCN_LON<-weatherStationDetails[match(weather$SRC_ID, weatherStationDetails$SRC_ID),5]
  
  wind$HIGH_PRCN_LAT<-weatherStationDetails[match(wind$SRC_ID, weatherStationDetails$SRC_ID),4]
  wind$HIGH_PRCN_LON<-weatherStationDetails[match(wind$SRC_ID, weatherStationDetails$SRC_ID),5]
}

#--------------------------------------------------------
#--------- DELETE WEATHER DATA WITHOUT LOCATION  --------
#--------------------------------------------------------

#https://stackoverflow.com/questions/4862178/remove-rows-with-all-or-some-nas-missing-values-in-data-frame
if (initialW > 0) {
  rain <- rain[complete.cases(rain[,4:5]),]
  soil <- soil[complete.cases(soil[,9:10]),]
  weather <- weather[complete.cases(weather[,24:25]),]
  wind <- wind[complete.cases(wind[,7:8]),]
}
  
#--------------------------------------------------------
#----------------- ADD STATION ID COLUMN ----------------
#--------------------------------------------------------

#https://stackoverflow.com/questions/18214395/add-empty-columns-to-a-dataframe-with-specified-names-from-a-vector
if (initialI > 0) {
  stationColumns <- c("SRC_ID", "distance")
  nrWeatherIncident[ , stationColumns] <- NA
  remove(stationColumns)
}

#--------------------------------------------------------
#------------- POPULATE STATION ID COLUMN ---------------
#--------------------------------------------------------


if (initialI > 0) {
  
  #https://stackoverflow.com/questions/55752064/r-finding-closest-coordinates-between-two-large-data-sets
  #The code that is commented out is old code that calculated distance between weather stations and rail incidents in a Euclidean way, which is less accurate but faster. A more accurate way is left uncommented.
  
  #nrWeatherIncident[ , c(20,21)] <- as.data.frame(RANN::nn2(weatherStationDetails[,c(4,5)],nrWeatherIncident[,c(18,19)],k=1))
  
  #nrWeatherIncident[,20] <- weatherStationDetails[nrWeatherIncident[,21], 1]
  
  #https://stackoverflow.com/questions/55754660/finding-closest-points-between-multiple-datasets-using-ellipsoidal-vincenty/
  
  nrWeatherIncident[,c(20,21)] <- t(
    apply(
      apply(nrWeatherIncident[,c(18,19)], 1, function(mrow){distVincentyEllipsoid(mrow, weatherStationDetails[,c(4,5)])}),
      2, function(x){ c(SRC_ID=weatherStationDetails[which.min(x),1],distance=min(x))}
    )
  )

}
  
remove(weatherStationDetails)

#https://www.rforexcelusers.com/vlookup-in-r/ - for merging stuff

#--------------------------------------------------------
#----------- REMOVE DISTANCES TOO FAR AWAY --------------
#--------------------------------------------------------

#https://stackoverflow.com/questions/8005154/conditionally-remove-dataframe-rows-with-r

#nrWeatherIncident<-nrWeatherIncident[(nrWeatherIncident$distance<"0.04"),]

#--------------------------------------------------------
#------------------ ADD WEATHER COLUMNs -----------------
#--------------------------------------------------------

#https://stackoverflow.com/questions/18214395/add-empty-columns-to-a-dataframe-with-specified-names-from-a-vector

weatherColumns <- c("PRCP_AMT", "Q5CM_SOIL_TEMP", "Q10CM_SOIL_TEMP", "Q20CM_SOIL_TEMP", "Q30CM_SOIL_TEMP", "Q50CM_SOIL_TEMP", "Q100CM_SOIL_TEMP", "SRC_OPR_TYPE", "WIND_DIRECTION", "WIND_SPEED", "CLD_TTL_AMT_ID", "LOW_CLD_TYPE_ID", "MED_CLD_TYPE_ID", "HI_CLD_TYPE_ID", "CLD_BASE_AMT_ID", "CLD_BASE_HT", "VISIBILITY", "MSL_PRESSURE", "AIR_TEMPERATURE", "DEWPOINT", "WETB_TEMP", "STN_PRES", "ALT_PRES", "GROUND_STATE_ID", "Q10MNT_MXGST_SPD", "WMO_HR_SUN_DUR", "RLTV_HUM", "SNOW_DEPTH", "MEAN_WIND_DIR", "MEAN_WIND_SPEED", "MAX_GUST_DIR", "MAX_GUST_SPEED")
nrWeatherIncident[ , weatherColumns] <- NA
remove(weatherColumns)

#--------------------------------------------------------
#-------------- POPULATE WEATHER COLUMNS ----------------
#--------------------------------------------------------




#--------------------------------------------------------
#----------- CONVERT POSTCODES TO COORDINATES -----------
#--------------------------------------------------------

# #https://allthingsr.blogspot.com/2012/01/geocode-your-data-using-r-json-and.html
# 
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
# 
# #https://stackoverflow.com/questions/13442461/populating-a-data-frame-in-r-in-a-loop
# #https://stackoverflow.com/questions/39556911/r-how-to-increment-the-incrementing-variable-within-a-for-loop
# #http://www.endmemo.com/program/R/rbind.php
# 
# for (postcode in postCodes$Postcode){
#   i <- i + 1
#   coordinates <- getGeoCode(postcode, googleAPIKey)
#   #print(coordinates)
#   postcodeLocation <- data.frame(postcode, coordinates["Lat"], coordinates["Lng"])
#   rownames(postcodeLocation) <- c(i)
#   #print(postcodeLocation)
#   colnames(postcodeLocation) <- c("postcode", "HIGH_PRCN_LAT", "HIGH_PRCN_LON")
#   stationLocations <- rbind(stationLocations, postcodeLocation)
# }
# 
# #https://stackoverflow.com/questions/15956183/how-to-save-a-data-frame-as-csv-to-a-user-selected-location-using-tcltk
# 
# write.csv(file="Location Reference Data.csv", x=stationLocations)
