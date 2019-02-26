#This script parses the London Fire Brigade KML file for all fire stations (current and past).
#The output consists of two data frames: all_stations (every station in the KML file) and current_stations (stations with Status=Open only).

#Check to see if required pacakges are installed, and if not, install them
if(!require(rgdal)){
  install.packages("rgdal")
  library(rgdal)
}

if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}

#Read in KML data
station_kml <- readOGR("London's fire stations (V2).kml", "All stations", encoding="KML")

#Parse the description field by splitting at the "<br>" tags
temp <- str_split(station_kml@data$Description,"<br>")

#Split parsed description field by element length so that the elements can be properly parsed
temp2 <- temp[lengths(temp)>9]
temp3 <- temp[lengths(temp)<=9]

#For current stations (element length 11 or 13), extract the values in each relevant field and save them to all_stations
for (i in seq_along(1:length(temp2))) {
  if (i==1) {all_stations <- str_replace(str_extract(temp2[[i]][3:10],":\\s(.+)$"),":\\s(.+)$","\\1")}
  else {all_stations <- rbind(all_stations,str_replace(str_extract(temp2[[i]][3:10],":\\s(.+)$"),":\\s(.+)$","\\1"))}
}

#For past stations (element length 9), extract the values in each relevant field and save them to all_stations
for (i in seq_along(1:length(temp3))) {
  all_stations <- rbind(all_stations,str_replace(str_extract(temp3[[i]][1:8],":\\s(.+)$"),":\\s(.+)$","\\1"))
}

#Set "all_stations" to a data frame and clean up column/row names
all_stations <- as.data.frame(all_stations)
colnames(all_stations) <- str_replace(str_extract(temp2[[1]][3:10],"^(.+):"),"^(.+):","\\1")
rownames(all_stations) <- seq(1:nrow(all_stations))

#Parse Latitude/Longitude from Location field
coordinates <- str_split(all_stations$Location,",")
all_stations$Latitude <- sapply(coordinates, "[", 1)
all_stations$Longitude <- sapply(coordinates, "[", 2)

#Create list of only currently open stations
#Note: Also remove "River Stn - Lambeth" as this station does not have fire engines (only boats for water rescues)
current_stations <- all_stations[which(all_stations$Status=="Open" & all_stations$'Station name'!="River Stn - Lambeth"),]

#Clean up
remove=c("station_kml","temp","temp2","temp3","coordinates","i","remove")
rm(list=remove)