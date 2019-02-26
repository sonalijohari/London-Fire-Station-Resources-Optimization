#This script loads the full incident file and then subsets it for the 7 boroughs in the Northern District.
#It also subsets the available fire stations.

#Check to see if required pacakges are installed, and if not, install them
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}
library(readxl)
library(lubridate)

source("Parse LFB KML.R")

incidents <- read_excel("LFB Incident data from Jan 2017.xlsx")

#Clean incident data
#Remove where DeployedfromStation is null or na
incidents_clean <- incidents[!is.na(incidents$FirstPumpArriving_DeployedFromStation),]
incidents_clean <- incidents_clean[incidents_clean$FirstPumpArriving_DeployedFromStation!='NULL',]

#Remove incidents without geographic info
incidents_clean <- incidents_clean[incidents_clean$IncGeo_BoroughCode!='E00000000',]

#Northern district only
northern_district <- toupper(c('Barnet','Camden','Enfield','Haringey','Islington','Westminster','City of London'))
incidents_filter <- incidents_clean %>% filter(IncGeo_BoroughName %in% northern_district) %>% filter(!is.na(Postcode_full))

#Filter fire stations by district above
keep_stations <- c('Dowgate','Euston','Holloway','Islington','Kentish Town','Paddington','Soho','West Hampstead')
stations <- current_stations %>% filter(`Station name` %in% keep_stations)