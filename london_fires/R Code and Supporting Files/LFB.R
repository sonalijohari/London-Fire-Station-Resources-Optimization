#This script solves a Fire Engine Mobilization Optimization Model for the London Fire Brigade (LFB).
#Model Details:
#Inputs:
# - Distance between incidents (fire or other emergency) and fire stations (pre-calculated from Google API)
# - Delay Factor between incidents and fire stations
# - Availability of fire engines/crews at each station (updated as fire engines are deployed and then return to home stations)
#Decision Variables:
# - Send fire engine/crew from a station to an incident (binary)
#Objective:
# - Minimize total effective distance for all deployed fire engines to all incidents
#Constraints:
# - Effective distance between incidents and fire stations (distance plus delay factor times distance)
# - One fire engine/crew is deployed to each incident
# - Total fire engines/crews deployed from each fire station does not exceed available engines/crews
#Model Assumptions:
# - All fire engines must be deployed from their home stations (no deployments directly from incident locations)
# - Delay factors range from 0 to 1

#Load the file with the KML parser
source("Parse LFB KML.R")
#Load the file with the "solve_ip" function
source("IP_Function.R")

#Fire Engine availability by station - this is the total available fire engines for each station (from LFB Fleet List)
#This availability vector is based on a subset of the fleet list, selecting active, operational, emergency response equipment of the type “pumping appliances” only.
starting_availability <- read.csv("availability2.csv", row.names=1)

#Enter time of iteration - will be used to track fire engine availability
timestamp <- strptime('2017-01-02 00:46', format = '%Y-%m-%d %H:%M', 'GMT')
#timestamp <- Sys.time()

#Distance Matrix for first five incidents
#Eventually, this will be read in from a spreadsheet or flat file (calculated from Google API)
distance <- read.csv("distance2_test.csv", row.names=1)
#colnames(distance) <- str_replace_all(colnames(distance), "[:punct:]", " ")

#Delay Factors for first five incidents
#Eventually this test file will be replaced with real delay factors (from Google API or historical data)
delay <- read.csv("delayfactor2_test.csv", row.names=1)

#Calculate effective distance as e = distance + delay * distance = (1+delay) * distance
effective_distance <- as.matrix((1+delay)*distance)

#Determine current availability of fire engines by station
#This vector will be the RHS row constraints for the IP model

#Get timestamps from last_availability vectors for all previous runs
previous_timestamps <- strptime(str_extract(list.files(".",pattern="last_availability_."),"\\d.+\\."), format="%Y%m%d_%H%M%S.", "GMT")

#Assume that all fire engines have returned to their home stations if there is a difference of more than 30 minutes between runs
if (length(previous_timestamps)==0) {
  current_availability <- starting_availability
} else if (difftime(timestamp,max(previous_timestamps),units="mins")>30) {
  current_availability <- starting_availability  
} else {
  current_availability <- read.csv(paste("last_availability_",format(max(previous_timestamps),"%Y%m%d_%H%M%S"),".csv",sep=""), row.names=1)
}

deployment_solution <- solve_ip(effective_distance,current_availability$Engines)

#Add station names to deployment_solution binary (0/1) matrix
rownames(deployment_solution$solution) <- current_stations$'Station name'
deployment_solution$solution

#Update availability matrix
last_availability <- current_availability - apply(deployment_solution$solution,1,sum)

#Write current state of availability out to csv file (for use in the next iteration)
write.csv(last_availability,file=paste("last_availability_",format(timestamp,"%Y%m%d_%H%M%S"),".csv",sep=""))