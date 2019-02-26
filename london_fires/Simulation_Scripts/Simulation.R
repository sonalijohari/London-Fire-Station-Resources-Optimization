#This script creates the simulation set to be used with the LFB.R script to simulate real-time emergency calls to the London Fire Brigade.

#Check to see if required pacakges are installed, and if not, install them
if(!require(countreg)){
  install.packages("countreg")
  library(countreg)
}

source("Load_Incidents.R")

set.seed(123)

#Select number of days to simulate
simulation_days <- 100

time_periods <- simulation_days*96 #24 hours times 4 15-minute periods per hour

lambda <- 2.7 #This is the average frequency of calls every 15 minutes for all fire stations in the system

simulation_set <- data.frame(Period_ID=seq(time_periods),Period=seq(ISOdate(2018,1,1), by ="15 mins", length.out=time_periods),
                             Calls=rztpois(time_periods,lambda))

for (i in 1:nrow(simulation_set)) {
  print(i)
  if (i==1) {
  samples <- data.frame(cbind(Period_ID=i,IncidentNumber=sample(incidents_filter$IncidentNumber,simulation_set$Calls[i],replace=FALSE)))
  } else {
    samples <- rbind(samples,
                     cbind(Period_ID=i,IncidentNumber=sample(incidents_filter$IncidentNumber,simulation_set$Calls[i],replace=FALSE)))
  }
}

samples$Period_ID <- as.integer(samples$Period_ID)
simulation_set <- inner_join(simulation_set,samples) #Can delete "Calls" column as it is now redundant
simulation_set$IncidentNumber <- as.character(simulation_set$IncidentNumber)

#Clean up
clean <- c('simulation_days','samples','clean')
rm(clean)