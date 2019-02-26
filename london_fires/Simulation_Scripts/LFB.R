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
#source("Parse LFB KML.R")
#Load the file with the "solve_ip" function
source("IP_Function.R")
#Load the file that creates the simulation data set
source("Simulation.R")

#Fire Engine availability by station - this is the total available fire engines for each station (from LFB Fleet List)
#This availability vector is based on a subset of the fleet list, selecting active, operational, emergency response equipment of the type “pumping appliances” only.
starting_availability <- read.csv("temp_stations.csv")
colnames(starting_availability) <- str_replace(colnames(starting_availability),'\\.',' ')

#Sensitivity Analysis
#starting_availability <- starting_availability + 2 #Add additional engines to each fire station

#Initialize data frame for results output
simulation_results <- simulation_set %>% select(Period_ID,IncidentNumber) %>%
                                 mutate(
                                 Dowgate=0,
                                 Euston=0,
                                 Holloway=0,
                                 Islington=0,
                                 'Kentish Town'=0,
                                 Paddington=0,
                                 Soho=0,
                                 'West Hampstead'=0)


simulation_summary <- data.frame(Period_ID=1:time_periods,
                                 Dowgate=integer(time_periods),
                                 Euston=integer(time_periods),
                                 Holloway=integer(time_periods),
                                 Islington=integer(time_periods),
                                 'Kentish Town'=integer(time_periods),
                                 Paddington=integer(time_periods),
                                 Soho=integer(time_periods),
                                 'West Hampstead'=integer(time_periods),
                                 Total_Distance=numeric(time_periods))

#Distance Matrix for simulation
#Eventually, this will be read in from a spreadsheet or flat file (calculated from Google API)
distances <- read_csv("distances.csv")

set.seed(123)
#Run simulation
for (i in 1:time_periods) {
  print(paste('Iteration',i,'of',time_periods,sep=' '))
  distance <- inner_join(simulation_set[simulation_set$Period_ID==i,],distances) #distances pre-calculated for all incidents/fire stations
  rownames(distance) <- distance$IncidentNumber
  rows_to_remove <- c('Period_ID','Period','Calls','IncidentNumber')
  distance <- distance %>% select(-rows_to_remove)

  #Delay Factors for incidents for each fire station
  #Randomly generate value between 0 and 1 to simulate traffic and other delays
  delay <- data.frame(distance)
  delay[,1:ncol(delay)] <- sample(1:100,ncol(delay),replace=TRUE)/100
  
  #This line checks to ensure that the delay factor is the same every time the simulation runs
  #if (i==1) {keep_delay <- delay} else {keep_delay <- rbind(keep_delay,delay)} 

  #Calculate effective distance as e = distance + delay * distance = (1+delay) * distance
  effective_distance <- as.matrix((1+delay)*distance)

  #Determine current availability of fire engines by station
  #This vector will be the RHS row constraints for the IP model

  #Assume that all fire engines have returned to their home stations if there is a difference of more than 30 minutes between runs (2 simulation periods)
  if (i==1) {
    current_availability <- starting_availability
  } else if (i==2) {
    current_availability <- starting_availability - simulation_summary[(i-1),2:9]
    current_availability[current_availability < 0] <- 0
  } else {
    current_availability <- starting_availability - apply(simulation_summary[(i-1):(i-2),2:9],2,sum)
    current_availability[current_availability < 0] <- 0
  }

  deployment_solution <- solve_ip(effective_distance,current_availability)

  #Add station names to deployment_solution binary (0/1) matrix
  colnames(deployment_solution$solution) <- colnames(starting_availability)
  simulation_results[i:(i+nrow(deployment_solution$solution)-1),3:10] <- deployment_solution$solution
  
  #Aggregate results into single line for each period
  simulated_deployment <- matrix(apply(deployment_solution$solution,2,sum),nrow=1,ncol=8)
  colnames(simulated_deployment) <- colnames(starting_availability)
  
  #Update optimization results
  simulation_summary[i,-1] <- cbind(simulated_deployment,Total_Distance=deployment_solution$objval)
}

#Write simulation results to output file
write_csv(simulation_results,"simulation_results.csv")
write_csv(simulation_summary,"simulation_summary.csv")
