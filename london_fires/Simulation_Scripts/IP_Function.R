#This file contains the "solve_ip" function, which solves the integer programming (IP) problem for the LFB deployment optimization.

#Checks to see if "lpSolve" package is installed, and if not, installs it
if(!require(lpSolve)){
  install.packages("lpSolve")
  library(lpSolve)
}

#Create function that can be called each time we want to solve the IP problem
solve_ip <- function(station_distances,availability) {
  #Set RHS column constraints to 1 for all columns
  min_deployment <- rep(1,nrow(station_distances))
  
  #Set column constraints to less than or equal to RHS
  column_signs <- rep("<=",ncol(station_distances))
  
  #Set row constraints to greater than or equal to RHS
  row_signs <- rep(">=",nrow(station_distances))
  
  #Solve IP problem given information above
  deployment <-
    lp.transport(station_distances, direction="min", row_signs, min_deployment,
                 column_signs, availability)

  return(deployment)
}