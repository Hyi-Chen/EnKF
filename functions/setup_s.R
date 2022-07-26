setup_s <- function() {
  
  s <- c()
  
  ### Number of observations
  s$nrobs = 4   
  
  ### Dimension of model state
  s$ndims = 5  
  
  ### Number of ensemble members
  s$nrens = 200 
  
  ### Position of each variable in the A
  s$CF = 1
  s$CR = 2
  s$CW = 3
  s$CL = 4
  s$CS = 5
  
  ### Position of each variable in the D
  s$RF = 1
  s$RW = 2
  s$RR = 3
  s$RS = 4
  
  return(s)
  
}
