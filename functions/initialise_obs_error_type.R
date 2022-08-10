initialise_obs_error_type <- function(s, err_type) {
  ## Define types of error 
  ## % as 1
  ## absolute values as 0
  
  err_type[s$RF] = 1
  err_type[s$RR] = 1
  err_type[s$RW] = 1
  err_type[s$RS] = 1

  
  return(err_type)
}