initialise_error_type <- function(s, err_type) {
  ## Define types of error 
  ## % as 1
  ## absolute values as 0
  
  err_type[s$CF] = 1
  err_type[s$CR] = 1
  err_type[s$CW] = 1
  err_type[s$CL] = 1
  err_type[s$CS] = 1
  
  return(err_type)
}