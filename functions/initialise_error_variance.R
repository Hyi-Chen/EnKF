initialise_error_variance <- function(s, err_var) {
  
  # default error variances for the state vector elements
  err_var[s$CF] = 0.2  # %
  err_var[s$CR] = 0.2  # %
  err_var[s$CW] = 0.2  # %
  err_var[s$CL] = 0.2  # %
  err_var[s$CS] = 0.2  # %
  
  return(err_var)
}
