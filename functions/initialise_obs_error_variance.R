initialise_obs_error_variance <- function(s, err_var) {
  
  err_var[s$RF,] = 0.2   # % 
  err_var[s$RR,] = 0.2   # % 
  err_var[s$RW,] = 0.2   # % 
  err_var[s$RS,] = 0.2   # % 
  
  return(err_var)
}