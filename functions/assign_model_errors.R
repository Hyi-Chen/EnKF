assign_model_errors <- function(s, ens_var, err_var, err_type, A_mean) {
  for (j in 1:s$ndims) {
      ens_var[j] <- err_var[j] * abs(A_mean[j])
  }
  
  return(ens_var)
}
