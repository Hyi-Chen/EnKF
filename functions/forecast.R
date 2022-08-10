forecast <- function(s, p, i, A, err_var, err_type, ens_var, q) {
  
  A_tmp = matrix(0, s$ndims, s$nrens)
  A_mean = rep(0, s$ndims)
  
  # generate model prediction
  M <- matrix(0, s$ndims, s$ndims)
  M[1,1] <- 1 - p$tf
  M[2,2] <- 1 - p$tr
  M[3,3] <- 1
  M[4,1] <- p$tf
  M[4,2] <- p$tr
  M[4,4] <- 1 - p$tl
  M[5,4] <- p$tl * p$frac_l
  M[5,5] <- 1 - p$rs
  
  G <- matrix(0, s$ndims, s$nrens)
  G[1,] <- gpp[i,3] * cue * p$af
  G[2,] <- gpp[i,3] * cue * p$ar
  G[3,] <- gpp[i,3] * cue * p$aw 

  A_tmp <- M %*% A + G
  A <- A_tmp
  
  
  # simulate the time evolution of model errors, eq 43
  for (j in 1:s$ndims) {
    for (k in 1:s$nrens) {
      q_previous_time_step <- q[j,k]
      q[j,k] <- p$alpha * q_previous_time_step + sqrt(1.0 - p$alpha^2) * rnorm(1, 0.0, 1.0)
    }
  }
  
  # Calculate ensemble average
  for (j in 1:s$ndims) {
    A_mean[j] <- sum(A[j,]) / s$nrens
  }

  # calculate the error variance
  ens_var <- assign_model_errors(s, ens_var, err_var, err_type, A_mean)
  
  # add the noise model into the ensemble, eq 43
  for (j in 1:s$ndims) {
    for (k in 1:s$nrens) {
      A[j,k] <- A[j,k] + sqrt(p$delta_t) * p$rho * sqrt(ens_var[j]) * q[j,k]
    }
  }
  
  
  return(list(A=A, ens_var=ens_var, q=q))
}  
  
