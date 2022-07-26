initialise_ensemble <- function(p, s, A) {
  
  A[s$CF,] = p$cf0 + rnorm(s$nrens, 0.0, 0.1 * p$cf0)
  A[s$CR,] = p$cr0 + rnorm(s$nrens, 0.0, 0.1 * p$cr0)
  A[s$CW,] = p$cw0 + rnorm(s$nrens, 0.0, 0.1 * p$cw0)
  A[s$CL,] = p$cl0 + rnorm(s$nrens, 0.0, 0.1 * p$cl0)
  A[s$CS,] = p$cs0 + rnorm(s$nrens, 0.0, 0.1 * p$cs0)
  
  return(A)
}
