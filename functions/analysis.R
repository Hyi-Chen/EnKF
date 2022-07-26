analysis <- function(A, s, obs, i,
         err_var, err_type, 
         err_var_obs, err_type_obs, 
         ens_var, q) {
  
  ### The standard analysis eqn: A = A + Pe H^T(H Pe H^T + Re)^-1 (D - H A)    (eq 48)
  ### can be reformed using D' = D - HA, Pe = A'(A')^T, Re = YY^T
  ### (where Y symbolises Gamma) such that it
  ### becomes A = A + A' A'^T H^T(HA' A'^T H^T + YY^T)^-1 D'
  
  ## Define matrix and vector
  nrmin <- min(s$nrobs, s$nrens)         # Minimum of nrobs and nrens
  nrsigma <- 0
  sigsum <- 0
  sigsum1 <- 0
  
  I <- diag(1, s$nrens,s$nrens)        # identity matrix
  H <- matrix(0, s$nrobs,s$ndims)        # Matrix holding observation operator
  HA <- matrix(0, s$nrobs,s$nrens)  
  A_dash <- matrix(0, s$ndims,s$nrens) # A'
  D <- matrix(0, s$nrobs,s$nrens)        # matrix holding innovations D
  E <- matrix(0, s$nrobs,s$nrens)        # Matrix holding observation uncertainty
  S <- matrix(0, s$nrobs,s$nrens)        # matrix holding product of HA'
  ES <- matrix(0, s$nrobs,s$nrens)       # Matrix holding product of HA' + E
  Reps <- matrix(0, s$ndims,s$nrobs)     # A'(HA')T
  U <- matrix(0, s$nrobs,nrmin)          # local variable
  VT <- matrix(0, s$nrens, s$nrens)    # HA'+E = U ∑ VT
  X1 <- matrix(0, nrmin,s$nrobs)         # local variable
  X2 <- matrix(0, nrmin,s$nrens)       # local variable
  X3 <- matrix(0, s$nrobs,s$nrens)       # local variable
  X4 <- matrix(0, s$nrens,s$nrens)     # local variable

  sig <- rep(0, nrmin)   # ∑  
  D_mean <- rep(0, s$nrobs)
  E_mean <- rep(0, s$nrobs)
  HA_mean <- rep(0, s$nrobs)
  A_mean <- rep(0, s$ndims)
  
  ## Generate the H matrix
  H[1,1] <- p$rf
  H[2,2] <- p$rr
  H[3,3] <- p$rw
  H[4,2] <- p$rw
  H[4,5] <- p$rs
  
  ## HA
  HA <- H %*% A
  
  ## observation uncertainty E 
  for (j in 1:s$nrobs) {
      E[j,] <- rnorm(s$nrens, mean=0, sd=abs(obs[j,i] * err_var_obs[j,i])) 
  }
  E_mean <- rowMeans(E, na.rm=T)
  
  
  ## the innovations D'(D' = D - HA) 
  for (j in 1:s$nrobs) {
    for (k in 1:s$nrens) {
      ## Add the observation uncertainty E to observation
      D[j,k] <- E[j,k] + obs[j,i] - HA[j,k]
    }
  }
  D_mean <- rowMeans(D, na.rm=T)
  
  
  ### Calculate HA' by taking mean_HA from HA. 
  ## mean value of HA 
  HA_mean <- rowMeans(HA, na.rm=T)
  
  ## HA' = HA - mean_HA 
  for (j in 1:s$nrobs) {
    for (k in 1:s$nrens) {
      S[j,k] <- HA[j,k] - HA_mean[j]
    }
  }
  
  ## ES = HA' + E (evenson page 356) , Ha' stored in S
  ES <- S + E
  
  ### Compute SVD of HA'+E store in U, eqn 59 and pg 357 evenson example 2003
  out <- svd(ES, nu = min(s$nrobs, s$nrens), nv = min(s$nrobs, s$nrens))
  U <- out$u   
  VT <- out$v
  sig <- out$d
  
  ## Convert to eigenvalues and work out sigsum - pg 357 evenson example 2003
  for (j in 1:nrmin) {
    sig[j] <- sig[j]^2
    sigsum <- sigsum + sig[j]
  }
  
  ## Compute number of significant eigenvalues - pg 357 evenson example 2003
  for (j in 1:nrmin) {
    if ((sigsum1 / sigsum) < 0.999) {
      nrsigma <- nrsigma + 1
      sigsum1 <- sigsum1 + sig[j]
      sig[j] <- 1.0 / sig[j]
    } else {
      for (k in j:nrmin) {
        sig[k] <- 0.0 
      }
    }
  }
  
  ## Compute X1 = Λ-1 * UT - pg 357 evenson example 2003
  X1 <- diag(sig) %*% t(U)
  
  ## Compute X2 = X1 * D' - pg 357 evenson example 2003 
  X2 <- X1 %*% D
  
  ## Compute X3 = U * X2 - pg 357 evenson example 2003
  X3 <- U %*% X2
  
  ## Compute final analysis - pg 357 evenson example 2003
  ## X4 = (HA')^T * X3
  ## X5 = X4 +I
  ## A = A + A' * X5
  if ((2 * s$ndims * s$nrobs) > (s$nrens * (s$nrobs + s$ndims))) {
    ## compute X4 = (HA')^T * X3 - note S matrix is HA' 
    X4 <- t(S) %*% X3
    
    ## Compute X5 = X4 + I (store in X4) 
    for (k in 1:s$nrens) {
      X4[k,k] <- X4[k,k] + 1.0;
    }
    X4 <- X4 + I
    
    ## Compute A = A * X5 (note X5 stored in X4 -> see Evenson) */
    A <- A %*% X4
    
  } else {
    ## Compute representers Reps = A' * S^T
    ## Calculate the mean of the ensemble required to work out A' 
    A_mean <- rowMeans(A, na.rm=T)
    
    ## Figure out A' 
    for (j in 1:s$ndims) {
      for (k in 1:s$nrens) {
        A_dash[j,k] <- A[j,k] - A_mean[j]
      }
    }
    
    ## Compute representers Reps = A' * S^T 
    Reps <- A_dash %*% t(S)
    
    ## Compute A = A + Reps * X3 
    A <- A + Reps %*% X3
  }
  
  return(A)
}
