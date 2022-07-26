setup_p <- function() {
  
  p <- c()
  
  ### dalec model values
  ## Fraction of NPP allocated to foliage
  p$af = 0.515851693
  
  ## Fraction of NPP allocated to root
  p$ar = 0.368619022
  
  ## respiration rate of leaf
  p$rf = 0.004735553
  
  ## respiration rate of root
  p$rr = 0.001680672
  
  ## respiration rate of wood
  p$rw = 0.000187537
  
  ## respiration rate of soil
  p$rs = 0.000775607

  ## Turnover rater of foliage
  p$tf = 0.002739726
  
  ## Turnover rate of root
  p$tr = 0.00182648
 
  ## Turnover rate of fresh litter
  p$tl = 0.02
  
  
  
  ### Initial conditions
  ## Filiage C pool
  p$cf0 = 151
  
  ## root
  p$cr0 = 833
  
  ## Wood (stems and coarse roots)
  p$cw0 = 4558
  
  ## Litter
  p$cl0 = 92.9
  
  ## Soil
  p$cs0 = 2183
  
  
  
  # timestep
  p$delta_t = 1.0
  
  # specified time decorrelation length s.
  p$tau = 1.0
  
  # The factor a should be related to the time step used, eqn 32
  # (i.e. this is zero)
  p$alpha = 1.0 - (p$delta_t / p$tau)
  
  # setup factor to ensure variance growth over time becomes independent of
  # alpha and delta_timestep (as long as the dynamical model is linear).
  p$rho = setup_stochastic_model_error(p)
  
  
  return(p)
  
}
