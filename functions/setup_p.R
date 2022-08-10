setup_p <- function() {
  
  p <- c()
  
  ### dalec model values
  ## Fraction of NPP allocated to foliage
  p$af = 0.45   
  
  ## Fraction of NPP allocated to root
  p$ar = 0.19   
  
  ## Fraction of NPP allocated to wood
  p$aw = 0.21   
  
  ## respiration rate of leaf
  p$rf = 0.004484774
  
  ## respiration rate of root
  p$rr = 0.001637644
  
  ## respiration rate of wood
  p$rw = 0.000204796
  
  ## respiration rate of soil
  p$rs = 0.000870421

  ## Turnover rater of foliage
  p$tf = 0.003041096
  
  ## Turnover rate of root
  p$tr = 0.001753425
 
  ## Turnover rate of fresh litter
  p$tl = 0.004794521
  
  # fraction of carbon leaving litter that enters the soil pool
  p$frac_l = 0.515
  
  ### Initial conditions
  ## Filiage C pool
  p$cf0 = 157
  
  ## root
  p$cr0 = 885
  
  ## Wood (stems and coarse roots)
  p$cw0 = 4709  
  
  ## Litter
  p$cl0 = 79.8
  
  ## Soil
  p$cs0 = 2282
  
  
  
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
