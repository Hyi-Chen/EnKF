
####---- Clear the console ----####
rm(list=ls(all=TRUE))

####----  Set up the model stuffs ----####
### read in GPP data
gpp = read.csv("observation/GPP_ele_0802.csv")
ndays <- nrow(gpp)
cue <- 0.35

####----  source all the input variables and functions ----####
source("prepare.R")

####----  Set up the model stuffs ----####
### initialise parameters and structures
p <- setup_p() 
s <- setup_s() 

### Setup matrix holding ensemble members
A <- matrix(0, s$ndims, s$nrens)
err_var <- rep(0, s$ndims)
ens_var <- rep(0, s$ndims)


### initialize noise matrix 
q <- matrix(rnorm(s$ndims*s$nrens, 0.0, 1.0), s$ndims, s$nrens)

### initialize A
A <- initialise_ensemble(p, s, A) 

### initialize error stuffs
err_var <- initialise_error_variance(s, err_var)

####----  Set up the observation stuffs ----####
obsDF <- read.csv("observation/obs_ele.csv", header=F)
obs <- as.matrix(obsDF[,2:ncol(obsDF)])
obs <- apply(obs, c(1, 2), as.numeric)

#### initialize measurement error variance and type
err_var_obs <- read.csv("observation/obsSD_ele.csv", header=F)
err_var_obs <- as.matrix(err_var_obs[,2:ncol(err_var_obs)])
err_var_obs <- apply(err_var_obs, c(1, 2), as.numeric)

####----  set up storage df to store the simulation output, with uncertainties ----####
ensembleDF <- matrix(0, nrow=ndays, ncol=(1+s$ndims*2))
ensembleDF <- as.data.frame(ensembleDF)
colnames(ensembleDF) <- c("Days", "CF", "CR", "CW", "CL", "CS", 
                          "CF_STDEV", "CR_STDEV", "CW_STDEV", "CL_STDEV", "CS_STDEV")
ensembleDF$Days <- c(1:ndays)

####---- Run the model ----####
for (i in 1:ndays) {
  
  A <- analysis(A, s, obs, i,  
                err_var, err_type, 
                err_var_obs, err_type_obs, 
                ens_var, q)  
  
  ## Forecast model
  out <- forecast(s, p, i, A, err_var,
                  err_type, ens_var, q)
  
  ## split the list of the forecast model
  A <- out$A                 # model prediction ensemble member
  ens_var <- out$ens_var     # model variance
  q <- out$q                 # model error
  

  ## Save output
  ensembleDF[i, 2:(s$ndims*2+1)] <- dump_output(s, A)
  
}

####----  Plotting ----####
### plotting    
ggplot() +
  geom_ribbon(data=ensembleDF, aes(x = Days, ymin=CF-CF_STDEV, 
                                   ymax=CF+CF_STDEV), fill="grey", alpha=1) +
  geom_line(data=ensembleDF, aes(y = CF, x=Days), color = "black") 

ggplot() +
  geom_ribbon(data=ensembleDF, aes(x = Days, ymin=CR-CR_STDEV, 
                                   ymax=CR+CR_STDEV), fill="grey", alpha=1) +
  geom_line(data=ensembleDF, aes(y = CR, x=Days), color = "black") 

ggplot() +
  geom_ribbon(data=ensembleDF, aes(x = Days, ymin=CW-CW_STDEV, 
                                   ymax=CW+CW_STDEV), fill="grey", alpha=1) +
  geom_line(data=ensembleDF, aes(y = CW, x=Days), color = "black") 

ggplot() +
  geom_ribbon(data=ensembleDF, aes(x = Days, ymin=CL-CL_STDEV, 
                                   ymax=CL+CL_STDEV), fill="grey", alpha=1) +
  geom_line(data=ensembleDF, aes(y = CL, x=Days), color = "black") 

ggplot() +
  geom_ribbon(data=ensembleDF, aes(x = Days, ymin=CS-CS_STDEV, 
                                   ymax=CS+CS_STDEV), fill="grey", alpha=1) +
  geom_line(data=ensembleDF, aes(y = CS, x=Days), color = "black") 

