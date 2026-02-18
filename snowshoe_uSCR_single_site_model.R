################## RUN THE MODEL ##############################################
#Unmarked Spatial Count Model

#Informative Priors in the USCR Model
#Spatial scale parameter (home range) on log scale
#example for deer: `log_sigma` ~ `dnorm(-1.4442, sd = 0.1451)
#Based on a deer average home range size of 1.1 km^2. 
#Increase SD to  weaken constraint if poor mixing or modify mean based 
#on regionally specific home rage size estimate

#hare home range ~8 ha
alpha <- 0.95
q <- qchisq(p = alpha, df = 2)
q
r<-0.1596
qa<-sqrt(q)
sigma<-r/qa
log(sigma)

#create sd (in ha) based on prior distribution of values from 6-10 ha
#6 ha
hr6 <- 6 * 0.01
r6 <- sqrt(hr6 / pi)
sigma6 <- r6 / sqrt(q)
log6 <- log(sigma6)

#10 ha
hr10 <- 10 * 0.01
r10 <- sqrt(hr10 / pi)
sigma10 <- r10 / sqrt(q)
log10 <- log(sigma10)

sd_logsigma <- (log10 - log6) / (2 * 1.96)
sd_logsigma

log_sigma_sim <- rnorm(10000, mean = -2.730253, sd = 0.045)
sigma_sim <- exp(log_sigma_sim)

r_sim <- sigma_sim * sqrt(q)
HR_ha_sim <- pi * r_sim^2 * 100

hist(HR_ha_sim, breaks = 40,
     main = "Implied Home Range Distribution",
     xlab = "Home Range (ha)")

#Baseline detection intensity
#`log_lam_0` ~ `dnorm(0, 1)`
#Increase SD for weaker prior

#Proportion of augmented individuals actually present, do not modify
#`psi` ~ `dunif(0, 1)`

#camera-level random effect
#sd_eps` ~ `dgamma(1,1)`
#Adjust shape to reduce prior influence

code <- nimbleCode({
  
  #set priors
  log_sigma ~ dnorm(-2.730253, sd = 0.045)
  log_lam_0 ~ dnorm(0, 1)
  psi ~ dunif(0, 1)
  sd_eps ~ dgamma(1, 1)
  
  sigma <- exp(log_sigma)
  lam_0 <- exp(log_lam_0)
  
  #model for unmarked animals
  for(i in 1:M){ #looping through data augmented individuals
    
    z[i] ~ dbern(psi) #latent capture history, assigning 0/1 to individuals
    
    #coordinates of latent home range centers
    hrc[i, 1] ~ dunif(xlim[1], xlim[2])
    hrc[i, 2] ~ dunif(ylim[1], ylim[2])
    
    for(j in 1:J){ #looping through all cameras
      
      #squared distance between individual i and cam j
      dist2[i, j] <- (hrc[i, 1] - cam[j, 1]) ^ 2 + 
        (hrc[i, 2] - cam[j, 2]) ^ 2
      
      #half-normal detection function, estimating latent activity center location
      #to explain counts at all cameras
      lambda[i, j] <- z[i] * lam_0 *
        exp(-dist2[i, j] / (2 * sigma ^ 2))
      
    }
    
    #state space constraint, an activity center must be within buffer distance of 
    #at least one camera
    in_ss[i] ~ dconstraint(min(sqrt(dist2[i, 1:J])) < buffer)
    
  }
  
  #count model
  for(j in 1:J){ #j each cam, J is total number of cameras
    
    #expected number of detections at each camera given detection intensity
    eps[j] ~ dnorm(0, sd_eps)
    Lambda[j] <- sum(lambda[1:M, j])
    log(mu[j]) <- log(Lambda[j]) + log(camera_days[j]) + eps[j]
    
    #actual observed counts at camera j
    y[j] ~ dpois(mu[j])
    #simulated counts from model
    y_sim[j] ~ dpois(mu[j])
    
    #measures how far counts are from expected
    pearson_obs[j] <- (y[j] - mu[j]) ^ 2 / mu[j]
    pearson_sim[j] <- (y_sim[j] - mu[j]) ^ 2 / mu[j]
    
  }
  
  sum_obs <- sum(pearson_obs[1:J])
  sum_sim <- sum(pearson_sim[1:J])
  
  bp <- step(sum_sim - sum_obs)
  
  #N is number of animals, D_mi2 is density
  N <- sum(z[1:M])
  D_mi2 <- N / area_m2
  
})

#state space buffer distance in km
buffer <- 0.10 * sqrt(qchisq(p = 0.99, df = 2))

#calculating area of buffer
cam_buff <- deps %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4269) %>%
  st_transform(crs = epsg_code) %>%
  st_buffer(buffer * 1000) %>%
  st_union()

#preparing data for NIMBLE
Const <- list(
  M = 100, #number of data augmented individuals
  xlim = c(min(out$utm_e) - buffer,
           max(out$utm_e) + buffer),
  ylim = c(min(out$utm_n) - buffer,
           max(out$utm_n) + buffer),
  J = nrow(out),
  cam = cbind(out$utm_e, out$utm_n),
  buffer = buffer,
  camera_days = camera_days,
  area_m2 = as.numeric(st_area(cam_buff)) / (2.59 * 1000000) 
)

Data <- list(
  y = camera_counts,
  in_ss = rep(1, Const$M)
)

inits <- function(){
  hrc <- matrix(nrow = Const$M, ncol = 2)
  random_cam_index <- sample(Const$J, Const$M, T)
  hrc[, 1] <- Const$cam[random_cam_index, 1] + runif(Const$M, -0.01, 0.01)
  hrc[, 2] <- Const$cam[random_cam_index, 2] + runif(Const$M, -0.01, 0.01)
  list(
    hrc = hrc
  )
}

run_MCMC_allcode <- function(index, code, constants, data, inits = NULL,
                             monitors, niter, nburnin, thin){
  
  library(nimble)
  
  if(is.null(inits)){
    results <- nimbleMCMC(code, constants, data,
                          monitors = monitors,
                          niter = niter, nburnin = nburnin, thin = thin,
                          WAIC = TRUE)
  } else{
    results <- nimbleMCMC(code, constants, data, inits,
                          monitors = monitors,
                          niter = niter, nburnin = nburnin, thin = thin,
                          WAIC = TRUE)
  }
  
  return(results)
}

#fit the model using parallel processing
chains <- 3
this_cluster <- makeCluster(chains)
clusterExport(this_cluster, 'Const')

fit <- parLapply(cl = this_cluster, X = 1:3, fun = run_MCMC_allcode,
                 data = Data, constants = Const, code = code, inits = inits,
                 niter = 30500, nburnin = 500, thin = 10,
                 monitors = c('log_sigma', 'log_lam_0', 'psi', 'sd_eps',
                              'sigma', 'lam_0', 'N', 'D_mi2',
                              'sum_obs', 'sum_sim', 'bp',
                              'z', 'hrc', 'eps'))
stopCluster(this_cluster)

#checking posterior distributions
diag <- MCMCsummary(list(fit[[1]]$samples, fit[[2]]$samples, fit[[3]]$samples),
                    c('log_sigma', 'log_lam_0', 'psi', 'sd_eps', 'sigma',
                      'lam_0', 'N', 'D_mi2', 'sum_obs', 'sum_sim', 'bp'))
diag

MCMCtrace(list(fit[[1]]$samples, fit[[2]]$samples, fit[[3]]$samples),
          c('log_sigma', 'log_lam_0', 'psi', 'sd_eps', 'sigma', 'lam_0', 'N',
            'D_mi2', 'sum_obs', 'sum_sim', 'bp'), pdf = F)

################### Convergence Diagnostics and Checks #########################
# diag <- MCMCsummary(fit, round = 3)
high_rhat <- diag[diag$Rhat > 1.1, ]

if(nrow(high_rhat) > 0){
  warning("⚠️ Convergence issue detected: R-hat > 1.1 for ",
          nrow(high_rhat)," parameter(s). Consider increasing iterations.\n",
          "Try: iter = iter * 2 or thin = thin * 2"
  )
  print(high_rhat)
} else {
  message("✅ Convergence good: All R-hat values < 1.1")
}

#check psi shape
psi_post <- do.call(rbind, list(fit[[1]]$samples, fit[[2]]$samples,
                                fit[[3]]$samples))[,"psi"]
if(mean(psi_post > 0.9) > 0.01){
  warning("⚠️ Psi is frequently near 1. This may indicate too few augmented 
          individuals (M) or insufficient iterations. Try increasing M 
          (e.g. M = 2000 - 4000) or increasing the number of iterations
          (e.g., iter = iter * 2).")
}

# Check N vs M boundary issue
N_post <- do.call(rbind, list(fit[[1]]$samples, fit[[2]]$samples,
                              fit[[3]]$samples))[,"N"]
if(mean(N_post > (Const$M * 0.95)) > 0.05){
  warning("⚠️ Posterior of N is approaching M (data augmentation limit). 
          Try increasing M (e.g. M = 2000 - 4000) or increasing the number of 
          iterations (e.g., iter = iter * 2).")
}

#posterior draws
write.csv(
  do.call(rbind,
          list(fit[[1]]$samples,
               fit[[2]]$samples,
               fit[[3]]$samples))[, c('sigma', 'lam_0', 'D_mi2', 'bp', 'psi')],
  'USCR-Posterior.csv', row.names = F)
