library(nimble)
library(parallel)
library(sf)
library(dplyr)

################### BRING IN DATA FROM ALL SITES ##############################
load("CRAN.RData")
load("KRO2.RData")

#create vector with all sites
sites <- list(
  CRAN = list(camera_counts = camera_counts_CRAN,
              camera_days = camera_days_CRAN,
              out = out_CRAN,
              deps = deps_CRAN),
  KRO2 = list(camera_counts = camera_counts_KRO2,
               camera_days = camera_days_KRO2,
               out = out_KRO2,
               deps = deps_KRO2))

#number of sites
S <- length(sites)

#define M for each site
M <- c(100, 100)

#calculate number of cameras per site
J <- sapply(sites, function(x) nrow(x$out))

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

#latent detection creation for each site
code <- nimbleCode({
  
  #site shared priors
  log_sigma ~ dnorm(-2.730253, sd = 0.045)
  log_lam_0 ~ dnorm(0, 1)
  sd_eps ~ dgamma(1,1)
  
  lam_0 <- exp(log_lam_0)
  sigma <- exp(log_sigma)
  
  
  #site loop
  for(s in 1:S){
    
    #latent inclusion probability per site
    psi[s] ~ dunif(0,1)
    
    #loop over augmented individuals at site s
    for(i in 1:M[s]){
      z[s,i] ~ dbern(psi[s])  #1/0 individual present
      
      #latent activity centers
      hrc[s,i,1] ~ dunif(xlim[s,1], xlim[s,2])
      hrc[s,i,2] ~ dunif(ylim[s,1], ylim[s,2])
      
      #loop over cameras at site s to calculate distance between latent activity
      #centers of individuals i and cameras j, compute lambda for each
      for(j in 1:J[s]){
        dist2[s,i,j] <- (hrc[s,i,1] - cam[s,j,1])^2 + (hrc[s,i,2] - cam[s,j,2])^2
        lambda[s,i,j] <- z[s,i] * lam_0 * exp(-dist2[s,i,j] / (2 * sigma^2))
      }
      
      #constraint to keep activity centers within buffer of state space
      for(j in 1:J[s]){
        is_close[s,i,j] <- step(buffer[s] - sqrt(dist2[s,i,j]))
      }
      in_ss[s,i] <- max(is_close[s,i,1:J[s]])
    }
    
    #count model for cameras in each site
    for(j in 1:J[s]){
      
      eps[s,j] ~ dnorm(0, sd_eps)
      
      #sum of latent detection intensities from all individuals at this site
      Lambda[s,j] <- sum(lambda[s, 1:M[s], j])
      
      #expected number of detections, accounting for camera days and eps
      log(mu[s,j]) <- log(Lambda[s,j]) + log(camera_days[s,j]) + eps[s,j]
      
      #observed counts
      y[s,j] ~ dpois(mu[s,j])
      
      #simulated counts for posterior predictive checking
      y_sim[s,j] ~ dpois(mu[s,j])
      
      #pearson residuals for model fit
      pearson_obs[s,j] <- (y[s,j] - mu[s,j])^2 / mu[s,j]
      pearson_sim[s,j] <- (y_sim[s,j] - mu[s,j])^2 / mu[s,j]
    }
    
    #sums of pearson residuals
    sum_obs[s] <- sum(pearson_obs[s, 1:J[s]])
    sum_sim[s] <- sum(pearson_sim[s, 1:J[s]])
    bp[s] <- step(sum_sim[s] - sum_obs[s])
    
    #site-level population size and density
    N[s] <- sum(z[s, 1:M[s]])
    D_km2[s] <- N[s] / area_km2[s]
    
  } 
  
}) 

#state space buffer distance and site areas
buffer_sites <- list()
area_sites <- setNames(numeric(S), names(sites))

for(site_name in names(sites)){
  
  #calculate buffer
  buf <- 0.10 * sqrt(qchisq(p = 0.99, df = 2))
  buffer_sites[[site_name]] <- buf
  
  #UTM zone (taken from data cleaning section)
  mean_lon <- mean(sites[[site_name]]$deps$Longitude, na.rm = TRUE)
  utm_zone <- floor((mean_lon + 180)/6) + 1
  epsg_code <- 26900 + utm_zone
  
  #buffer polygon and area
  cam_buff <- sites[[site_name]]$deps %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4269) %>%
    st_transform(crs = epsg_code) %>%
    st_buffer(buf * 1000) %>%
    st_union()
  
  #convert to km2 and store in sites
  area_sites[site_name] <- as.numeric(st_area(cam_buff)) / 1e6 
}

#prepare constants for NIMBLE
Const <- list(
  S = S,
  M = M,
  J = as.numeric(J),
  xlim = t(sapply(sites, function(x) c(min(x$out$utm_e) - buffer_sites[[names(sites)[1]]],
                                       max(x$out$utm_e) + buffer_sites[[names(sites)[1]]]))),
  ylim = t(sapply(sites, function(x) c(min(x$out$utm_n) - buffer_sites[[names(sites)[1]]],
                                       max(x$out$utm_n) + buffer_sites[[names(sites)[1]]]))),
  cam = lapply(sites, function(x) cbind(x$out$utm_e, x$out$utm_n)),
  buffer = unlist(buffer_sites),
  camera_days = lapply(sites, function(x) x$camera_days),
  area_km2 = area_sites
)

#convert cam list to array for NIMBLE
max_J <- max(Const$J)
cam_array <- array(NA, dim = c(Const$S, max_J, 2))
for(s in 1:Const$S){
  cam_s <- Const$cam[[s]]
  cam_array[s, 1:nrow(cam_s), 1:2] <- cam_s
}
Const$cam <- cam_array

#convert camera_days list to matrix
camera_days_array <- matrix(NA, nrow = Const$S, ncol = max_J)
for(s in 1:Const$S){
  days <- Const$camera_days[[s]]
  camera_days_array[s, 1:length(days)] <- days
}
Const$camera_days <- camera_days_array

#observed data for each site
max_M <- max(Const$M)
y_array <- matrix(0, nrow = Const$S, ncol = max_J)
for(s in 1:Const$S){
  y_s <- sites[[s]]$camera_counts
  y_array[s, 1:length(y_s)] <- y_s
}

in_ss_array <- matrix(1, nrow = Const$S, ncol = max_M)

Data <- list(
  y = y_array,
  in_ss = in_ss_array
)

#initial values
inits <- function(){
  max_M <- max(Const$M)
  hrc_array <- array(NA, dim = c(Const$S, max_M, 2))
  for(s in 1:Const$S){
    M_s <- Const$M[s]
    J_s <- Const$J[s]
    cam_s <- Const$cam[s,,]
    random_cam_index <- sample(J_s, M_s, replace = TRUE)
    hrc_array[s, 1:M_s, 1] <- cam_s[random_cam_index, 1] + runif(M_s, -0.01, 0.01)
    hrc_array[s, 1:M_s, 2] <- cam_s[random_cam_index, 2] + runif(M_s, -0.01, 0.01)
  }
  list(hrc = hrc_array)
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
clusterExport(this_cluster, varlist = c("Const", "Data", "inits", "code", "run_MCMC_allcode"))
fit <- parLapply(cl = this_cluster, X = 1:chains, fun = run_MCMC_allcode,
                 data = Data,
                 constants = Const,
                 code = code,
                 inits = inits,
                 niter = 44000,
                 nburnin = 1000,
                 thin = 200,
                 monitors = c('log_sigma', 'log_lam_0', 'psi', 'sd_eps',
                              'sigma', 'lam_0', 'N',
                              'sum_obs', 'sum_sim', 'bp',
                              'z', 'hrc', 'eps', 'D_km2'))
stopCluster(this_cluster)

#checking posterior distributions
diag <- MCMCsummary(list(fit[[1]]$samples, fit[[2]]$samples, fit[[3]]$samples),
                    c('log_sigma', 'log_lam_0', 'psi', 'sd_eps', 'sigma',
                      'lam_0', 'N', 'sum_obs', 'sum_sim', 'bp', 'D_km2'))
diag

MCMCtrace(list(fit[[1]]$samples, fit[[2]]$samples, fit[[3]]$samples),
          c('log_sigma', 'log_lam_0', 'psi', 'sd_eps', 'sigma', 'lam_0', 'N'
            ,'sum_obs', 'sum_sim', 'bp'), pdf = F)

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

#check psi shape for site 1
psi_post <- do.call(rbind, list(fit[[1]]$samples, fit[[2]]$samples,
                                fit[[3]]$samples))[,"psi[1]"]
if(mean(psi_post > 0.9) > 0.01){
  warning("⚠️ Psi is frequently near 1. This may indicate too few augmented 
          individuals (M) or insufficient iterations. Try increasing M 
          (e.g. M = 2000 - 4000) or increasing the number of iterations
          (e.g., iter = iter * 2).")
}

# Check N vs M boundary issue for site 1
N_post <- do.call(rbind, list(fit[[1]]$samples, fit[[2]]$samples,
                              fit[[3]]$samples))[,"N[1]"]
if(mean(N_post > (Const$M * 0.95)) > 0.05){
  warning("⚠️ Posterior of N is approaching M (data augmentation limit). 
          Try increasing M (e.g. M = 2000 - 4000) or increasing the number of 
          iterations (e.g., iter = iter * 2).")
}

write.csv(
  do.call(rbind,
          list(fit[[1]]$samples,
               fit[[2]]$samples,
               fit[[3]]$samples))[, c('sigma', 'lam_0', 'D_km2', 'bp', 'psi')],
  'USCR-Posterior.csv', row.names = F)
