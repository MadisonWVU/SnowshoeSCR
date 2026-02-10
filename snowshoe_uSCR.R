############## use uSCR to estimate density of snowshoe hares #################
setwd("C:/Users/Owner/Desktop/camera trap/uSCR")

#first need to add lat/long to image file and export it
#only do this the first time after downloading raw data
library(dplyr)
images<- read.csv('CRAN/cranberry_traptagger_raw.csv')
deployment<- read.csv('CRAN/CRAN_2024_Deployments.csv')

images_ll <- images %>%
  left_join(
    deployment %>% select(Site.Name, Latitude, Longitude),
    by = "Site.Name")

images_ll <- images_ll %>%
  relocate(Latitude, Longitude, .after = 1)

write.csv(
  images_ll,
  "CRAN/CRAN_2024_Images.csv",
  row.names = FALSE)
#manually check new images file whether columns are correctly named in order

#need R version 4.5.0 or higher
#have your data and functions formatted as below
#/Camera Project/
#  ├── MORR/
#  │    ├── MORR_2025_Deployments.csv
#  │    ├── MORR_2025_Images.csv
#  ├── functions/
#  │    ├── check_deployments.R
#  │    ├── check_images.R
#  │    ├── trim_images_56days.R
#  │    ├── format_deployments.R
#  └── Camera_Unmarked_Analyses.Rmd 

#install all needed packages
list.of.packages <- c(
  "sf",            #Spatial data manipulation
  "tidyverse",     #A package for data manipulation
  "nimble",        #To run all the unmarked models
  "nimbleHMC",     #Sampler to achieve convergence faster
  "MCMCvis",       #Visualize model output 
  "dplyr",         #Data manipulation
  "readr",         #Reading in data
  "parallel",      #Running models in parallel  
  "ggplot2",       #Making plots and figures 
  "camtrapR",      #Visualizing camera data 
  "lubridate",     #Manipulating dates and times
  "leaflet",       #Making interactive maps
  "reshape2",      #Making heat maps
  "ggrepel",       #Moving text in ggplot
  "data.table",
  "read.r")    #Summarizing data    

#check you have them in your library
new.packages <- list.of.packages[!(list.of.packages %in% 
                                     installed.packages()[,"Package"])]
#load them
if(length(new.packages)) install.packages(new.packages,
                                          repos = "http://cran.us.r-project.org")
lapply(list.of.packages, require, character.only = TRUE)

#load all needed functions
source("functions/check_deployments.R") #checks deployment dataset for accuracy
source("functions/check_images.R") #checks images dataset for accuracy
source("functions/trim_images_56days.R") #checks for photos taken after
#56 days of deployment and trims
source("functions/format_deployments.R") #cleans deployment dataset in cases 
#where cameras were deployed > 56 days

#bring in data
park_code <- "CRAN"
survey_year <- 2024

####################### DIRECTORY SETUP ######################################
project_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

#find all folders starting with park code
park_folders <- list.dirs(project_dir, recursive = FALSE, full.names = TRUE)
matching_folders <- park_folders[grepl(paste0("^", park_code),
                                       basename(park_folders))]

if (length(matching_folders) == 0) {
  stop(paste("Error: No folder found starting with", park_code, "in", 
             project_dir))
} else if (length(matching_folders) == 1) {
  # Only one folder found; use it
  park_dir <- matching_folders
  message(paste("Using park folder:", basename(park_dir)))
} else {
  #multiple folders found; must select
  cat("Multiple folders found for park", park_code, ":\n")
  for (i in seq_along(matching_folders)) {
    cat(i, ":", basename(matching_folders[i]), "\n")
  }
  selection <- as.integer(readline(prompt = "Enter the number of 
                                   the folder to use: "))
  
  #validate selection
  if (is.na(selection) || selection < 1 || selection > 
      length(matching_folders)) {
    stop("Invalid selection. Please run again and choose a valid number.")
  }
  
  park_dir <- matching_folders[selection]
  message(paste("Using park folder:", basename(park_dir)))
}

########################### FIND FILES #########################################
unit_code <- basename(park_dir) 
unit_code_clean <- gsub(" ", "_", unit_code)  # replace spaces with underscores

files <- list.files(
  park_dir,
  pattern = paste0(unit_code_clean, "_", survey_year, "_.*\\.csv$"),
  full.names = TRUE
)

deployment_file <- files[grepl("_Deployments\\.csv$", files, 
                               ignore.case = TRUE)]
images_file     <- files[grepl("_Images\\.csv$", files, ignore.case = TRUE)]

#checks
if (length(deployment_file) != 1 | length(images_file) != 1) {
  stop(paste0("Error: Could not uniquely identify deployment/images files for ", 
              park_code, " in ", survey_year, 
              ". Ensure filenames follow 'PARK_YEAR_Deployments.csv' and
               'PARK_YEAR_Images.csv'."))
}

############################## READ IN DATA ####################################
#deployment files
deployments <- read_csv(deployment_file)

#standardize column names: trim whitespace and remove special characters
names(deployments) <- gsub("\\.+$", "", names(deployments)) 
names(deployments) <- gsub("[\\s\\p{Z}]+$", "", names(deployments), perl = TRUE) 
names(deployments) <- gsub("^[\\s\\p{Z}]+", "", names(deployments), perl = TRUE) 
names(deployments) <- gsub("[\\s\\p{Z}]+", " ", names(deployments), perl = TRUE)
names(deployments) <- trimws(names(deployments))

#trim leading/trailing whitespace from all character columns
deployments[] <- lapply(deployments, function(x) {
  if (is.character(x)) {
    # Remove any Unicode whitespace at start/end
    gsub("^[\\s\\p{Z}]+|[\\s\\p{Z}]+$", "", x, perl = TRUE)
  } else {
    x
  }
})

#Images file
images <- read_csv(images_file,
                   col_types = cols(
                     `Sighting Count` = col_character(),
                     Species = col_character()
                   ))
names(images) <- gsub("\\.+$", "", names(images)) 
names(images) <- trimws(names(images)) 
images$Timestamp <- sub(" UTC$", "", images$Timestamp)

#convert timestamp to correct format
images$Timestamp <- as.POSIXct(images$Timestamp, format = "%m/%d/%Y %H:%M")
images$Timestamp <- format(images$Timestamp, "%Y-%m-%d %H:%M:%S")
head(images$Timestamp)

#Separate rows with multiple species into distinct rows
images <- images %>%
  separate_rows(Species, `Sighting Count`, sep = "\\|") %>%
  mutate(`Sighting Count` = as.numeric(`Sighting Count`))

# --- Status message ---
if (grepl("_", unit_code_clean)) {
  # Unit-level folder detected (e.g., FRSP_SPOT)
  cat("Loaded data for Park:", park_code, 
      "Unit:", sub(paste0("^", park_code, "_"), "", unit_code_clean),
      "Year:", survey_year, "\n")
} else {
  # Park-level folder only (e.g., FRSP)
  cat("Loaded data for Park:", park_code, 
      "Year:", survey_year, "\n")
}

################### SUMMARY STATISTICS AND PLOTS ##############################
deploy_summary <- deployments %>%
  mutate(
    Start_Date = as.Date(`Start Date`, format = "%m/%d/%Y"),
    End_Date   = as.Date(`End Date`,   format = "%m/%d/%Y"),
    operational_days = as.numeric(difftime(End_Date, Start_Date, 
                                           units = "days"))
  ) %>%
  summarise(
    n_cameras = n(),
    mean_days = mean(operational_days, na.rm = TRUE),
    total_camera_days = sum(operational_days, na.rm = TRUE)
  )

print(deploy_summary)

#interactive map of camera locations
icon.fa<-makeAwesomeIcon(
  icon="camera", markerColor="red",
  library="fa",
  iconColor="black"
)

leaflet(deployments) %>% addTiles()%>%
  addAwesomeMarkers(lng=~Longitude, lat=~Latitude, icon=icon.fa,
                    popup=~as.character(`Site Name`), 
                    label=~as.character(`Site Name`))

#summary statistics on images
image_summary <- images %>%
  group_by(Species) %>%
  summarise(
    total_images = n(),
    total_detections = sum(as.numeric(`Sighting Count`), na.rm = TRUE),
    cameras_detected = n_distinct(`Site Name`)
  ) %>%
  arrange(desc(total_detections))

print(image_summary)

ggplot(image_summary, aes(x=reorder(Species, -total_detections),
                          y=total_detections)) +
  geom_bar(stat="identity", fill="darkgreen") +
  labs(x="Species", y="Total Detections") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

#summary per camera
hare_summary <- images %>%
  group_by(`Site Name`) %>%
  summarise(
    hare_images = n(),
    hare_detections = sum(as.numeric(`Sighting Count`), na.rm = TRUE)
  )

print(hare_summary)

#summarize total hares per camera
hare_counts <- images %>%
  group_by(`Site Name`, Latitude, Longitude) %>%
  summarize(total_hare = sum(`Sighting Count`, na.rm = TRUE), .groups = "drop")

#bubble plot with site names
ggplot(hare_counts, aes(x = Longitude, y = Latitude)) +
  geom_point(aes(size = total_hare), color = "forestgreen", alpha = 0.7) +
  geom_text_repel(aes(label = `Site Name`),
                  size = 3,
                  nudge_y = 0.0005,       # small vertical adjustment
                  nudge_x = 0.0005,       # small horizontal adjustment
                  max.overlaps = Inf) +   # ensures all labels are shown
  scale_size_continuous(name = "Total Hares") +
  coord_fixed() +
  labs(title = "Total Hare Detections by Camera",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right")

#daily hare detections per camera
daily_hare <- images %>%
  mutate(Date = as.Date(Timestamp)) %>%
  group_by(`Site Name`, Date) %>%
  summarize(detections = sum(`Sighting Count`, na.rm = TRUE), .groups = "drop")

#camera on y-axis, Date on x-axis, point size = number of deer detected that day
ggplot(daily_hare, aes(x = Date, y = `Site Name`, size = detections)) +
  geom_point(color = "forestgreen", alpha = 0.7, position = position_jitter
             (width = 0, height = 0.1)) +
  scale_size_continuous(name = "Hare Count", range = c(2, 8)) +
  labs(
    title = "Daily Hare Detections by Camera",
    x = "Date",
    y = "Site"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    legend.position = "right"
  )

########################## FORMAT DATA FOR MODELS ##############################
deps<- deployments

#determine mean longitude to estimate UTM zone
mean_lon <- mean(deployments$Longitude, na.rm = TRUE)
utm_zone <- floor((mean_lon + 180) / 6) + 1
epsg_code <- 26900 + utm_zone

#convert to UTM
utm_coords <- deployments %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4269) %>%
  st_transform(crs = epsg_code) %>%
  st_coordinates()

#add UTM columns to your dataframe, center and convert to KM
min_start <- as.Date(min(deployments$`Start Date`, na.rm = TRUE), format = "%m/%d/%Y")

deps <- deployments %>%
  mutate(utm_e = (utm_coords[, 'X'] -
                    mean(c(min(utm_coords[, 'X']),
                           max(utm_coords[, 'X'])))) / 1000,
         utm_n = (utm_coords[, 'Y'] -
                    mean(c(min(utm_coords[, 'Y']),
                           max(utm_coords[, 'Y'])))) / 1000) %>%
  rename(Site = `Site Name`) %>%
  mutate(`Start Date` = as.Date(deps$`Start Date`, format = "%m/%d/%Y"),
         `End Date` = as.Date(deps$`End Date`, format = "%m/%d/%Y")) %>% 
  mutate(
    `Start Index` = as.numeric(difftime(`Start Date`, min_start, units = 'days')) + 2,
    `End Index_raw` = ceiling(as.numeric(difftime(`End Date`, min_start, units = 'days')))
  ) %>%
  rowwise() %>%
  mutate(`End Index` = `End Index_raw`) %>%
  ungroup() %>%
  select(-`End Index_raw`)

#camera photo information
seqs <- images %>%
  group_by(`Cluster ID`) %>%
  summarise(Site = unique(`Site Name`),
            latitude = unique(Latitude),
            longitude = unique(Longitude),
            detection_date = min(date(Timestamp)),
            hare_count = max(as.numeric(`Sighting Count`)),
            start = min(Timestamp),
            end = max(Timestamp)) %>%
  mutate(cluster_length_days =
           as.numeric(difftime(end, start, units = 'days')))

#make sure all dates are covered by adding 0 counts of hares
#at all sites over all days cameras were deployed.
zero_counts <- expand.grid(
  Site = unique(deps$`Site`),
  detection_date = date(seq(from = min(deps$`Start Date`),
                            to = max(deps$`End Date`),
                            by = 'day'))) %>%
  mutate(hare_count = 0, cluster_length_days = 0)

#counting hare detections and camera operation time by date
counts_time <- seqs %>%
  bind_rows(zero_counts) %>%
  group_by(Site, detection_date) %>%
  summarise(detections = sum(hare_count),
            camera_time_days = sum(cluster_length_days))

#detections to matrix
counts <- counts_time %>%
  select(!camera_time_days) %>%
  pivot_wider(names_from = detection_date,
              values_from = detections,
              values_fill = 0,
              names_sort = T)

#combining with camera-level data to preserve site order
out <- counts %>% left_join(deps, by = 'Site')

detection_matrix <- as.matrix(counts %>% column_to_rownames('Site'))

#exporting time cameras operational to matrix
camera_time_matrix <- as.matrix(counts_time %>%
                                  select(!detections) %>%
                                  pivot_wider(names_from = detection_date,
                                              values_from = camera_time_days,
                                              values_fill = 0,
                                              names_sort = T) %>%
                                  column_to_rownames('Site'))

#summarizing counts and detection days by camera
camera_counts <- camera_days <- numeric(nrow(out))

for(i in 1:nrow(out)){
  
  camera_counts[i] <-
    sum(detection_matrix[i, out$`Start Index`[i]:out$`End Index`[i]])
  
  camera_days[i] <- length(out$`Start Index`[i]:out$`End Index`[i]) -
    sum(camera_time_matrix[i, out$`Start Index`[i]:out$`End Index`[i]])
  
}
################## RUN THE MODEL ##############################################
#Unmarked Spatial Count Model

#Start with M=8,000; iter=101,000; burin=1,000; and thin = 100. If you receive
#warning messages that indicate lack of convergence, follow the prompts to re-run
#the model by increasing M, iter, burin, and/or thin values.

#Informative Priors in the USCR Model

#Spatial scale parameter (home range) on log scale
  #example for deer: `log_sigma` ~ `dnorm(-1.4442, sd = 0.1451)
  #Based on a deer average home range size of 1.1 km^2. 
  #Increase SD to  weaken constraint if poor mixing or modify mean based 
  #on regionally specific home rage size estimate

#hare home range ~7 ha
alpha <- 0.95
q <- qchisq(p = alpha, df <- 2)
q
r<-0.15
qa<-sqrt(q)
sigma<-r/qa
log(sigma)

#calculate SD for 3.5-10.5 ha
sd_log_area <- log(1.5)
sd_log_sigma <- 0.5 * sd_log_area
sd_log_sigma

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
  log_sigma ~ dnorm(-2.792288, sd = 0.2027326)
  log_lam_0 ~ dnorm(0, 1)
  psi ~ dunif(0, 1)
  sd_eps ~ dgamma(1, 1)
  
  sigma <- exp(log_sigma)
  lam_0 <- exp(log_lam_0)
  
  #model for unmarked animals
  for(i in 1:M){ #looping through data augmented individuals
    
    z[i] ~ dbern(psi) #is individual in population
    
    #coordinates of home range centers
    hrc[i, 1] ~ dunif(xlim[1], xlim[2])
    hrc[i, 2] ~ dunif(ylim[1], ylim[2])
    
    for(j in 1:J){ #looping through all cameras
      
      dist2[i, j] <- (hrc[i, 1] - cam[j, 1]) ^ 2 +
        (hrc[i, 2] - cam[j, 2]) ^ 2
      
      #detection intensity
      lambda[i, j] <- z[i] * lam_0 *
        exp(-dist2[i, j] / (2 * sigma ^ 2))
      
    }
    
    in_ss[i] ~ dconstraint(min(sqrt(dist2[i, 1:J])) < buffer)
    
  }
  
  #count model
  for(j in 1:J){
    
    eps[j] ~ dnorm(0, sd_eps)
    Lambda[j] <- sum(lambda[1:M, j])
    log(mu[j]) <- log(Lambda[j]) + log(camera_days[j]) + eps[j]
    
    y[j] ~ dpois(mu[j])
    y_sim[j] ~ dpois(mu[j])
    
    pearson_obs[j] <- (y[j] - mu[j]) ^ 2 / mu[j]
    pearson_sim[j] <- (y_sim[j] - mu[j]) ^ 2 / mu[j]
    
  }
  
  sum_obs <- sum(pearson_obs[1:J])
  sum_sim <- sum(pearson_sim[1:J])
  
  bp <- step(sum_sim - sum_obs)
  
  N <- sum(z[1:M])
  D_mi2 <- N / area_m2
  
})

#state_space buffer distance, km
buffer <- 0.10 * sqrt(qchisq(p = 0.99, df = 2))

#calculating area of buffer
cam_buff <- deps %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4269) %>%
  st_transform(crs = epsg_code) %>%
  st_buffer(buffer * 1000) %>%
  st_union()

#preparing data for NIMBLE
Const <- list(
  M = 1000, #number of data augmented individuals
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
                 niter = 1000, nburnin = 50, thin = 3,
                 monitors = c('log_sigma', 'log_lam_0', 'psi', 'sd_eps',
                              'sigma', 'lam_0', 'N', 'D_mi2',
                              'sum_obs', 'sum_sim', 'bp',
                              'z', 'hrc', 'eps'))
stopCluster(this_cluster)

#fit the model in single
fit <- pblapply(1:3, function(i) {
  run_MCMC_allcode(
    data = Data,
    constants = Const,
    code = code,
    inits = inits,
    niter = 10100,
    nburnin = 100,
    thin = 3,
    monitors = c('log_sigma', 'log_lam_0', 'psi', 'sd_eps',
                 'sigma', 'lam_0', 'N', 'D_mi2',
                 'sum_obs', 'sum_sim', 'bp',
                 'z', 'hrc', 'eps')
  )
})

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
