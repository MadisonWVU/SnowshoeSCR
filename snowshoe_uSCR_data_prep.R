############## use uSCR to estimate density of snowshoe hares #################
setwd("C:/Users/Owner/Desktop/camera trap/uSCR")

#first need to add lat/long to image file and export it
#only do this the first time after downloading raw data
library(dplyr)
images<- read.csv('LCA6/traptagger_raw.csv')
deployment<- read.csv('LCA6/LCA6_2024_Deployments.csv')

images_ll <- images %>%
  left_join(
    deployment %>% select(Site.Name, Latitude, Longitude),
    by = "Site.Name")

images_ll <- images_ll %>%
  relocate(Latitude, Longitude, .after = 1)

write.csv(
  images_ll,
  "LCA6/LCA6_2024_Images.csv",
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
  "readr")    #Summarizing data    

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

#camera on y-axis, Date on x-axis, point size = number of hares detected that day
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

#rename to specific site (change for new site)
camera_counts_CRAN<-camera_counts
camera_days_CRAN<- camera_days
out_CRAN<-out
deps_CRAN<-deps

#save all needed objects
save(camera_counts_CRAN, camera_days_CRAN, out_CRAN, deps_CRAN, file = "CRAN.RData")
