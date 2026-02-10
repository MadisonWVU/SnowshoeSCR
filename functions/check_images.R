check_images <- function(images, deployments, survey_year) {
  issues <- c()
  fixes <- c()
  
  # ---- Required columns ----
  required_cols <- c("Site Name", "Latitude", "Longitude", "Timestamp",
                     "Species", "Sighting Count", "Image URL", "Cluster ID")
  missing_cols <- setdiff(required_cols, names(images))
  if (length(missing_cols) > 0) {
    stop(paste0(
      "❌ Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      "\nPlease fix the images file and re-run the function."
    ))
  }
  
  # ---- Trim whitespace from character columns ----
  char_cols <- names(images)[sapply(images, is.character)]
  images[char_cols] <- lapply(images[char_cols], trimws)
  
  # ---- Fix Site Names missing leading zero ----
  fix_idx <- which(grepl("^[A-Z]+_[0-9]$", images$`Site Name`))
  if (length(fix_idx) > 0) {
    corrected_names <- sub("^(.*_)([0-9])$", "\\10\\2", images$`Site Name`[fix_idx])
    fixes <- paste(images$`Site Name`[fix_idx], "→", corrected_names)
    images$`Site Name`[fix_idx] <- corrected_names
    message("🛠 Fixed Site Names:\n", paste(unique(fixes), collapse = "\n"))
  }
  
  # ---- Site Name validation ----
  valid_regex <- "^[A-Z]{4}_(0[1-9]|[1-9][0-9])$|^[A-Z]+_(0[1-9]|[1-9][0-9]|10[0-9]|[1-9][0-9]{2})$"
  invalid_idx <- which(!grepl(valid_regex, images$`Site Name`))
  if (length(invalid_idx) > 0) {
    site_counts <- table(images$`Site Name`[invalid_idx])
    for (sn in names(site_counts)) {
      issues <- c(issues, paste0(
        "❌ Invalid Site_Name: ", sn,
        " — found ", site_counts[[sn]], " occurrence(s); must follow 'PARK_##/PARK_###' or 'UNIT_##/UNIT_###'."
      ))
    }
  }
  
  # ---- Latitude / Longitude checks ----
  lat_num <- suppressWarnings(as.numeric(images$Latitude))
  lon_num <- suppressWarnings(as.numeric(images$Longitude))
  
  # Latitude checks
  bad_lat <- which(is.na(lat_num) | lat_num == 0)
  if (length(bad_lat) > 0) {
    issues <- c(issues, paste0("❌ ", length(bad_lat), " image(s) have missing or zero Latitude"))
  }
  
  # Longitude checks
  bad_lon <- which(is.na(lon_num) | lon_num == 0)
  if (length(bad_lon) > 0) {
    issues <- c(issues, paste0("❌ ", length(bad_lon), " image(s) have missing or zero Longitude"))
  }
  
  # Auto-fix positive longitudes
  fix_lon <- which(!is.na(lon_num) & lon_num > 0 & lon_num <= 180)
  if (length(fix_lon) > 0) {
    old_vals <- lon_num[fix_lon]
    new_vals <- -abs(old_vals)
    images$Longitude[fix_lon] <- new_vals
    message("🛠 Fixed Longitude for ", length(fix_lon), " image(s) ")
  }
  
  # ---- Required fields check ----
  for (field in c("Species", "Sighting Count", "Image URL")) {
    missing_rows <- which(is.na(images[[field]]) | images[[field]] == "")
    if (length(missing_rows) > 0) {
      issues <- c(issues, paste0("❌ Missing ", field, " in ", length(missing_rows), " image(s)"))
    }
  }
  
  # ---- Cluster ID numeric check ----
  cluster_num <- suppressWarnings(as.numeric(images$`Cluster ID`))
  bad_cluster <- which(!is.na(images$`Cluster ID`) & images$`Cluster ID` != "" & is.na(cluster_num))
  if (length(bad_cluster) > 0) {
    issues <- c(issues, paste0("❌ ", length(bad_cluster), " image(s) have non-numeric Cluster ID"))
  }
  
  # ---- Cross-check Site Names with deployments ----
  bad_site_match <- which(!images$`Site Name` %in% deployments$`Site Name`)
  if (length(bad_site_match) > 0) {
    issues <- c(issues, paste0("❌ ", length(bad_site_match), " image(s) have Site Names not found in deployments"))
  }
  
  # ---- Timestamp parsing and year check ----
  ts_parsed <- suppressWarnings(as.POSIXct(images$Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  missing_time <- is.na(ts_parsed) & !is.na(images$Timestamp) & images$Timestamp != ""
  if (any(missing_time)) {
    ts_parsed[missing_time] <- suppressWarnings(
      as.POSIXct(paste0(images$Timestamp[missing_time], " 00:00:00"),
                 format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    )
  }
  bad_ts <- which(!is.na(images$Timestamp) & images$Timestamp != "" & is.na(ts_parsed))
  if (length(bad_ts) > 0) {
    site_counts <- table(images$`Site Name`[bad_ts])
    for (sn in names(site_counts)) {
      issues <- c(issues, paste0("❌ Bad Timestamp format at site ", sn,
                                 " — ", site_counts[[sn]], " occurrence(s); should be yyyy-mm-dd HH:MM:SS."))
    }
  }
  
  # ---- Timestamp vs deployment window check ----
  # Prepare deployment dates
  dt_deploy <- data.table::data.table(deployments)
  dt_deploy[, dep_start := as.Date(`Start Date`, format = "%m/%d/%Y")]
  dt_deploy[, dep_end   := as.Date(`End Date`,   format = "%m/%d/%Y")]
  
  # Merge image timestamps with deployment window
  dt_images <- data.table::data.table(images)
  dt_images[, ts := ts_parsed]
  dt_images[, img_date := as.Date(ts)]
  
  merged_dates <- merge(
    dt_images,
    dt_deploy[, .(`Site Name`, dep_start, dep_end)],
    by = "Site Name",
    all.x = TRUE
  )
  
  # Allowable buffer around deployment window (days)
  buffer_days <- 3
  
  # Identify images outside deployment window
  out_of_window <- merged_dates[
    !is.na(img_date) &
      (
        img_date < (dep_start - buffer_days) |
          img_date > (dep_end + buffer_days)
      )
  ]
  
  if (nrow(out_of_window) > 0) {
    site_counts <- out_of_window[, .N, by = `Site Name`]
    for (i in seq_len(nrow(site_counts))) {
      issues <- c(issues, paste0(
        "⚠️ Timestamp outside deployment window at site ",
        site_counts$`Site Name`[i],
        " — ", site_counts$N[i], " image(s)."
      ))
    }
  }
  
  # ---- First image vs. Start Date check ----
  dt_images <- data.table::data.table(images)
  dt_images[, ts := ts_parsed]
  
  # Get first image per site
  first_img <- dt_images[!is.na(ts), .(first_img = min(ts)), by = `Site Name`]
  
  # Prepare deployment start dates
  dt_deploy <- data.table::data.table(deployments)
  dt_deploy[, dep_start := as.Date(`Start Date`, format = "%m/%d/%Y")]
  
  # Merge and compare
  merged_dates <- merge(first_img, dt_deploy[, .(`Site Name`, dep_start)], by = "Site Name", all.x = TRUE)
  merged_dates[, first_img_date := as.Date(first_img)]
  
  mismatched_sites <- merged_dates[first_img_date != dep_start, `Site Name`]
  if (length(mismatched_sites) > 0) {
    issues <- c(issues, paste0(
      "❌ Date mismatch for ", length(mismatched_sites), " site(s): ",
      paste(mismatched_sites, collapse = ", "),
      " — first image date does not match deployment Start Date."
    ))
  }
  
  # ---- Final output ----
  if (length(issues) == 0) {
    message("✅ Images file is formatted correctly and all Site Names match deployments!")
  } else {
    warning("⚠️ Issues found in images file:\n", paste(issues, collapse = "\n"))
  }
  
  return(images)
}
