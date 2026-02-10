check_deployments <- function(deployment) {
  issues <- list()
  
  # ---- Column presence ----
  required_cols <- c(
    "Park", "Site Name", "Camera ID", "SD Card ID", 
    "Start Date", "Start Time", "End Date", "End Time", 
    "Latitude", "Longitude", "Camera Height", 
    "Camera Orientation", "Camera Functioning", 
    "Camera Malfunction Date", "Detection Distance", "Notes"
  )
  
  missing_cols <- setdiff(required_cols, names(deployment))
  if (length(missing_cols) > 0) {
    cat(
      "❌ Missing required column(s):", paste(missing_cols, collapse = ", "), "\n\n",
      "💡 Guidance:\n",
      "• If these columns are truly missing from your dataset (for example, no Start Time or End Time was ever recorded),\n",
      "  please **add the column names** to your CSV file and leave the cells blank.\n",
      "• If you believe these columns exist but are labeled slightly differently (e.g., 'Start_Date' instead of 'Start Date'),\n",
      "  please rename them to match exactly the expected column names above.\n\n",
      "After fixing the file, re-run `check_deployments()`.\n"
    )
    stop("Deployment file is missing one or more required columns.")
  }
  
  # ---- Auto-fix Site Names missing leading zero ----
  deployment$`Site Name` <- mapply(function(sn, park) {
    if (is.na(sn) || sn == "") return(sn)
    
    # Match pattern like HOFU_1 or MORR_9 (no leading zero)
    if (grepl(paste0("^", park, "_[0-9]$"), sn)) {
      num <- sub(".*_", "", sn)
      corrected <- paste0(park, "_0", num)
      message("🛠 Fixed Site Name: ", sn, " → ", corrected)
      return(corrected)
    } else {
      return(sn)
    }
  }, deployment$`Site Name`, deployment$Park, USE.NAMES = FALSE)
  
  # --- Site Name validation ---
  for (i in seq_len(nrow(deployment))) {
    park <- deployment$Park[i]
    sn   <- trimws(deployment$`Site Name`[i]) 
    
    if (!is.na(sn) && sn != "") {
      # Suffix: 01–09, 10–99, or 100–999
      re_suffix <- "(0[1-9]|[1-9][0-9]|[1-9][0-9]{2})$"
      
      # Build patterns
      re_park_only        <- paste0("^", park, "_", re_suffix)              # FRSP_01
      re_unit_only        <- paste0("^[A-Za-z]{2,}_", re_suffix)            # WILD_01
      re_park_unit_us     <- paste0("^", park, "_[A-Za-z]{2,}_", re_suffix) # FRSP_WILD_01
      re_park_unit_concat <- paste0("^", park, "[A-Za-z]{2,}_", re_suffix)  # FRSPWILD_01
      
      patterns <- c(re_park_only, re_unit_only, re_park_unit_us, re_park_unit_concat)
      
      is_valid <- any(vapply(patterns,
                             function(p) grepl(p, sn, ignore.case = TRUE),
                             logical(1)))
      
      if (!is_valid) {
        issues <- c(issues, paste0(
          "Invalid Site_Name: ", sn,
          " → must match one of: 'PARK_##/PARK_###' (e.g., FRSP_09 or FRSP_101), ",
          "'UNIT_##/UNIT_###' (e.g., WILD_01), ",
          "'PARK_UNIT_##' (e.g., FRSP_WILD_01), or 'PARKUNIT_##' (e.g., FRSPWILD_01)."
        ))
      }
    }
  }
  
  # ---- Per-row checks ----
  deployment_cols <- setdiff(required_cols, c("Park", "Site Name", "Notes"))
  
  for (i in seq_len(nrow(deployment))) {
    row_values <- deployment[i, deployment_cols]
    
    # Skip row if all deployment columns except Park, Site Name, Notes are blank
    if (all(is.na(row_values) | row_values == "")) next
    
    park <- deployment$Park[i]
    sn <- deployment$`Site Name`[i]
    cam <- deployment$`Camera ID`[i]
    
    # --- Camera ID ---
    if (is.na(cam) || cam == "") {
      issues <- c(issues, paste("❌ Missing Camera ID in row", i, " — must have a value"))
    }
    
    # --- SD Card ID ---
    sd_val <- deployment$`SD Card ID`[i]
    if (is.na(sd_val) || sd_val == "") {
      issues <- c(issues, paste("❌ Missing SD Card ID in row", i, " — must have a value"))
    }
    
    # --- Dates ---
    date_cols <- c("Start Date", "End Date")
    for (col in date_cols) {
      val <- deployment[[col]][i]
      if (!is.na(val) && val != "") {
        if (is.na(as.Date(val, format = "%m/%d/%Y"))) {
          issues <- c(issues, paste("❌ Bad date in", col, "row", i, ":", val, " — should be mm/dd/yyyy"))
        }
      }
    }
    
    # Camera Malfunction Date only required if Camera Functioning == "No"
    cam_func <- deployment$`Camera Functioning`[i]
    if (!is.na(cam_func) && tolower(cam_func) == "no") {
      val <- deployment$`Camera Malfunction Date`[i]
      if (is.na(val) || val == "") {
        issues <- c(issues, paste("❌ Camera Malfunction Date missing in row", i, 
                                  " — required because Camera Functioning = No"))
      } else if (is.na(as.Date(val, format = "%m/%d/%Y"))) {
        issues <- c(issues, paste("❌ Bad date in Camera Malfunction Date row", i, ":", val, 
                                  " — should be mm/dd/yyyy"))
      }
    }
    
    # --- Times ---
    time_cols <- c("Start Time", "End Time")
    for (col in time_cols) {
      val <- deployment[[col]][i]
      if (!is.na(val) && val != "") {
        if (!grepl("^(?:[01]?[0-9]|2[0-3]):[0-5][0-9](:[0-5][0-9])?$", val)) {
          issues <- c(issues, paste("❌ Bad time in", col, "row", i, ":", val, " — should be HH:MM or HH:MM:SS 24h"))
        }
      }
    }
    
    # ---- Numeric checks with suppression ----
    num_cols <- c("Camera Height", "Detection Distance")
    for (col in num_cols) {
      val <- deployment[[col]][i]
      if (!is.na(val) && val != "" && suppressWarnings(is.na(as.numeric(val)))) {
        issues <- c(issues, paste("❌ Non-numeric value in", col, "row", i, ":", val))
      }
      
      if (col == "Longitude" && !is.na(val) && val != "" && as.numeric(val) >= 0) {
        issues <- c(issues, paste("❌ Longitude in row", i, "is not negative:", val,
                                  " — must have a minus sign for western hemisphere"))
      }
    }
    
    # --- Latitude check (must exist and be non-zero) ---
    lat_val <- suppressWarnings(as.numeric(deployment$Latitude[i]))
    if (is.na(lat_val) || lat_val == 0) {
      issues <- c(issues, paste("❌ Latitude missing or invalid (0) in row", i))
    }
    
    # --- Longitude check (must exist and be non-zero; auto-fix sign if positive) ---
    long_val <- suppressWarnings(as.numeric(deployment$Longitude[i]))
    if (is.na(long_val) || long_val == 0) {
      issues <- c(issues, paste("❌ Longitude missing or invalid (0) in row", i))
    } else if (long_val > 0) {
      # Auto-fix longitude sign
      fixed_val <- -abs(long_val)
      deployment$Longitude[i] <- fixed_val
      message("🛠 Fixed Longitude in row ", i, ": ", long_val, " → ", fixed_val)
    }
    
    # --- Camera Orientation ---
    val <- deployment$`Camera Orientation`[i]
    valid_cardinals <- c("N","NE","E","SE","S","SW","W","NW")
    if (!is.na(val) && val != "") {
      val_upper <- toupper(val)
      val_numeric <- suppressWarnings(as.numeric(val))
      if (!(val_upper %in% valid_cardinals | (!is.na(val_numeric) & val_numeric >= 0 & val_numeric <= 359))) {
        issues <- c(issues, paste("❌ Invalid Camera Orientation in row", i, ":", val,
                                  "— must be N/NE/.../NW or 0–359 degrees"))
      }
    }
    
    # --- Camera Functioning ---
    val <- deployment$`Camera Functioning`[i]
    if (!is.na(val) && val != "") {
      val_lower <- tolower(val)
      if (!(val_lower %in% c("yes","no"))) {
        issues <- c(issues, paste("❌ Invalid Camera Functioning value in row", i, ":", val, " — must be Yes or No"))
      }
    }
    
  } # end row loop
  
  # ---- Output ----
  if (length(issues) == 0) {
    message("✅ Deployments file is formatted correctly!")
  } else {
    warning("⚠️ Issues found in deployments file:\n", paste(issues, collapse = "\n"))
  }
  return(deployment)
}