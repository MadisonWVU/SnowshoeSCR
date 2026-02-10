format_deployments <- function(deployments) {
  
  # Ensure proper columns exist and remove any empty Camera IDs
  deps <- deployments %>%
    filter(`Camera ID` != "") %>%
    mutate(
      `Start Date` = as.POSIXct(`Start Date`, format = '%m/%d/%Y'),
      `End Date`   = as.POSIXct(`End Date`, format = '%m/%d/%Y'),
      `Camera Malfunction Date` = as.POSIXct(`Camera Malfunction Date`, format = '%m/%d/%Y')
    )
  
  # --- Replace End Date with Camera Malfunction Date where appropriate ---
  malfunction_sites <- deps %>%
    filter(tolower(`Camera Functioning`) == "no" & !is.na(`Camera Malfunction Date`)) %>%
    pull(`Site Name`)
  
  deps <- deps %>%
    mutate(`End Date` = if_else(
      tolower(`Camera Functioning`) == "no" & !is.na(`Camera Malfunction Date`),
      `Camera Malfunction Date`,
      `End Date`
    ))
  
  if (length(malfunction_sites) > 0) {
    message("🛠 Used Camera Malfunction Date as End Date for: ",
            paste(unique(malfunction_sites), collapse = ", "))
  }
  
  # --- Calculate relative indices ---
  min_start <- min(deps$`Start Date`, na.rm = TRUE)
  
  deps <- deps %>%
    mutate(
      `Start Index` = as.numeric(difftime(`Start Date`, min_start, units = 'days')) + 2,
      `End Index_raw` = ceiling(as.numeric(difftime(`End Date`, min_start, units = 'days')))
    )
  
  # --- Identify and trim long deployments ---
  trim_idx <- which(deps$`End Index_raw` - deps$`Start Index` + 1 > 56)
  trimmed_sites <- deps$`Site Name`[trim_idx]
  
  if (length(trimmed_sites) > 0) {
    message("⚠️ Trimmed End Index to 56 days for: ", paste(unique(trimmed_sites), collapse = ", "))
  }
  
  deps <- deps %>%
    rowwise() %>%
    mutate(`End Index` = min(`End Index_raw`, `Start Index` + 56 - 1)) %>%
    ungroup() %>%
    select(-`End Index_raw`)
  
  return(deps)
}