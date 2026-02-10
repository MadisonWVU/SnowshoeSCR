trim_images_56days <- function(images) {

  # Determine first timestamp per site
  first_times <- images %>%
    group_by(`Site Name`) %>%
    summarize(first_date = min(Timestamp, na.rm = TRUE), .groups = "drop")
  
  # Join to images to calculate days from first image
  images_check <- images %>%
    left_join(first_times, by = "Site Name") %>%
    mutate(days_from_start = as.numeric(difftime(Timestamp, first_date, units = "days")))
  
  # Check if any rows exceed 56 days
  rows_to_remove <- images_check %>% filter(days_from_start > 56)
  
  if (nrow(rows_to_remove) == 0) {
    message("✅ No images  taken after 56 days. Dataset left unchanged.")
    return(images)
  } else {
    # Count rows removed per site
    removal_counts <- rows_to_remove %>%
      group_by(`Site Name`) %>%
      summarize(removed = n(), .groups = "drop")
    
    message("⚠️ Images beyond 56 days were removed:")
    for (i in seq_len(nrow(removal_counts))) {
      message(paste0("  Site ", removal_counts$`Site Name`[i], ": ", removal_counts$removed[i], " row(s) removed"))
    }
    
    # Keep only rows within 56 days
    images_trimmed <- images_check %>% filter(days_from_start <= 56) %>% select(-first_date, -days_from_start)
    
    return(images_trimmed)
  }
}