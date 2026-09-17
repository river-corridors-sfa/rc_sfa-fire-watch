# =================================== Objectives =================================
#
# Track our EXO calibration records

# Step 1: 
# Step 2: 
# Step 3: 
# STEP 4: 
# STEP 5: 
# STEP 6: 
# Status: 
#
# Review status: 
# Notes: 

# ============================= Authorship ===========================
# Author: Jake Cavaiani
# 4 September 2026

# ============================ Libraries ===========================
rm(list = ls(all = TRUE))

library(pacman)
p_load(tidyverse,
       lubridate)


# ------  STEP 1: Path setup ------------------------------------------
# ---
ph_dir <- "/Users/cava304/Library/CloudStorage/OneDrive-PNNL/Documents - RC-SFA/Announcements and General/Sensor Calibration and Ordering/EXO2/pH"

# --- Parser for a single pH calibration file ---
parse_ph_cal <- function(file) {
  
  lines <- readLines(file, warn = FALSE, encoding = "latin1")
  
  # Helper to grab a value by key
  get_val <- function(key) {
    hit <- grep(key, lines, fixed = TRUE, value = TRUE)
    if (length(hit) == 0) return(NA_character_)
    # split on "=" and strip
    str_trim(str_remove(hit[1], paste0(".*", key)))
  }
  
  # Sensor serial from filename (e.g., 241112_Calibration_File_Export_24K102918)
  fname <- tools::file_path_sans_ext(basename(file))
  parts <- str_split(fname, "_")[[1]]
  cal_date <- suppressWarnings(ymd(parts[1]))
  sensor_sn_file <- parts[length(parts)]
  
  # Metadata
  sensor_sn <- get_val("Sensor Serial Number=")
  if (is.na(sensor_sn) || sensor_sn == "") sensor_sn <- sensor_sn_file
  
  delta_slope_47   <- get_val("pH Range 4.00-7.00 Delta Slope=")
  mv_dec_47        <- get_val("pH Range 4.00-7.00 mV per Dec")
  
}







#





