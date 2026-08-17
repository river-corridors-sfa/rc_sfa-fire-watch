# =================================== Objectives =================================
#
# Objectives: I want to generate a rating curve for the discharge and do some other summary stats 
# 
# Status: 
#
# Review status: 
# Notes: 
# We have 3 different ways to calculate depth:
  # 1.) Depth transects - maintanence metadata sheet
  # 2.) EXO depth readings - L1 EXO data 
  # 3.) Swoffer measurements - Swoffer metadata sheet. 
# Steps
  # 1.) Lets check our depth measurements between the depth transects and the EXO depth readings first and compare that to the Swoffer measurements
# ============================= Authorship ===========================
# Author: Jake Cavaiani
# 5 August 2026

# ============================ Libraries ===========================
rm(list = ls(all = TRUE))
# install.packages("devtools")
# library(devtools)
# devtools::install_github("DOI-USGS/streamMetabolizer")

library(pacman)
p_load(tidyverse,
       lubridate,
       scales, 
       fs,
       data.table,
       gsheet,
       fuzzyjoin)


# ================================= User inputs ================================
data_dir     <- "~/OneDrive - PNNL/Documents - RC-SFA/Study_PRT/EXO"
# data_dir   <- 'C:/Users/forb086/OneDrive - PNNL/Documents - RC-SFA/Study_PRT/EXO'

metadata_file_deployment  <- 'https://docs.google.com/spreadsheets/d/1xlAzs6bjV7yzWHqEAK55cjWj595yzz9tsr6gIQSgeKU/edit?gid=1077019345#gid=1077019345'
metadata_file_maintenance <- 'https://docs.google.com/spreadsheets/d/1n1yLYtAyI73BNjFVA3Go1wVuilkccrc-v5yt16TROq4/edit?usp=sharing'

sensor_specs_path <- '/Users/cava304/Library/CloudStorage/OneDrive-PNNL/Documents - RC-SFA/Announcements and General/Sensor Calibration and Ordering/sensor_specs.csv'

# ============================= Pull metadata =================================
metadata_deployment <- gsheet2tbl(metadata_file_deployment) %>%
  rename(Casing_Angle_Of_Redeployment = Casing_Angle) %>%
  mutate(Date = as.Date(Date))

metadata_maintenance <- gsheet2tbl(metadata_file_maintenance) %>%
  mutate(EXO_Time_Start_PST = as.character(EXO_Time_Start_PST),
         EXO_Time_End_PST   = as.character(EXO_Time_End_PST),
         Latitude           = as.numeric(str_replace_all(Latitude,  "\\[.*?\\]\\s*", "")),
         Longitude          = as.numeric(str_replace_all(Longitude, "\\[.*?\\]\\s*", "")),
         Date               = as.Date(Date))

# ---- Coerce maintenance columns to match deployment types ----
shared_cols <- intersect(names(metadata_deployment), names(metadata_maintenance))
for (col in shared_cols) {
  target_class <- class(metadata_deployment[[col]])[1]
  metadata_maintenance[[col]] <- switch(
    target_class,
    "character" = as.character(metadata_maintenance[[col]]),
    "numeric"   = suppressWarnings(as.numeric(metadata_maintenance[[col]])),
    "integer"   = suppressWarnings(as.integer(metadata_maintenance[[col]])),
    "Date"      = as.Date(metadata_maintenance[[col]]),
    "logical"   = as.logical(metadata_maintenance[[col]]),
    metadata_maintenance[[col]]
  )
}

metadata <- bind_rows(metadata_deployment, metadata_maintenance)
rm(metadata_deployment, metadata_maintenance, shared_cols)

# ============================= Pull EXO data =================================
# site_IDs <- unique(metadata$Site_ID)

# Get all L1 files
l1_files <- list.files(
  path = file.path(data_dir, "03_ProcessedData_L1"),
  pattern = "^L1_PRT_.*_EXO\\.csv$",
  full.names = TRUE
)

# Check that files exist
if (length(l1_files) == 0) {
  stop("No L1 files found.")
}

# Read and combine
combined_wide <- l1_files %>%
  set_names() %>%
  map_dfr(
    ~ read_csv(.x, col_types = cols(.default = "c")),
    .id = "file_name"
  ) %>%
  mutate(
    DateTime = ymd_hms(trimws(DateTime)),
    Out_of_Bounds = na_if(Out_of_Bounds, "N/A")
  ) %>%
  select(DateTime, Site_ID, Mean5sec_Depth, Out_of_Bounds) |> 
  arrange(DateTime)


# =============== Compare Metadata depth (depth transects) with EXO depth =================================
depth_transect <- metadata %>%
  mutate(
    Time = coalesce(
      na_if(Time_Start_PST, "-9999"),
      na_if(EXO_Time_Start_PST, "-9999")
    ),
    DateTime = ymd_hms(
      paste(Date, paste0(Time, ":00"))
    )
  ) %>%
  select(-Time)|> 
  select(DateTime, Site_ID, Depth_At_Sensor_cm:Point_E_Depth_cm) |> 
  na.omit(cols = DateTime)

# Average the transect depth
depth_transect <- depth_transect %>%
  mutate(
    across(c(Depth_At_Sensor_cm,
             Point_A_Depth_cm:Point_E_Depth_cm),
           ~ na_if(., -9999))
  ) %>%
  mutate(
    Mean_Transect_Depth_cm = rowMeans(
      select(., Point_A_Depth_cm:Point_E_Depth_cm),
      na.rm = TRUE
    )
  )


# clean up exo depth measurement
exo_clean <- combined_wide %>%
  mutate(
    Mean5sec_Depth = na_if(Mean5sec_Depth, "-9999"),
    Mean5sec_Depth = as.numeric(Mean5sec_Depth),
    Mean5sec_Depth = Mean5sec_Depth*100
  ) %>%
  filter(!is.na(Mean5sec_Depth))
  

library(data.table)

# Convert to data.tables
setDT(depth_transect)
setDT(exo_clean)

# Rename timestamps
setnames(depth_transect, "DateTime", "Transect_DateTime")
setnames(exo_clean, "DateTime", "EXO_DateTime")

# Nearest EXO match within site
comparison <- exo_clean[
  depth_transect,
  on = .(Site_ID, EXO_DateTime = Transect_DateTime),
  roll = "nearest",
  .(
    Site_ID = x.Site_ID,
    Transect_DateTime = i.Transect_DateTime,
    EXO_DateTime = x.EXO_DateTime,
    Mean5sec_Depth = x.Mean5sec_Depth,
    Depth_At_Sensor_cm = i.Depth_At_Sensor_cm,
    Point_A_Depth_cm = i.Point_A_Depth_cm,
    Point_B_Depth_cm = i.Point_B_Depth_cm,
    Point_C_Depth_cm = i.Point_C_Depth_cm,
    Point_D_Depth_cm = i.Point_D_Depth_cm,
    Point_E_Depth_cm = i.Point_E_Depth_cm,
    Mean_Transect_Depth_cm = i.Mean_Transect_Depth_cm
  )
]

# Calculate time difference
comparison_1 <- comparison %>%
  mutate(
    Time_difference_min = abs(
      as.numeric(difftime(
        Transect_DateTime,
        EXO_DateTime,
        units = "mins"
      ))
    ),
    Sensor_vs_EXO = Depth_At_Sensor_cm - Mean5sec_Depth,
    TransectMean_vs_EXO = Mean_Transect_Depth_cm - Mean5sec_Depth,
    Sensor_vs_TransectMean = Depth_At_Sensor_cm - Mean_Transect_Depth_cm
  ) |> 
  select(Site_ID, Transect_DateTime, EXO_DateTime, Mean5sec_Depth, Depth_At_Sensor_cm, Mean_Transect_Depth_cm, Time_difference_min, Sensor_vs_EXO:Sensor_vs_TransectMean)


# Plot the difference 
comparison_1 |> filter(Time_difference_min < 100) |> 
ggplot(aes(x = Mean5sec_Depth, y = Depth_At_Sensor_cm)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  facet_wrap(~Site_ID) +
  labs(
    x = "EXO Depth (cm)",
    y = "Mean Transect Depth (cm)"
  )


ggplot() +
  geom_line(data = exo_clean, aes(x = EXO_DateTime, y = Mean5sec_Depth)) +
  geom_point(data = depth_transect, aes(x = Transect_DateTime, y = Depth_At_Sensor_cm), color = "red") +
  ylim(c(0,200)) +
  facet_wrap(~Site_ID) +
  theme_bw()

summary <- depth_transect |> 
  group_by(Site_ID) |> 
  summarise(min = min(Depth_At_Sensor_cm),
            max = max(Depth_At_Sensor_cm)
            )

# Relationship between depth at sensor and EXO depth 
library(dplyr)
library(broom)

site_r2 <- comparison_1 %>%
  group_by(Site_ID) %>%
  do(model = lm(Mean5sec_Depth ~ Depth_At_Sensor_cm, data = .)) %>%
  mutate(
    r2 = summary(model)$r.squared
  ) %>%
  select(Site_ID, r2)

site_r2
  
  
  
  
  
  
  
  #










  
