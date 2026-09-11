# =================================== Objectives =================================
#
# When do we go to the field and should we target specific dates moving forward

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
# install.packages("devtools")
# library(devtools)
# devtools::install_github("DOI-USGS/streamMetabolizer")

library(pacman)
p_load(readr,
       lubridate,
       tidyverse,
       ggplot2,
       patchwork)


# ------  STEP 1: Read in USGS data ------------------------------------------
M01_water_quality_discharge <- read_csv("~/GitHub/rcsfa-ST1B-PRT-patterns_n_stuff/outputs_for_analysis/01_get_usgs_data/M01_water_quality_discharge.csv")

# separate out Q and mutate a year column
Q <- M01_water_quality_discharge |> 
  select(DateTime, Discharge) |> 
  mutate(year = year(DateTime))

# ------  STEP 2: source vistation data ------------------------------------------
source('~/GitHub/rcsfa-sensor-processing/Study_Specific_Sensor_Processing/PRT_pull_deployment_windows.R')

# Pull in exo start times which is indicative of when we've visited the field
in_water_intervals <- pull_deployment_windows()

in_water_intervals <- in_water_intervals %>%
  add_row(
    Site_ID = "M02",
    exo_start = as.POSIXct("2026-08-12 13:00:00", tz = "UTC"),
    casing_angle_at_start_deployment = NA_real_,
    Latitude = NA_real_,
    Longitude = NA_real_,
    exo_end = as.POSIXct(NA)
  ) |> 
  add_row(
    Site_ID = "M02",
    exo_start = as.POSIXct("2026-04-29 13:00:00", tz = "UTC"),
    casing_angle_at_start_deployment = NA_real_,
    Latitude = NA_real_,
    Longitude = NA_real_,
    exo_end = as.POSIXct(NA)
  )



# ------  STEP 3: Plot ------------------------------------------
ggplot(data = Q, aes(x = DateTime, y = Discharge)) +
  geom_point() +
  facet_wrap(~year, nrow = 3, ncol = 1) +
  theme_bw()

# transform Q
Q_plot <- Q %>%
  mutate(doy = as.POSIXct(format(DateTime, "2000-%m-%d %H:%M:%S")))

# transform vline data — need a `year` column matching facets
vlines <- in_water_intervals %>%
  mutate(
    year = as.numeric(format(exo_start, "%Y")),
    doy  = as.POSIXct(format(exo_start, "2000-%m-%d %H:%M:%S"))
  )

ggplot(Q_plot, aes(doy, Discharge)) +
  geom_line() +
  geom_vline(data = vlines, aes(xintercept = doy),
             color = "red", linetype = "dashed", linewidth = 0.6) +
  facet_wrap(~ year, ncol = 1) +
  scale_x_datetime(
    limits = as.POSIXct(c("2000-01-01", "2000-12-31")),
    date_breaks = "1 month", date_labels = "%b"
  ) +
  labs(x = NULL, y = "Discharge") +
  theme_bw()

#

























