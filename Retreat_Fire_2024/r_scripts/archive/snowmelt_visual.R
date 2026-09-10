# =================================== Objectives =================================
#
# Visual inspection of snowmelt data  

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
p_load(tidyverse,
       lubridate,
       scales, 
       fs,
       data.table,
       gsheet,
       fuzzyjoin,
       dataRetrieval,
       viridis,
       pracma)


# ================================= Step 1: Read in data ================================
# DO:
# DO_Q <- read_csv("~/GitHub/rcsfa-ST1B-PRT-patterns_n_stuff/outputs_for_analysis/missing_data_output/param_DO_mgL.csv")
# 
# scaling_factor <- 5
# ggplot() +
#   geom_point(data = DO_Q, aes(x = DateTime, y = DO_mgL, colour = "DO (mg/L)"), size = 0.2) +
#   geom_line(data = DO_Q, aes(x = DateTime, y = q_cfs / scaling_factor, colour = "Discharge (cfs)")) +
#   scale_y_continuous(
#     name = "Dissolved Oxygen (mg/L)",
#     sec.axis = sec_axis(~ . * scaling_factor, name = "Discharge (cfs)")
#   ) +
#   scale_color_manual(values = c("DO (mg/L)" = "red", "Discharge (cfs)" = "black")) +
#   
#   facet_wrap(~dataset, scales = "free") +
#   theme_bw() +
#   labs(colour = "Variables") +
#   theme(legend.position = "bottom")

# ============================ Parameter missing PERIODS ===========================
# ---- 1. Define parameters and file paths ----
params <- c("DO_mgL", "fdom_qse", "pH", "spc_us_cm", "stream_temp", "turb_fnu")

base_path <- "~/GitHub/rcsfa-ST1B-PRT-patterns_n_stuff/outputs_for_analysis/missing_data_output/"

# ---- 2. Flush window definitions ----
assign_flush <- function(date) {
  case_when(
    date >= as.Date("2025-02-23") & date <  as.Date("2025-03-23") ~ "2025_1st_flush",
    date >= as.Date("2025-03-23") & date <= as.Date("2025-05-10") ~ "2025_2nd_flush",
    date >= as.Date("2025-12-08") & date <  as.Date("2026-03-06") ~ "2026_1st_flush",
    date >= as.Date("2026-03-06") & date <= as.Date("2026-04-09") ~ "2026_2nd_flush",
    TRUE ~ NA_character_
  )
}

# ---- 3. Function: read, compute daily missing + flush summary ----
process_param <- function(param) {
  file <- paste0(base_path, "param_", param, ".csv")
  df   <- read_csv(file, show_col_types = FALSE)
  
  # find the value column (everything except dataset/DateTime/q_cfs)
  value_col <- setdiff(names(df), c("dataset", "DateTime", "q_cfs"))
  
  df %>%
    mutate(date = as.Date(DateTime),
           flush = assign_flush(date)) %>%
    filter(!is.na(flush)) %>%
    group_by(flush, date) %>%
    summarise(
      observed    = sum(!is.na(.data[[value_col]])),
      pct_missing = (96 - observed) / 96 * 100,
      .groups = "drop"
    ) %>%
    group_by(flush) %>%
    summarise(
      parameter            = param,
      total_days           = n(),
      days_over_50_missing = sum(pct_missing > 50),
      pct_days_bad         = days_over_50_missing / total_days * 100,
      .groups = "drop"
    ) %>%
    relocate(parameter)
}

# ---- 4. Run across all parameters ----
flush_summary_all <- map_dfr(params, process_param)
flush_summary_all

# ---- 5. Optional: wide view (parameters as columns) ----
flush_summary_wide <- flush_summary_all %>%
  select(parameter, flush, days_over_50_missing) %>%
  pivot_wider(names_from = parameter, values_from = days_over_50_missing)
flush_summary_wide



#
# ============================ COMPARE WITH PAIGE ===========================
snowmelt_2025_1 <- read_csv("~/GitHub/rcsfa-ST1B-PRT-patterns_n_stuff/outputs_for_analysis/missing_data_output/snowmelt_2025_1st_flush_daily_missing.csv")
snowmelt_2025_2 <- read_csv("~/GitHub/rcsfa-ST1B-PRT-patterns_n_stuff/outputs_for_analysis/missing_data_output/snowmelt_2025_2nd_flush_daily_missing.csv")
snowmelt_2026_1 <- read_csv("~/GitHub/rcsfa-ST1B-PRT-patterns_n_stuff/outputs_for_analysis/missing_data_output/snowmelt_2026_1st_flush_daily_missing.csv")

#







