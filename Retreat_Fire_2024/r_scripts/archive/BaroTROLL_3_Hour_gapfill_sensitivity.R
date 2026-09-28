# =================================== Objectives =================================
#
# 3 hour gap fill sensitivity for barotroll
# Sensitivity analysis: 3-hour gap punch + linear interpolation
# Compare gap-filled vs original per site (BaroTROLL Air data)

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
p_load(dplyr,
       tidyr,
       readr,
       lubridate,
       zoo,
       ggplot2, 
       stringr)

# ---- STEP 1: Setup ------------------------------------------------------------------
data_dir <- "/Users/cava304/Library/CloudStorage/OneDrive-PNNL/Documents - RC-SFA/Study_PRT/BaroTROLL/03_ProcessedData_L1/"
out_dir  <- file.path(data_dir, "GapFill_Sensitivity")
dir.create(out_dir, showWarnings = FALSE)

set.seed(42)
gap_hours   <- 3     # size of each hole
n_holes     <- 20    # number of random holes per site
value_cols  <- NULL  # NULL = auto-detect numeric columns

# ---- STEP 2: Read one file ----------------------------------------------------------
read_one <- function(f) {
  site <- str_match(basename(f), "L1_PRT_Air_([^_]+)_")[,2]
  df <- read_csv(f, show_col_types = FALSE)
  
  candidates <- c("DateTime", "Datetime", "datetime", "TIMESTAMP",
                  "Timestamp", "timestamp", "Date_Time", "date_time", "Time")
  dt_col <- intersect(candidates, names(df))[1]
  
  if (is.na(dt_col)) {
    parse_try <- sapply(df, function(x) {
      if (inherits(x, "POSIXct")) return(sum(!is.na(x)))
      if (!is.character(x)) return(0L)
      sum(!is.na(parse_date_time(x,
                                 orders = c("ymd HMS","ymd HM","mdy HMS","mdy HM","dmy HMS","dmy HM"),
                                 quiet = TRUE)))
    })
    if (max(parse_try) == 0) stop("No parseable datetime column in ", basename(f))
    dt_col <- names(df)[which.max(parse_try)]
  }
  
  if (inherits(df[[dt_col]], "POSIXct")) {
    df$DateTime <- df[[dt_col]]
  } else {
    df$DateTime <- parse_date_time(df[[dt_col]],
                                   orders = c("ymd HMS","ymd HM","mdy HMS","mdy HM","dmy HMS","dmy HM"),
                                   quiet = TRUE)
  }
  
  n_bad <- sum(is.na(df$DateTime))
  if (n_bad > 0) message(basename(f), ": ", n_bad, " unparseable timestamps dropped")
  df <- df[!is.na(df$DateTime), ]
  
  df$Site <- site
  df
}

# ---- STEP 3: Load all files ---------------------------------------------------------
files <- list.files(data_dir, pattern = "^L1_PRT_Air_.*_Barotroll\\.csv$",
                    full.names = TRUE)
if (length(files) == 0) stop("No matching files found in ", data_dir)

all_data <- bind_rows(lapply(files, read_one)) %>%
  filter(!is.na(DateTime)) %>%
  arrange(Site, DateTime) %>%
  distinct(Site, DateTime, .keep_all = TRUE)

# ---- Diagnostic -------------------------------------------------------------
diag <- all_data %>%
  group_by(Site) %>%
  summarise(n_rows = n(),
            min_dt = suppressWarnings(min(DateTime, na.rm = TRUE)),
            max_dt = suppressWarnings(max(DateTime, na.rm = TRUE)),
            .groups = "drop")
print(diag)

# ---- Detect numeric columns -------------------------------------------------
if (is.null(value_cols)) {
  value_cols <- all_data %>%
    select(-DateTime, -Site) %>%
    select(where(is.numeric)) %>%
    names()
}
message("Testing variables: ", paste(value_cols, collapse = ", "))

# ---- STEP 4: Gap-fill test per site -------------------------------------------------
gap_fill_test <- function(df_site, site_name) {
  
  df_site <- df_site %>% filter(!is.na(DateTime))
  if (nrow(df_site) < 10) {
    message("Skipping ", site_name, ": too few valid rows")
    return(list(stats = NULL, wide = NULL))
  }
  
  dt <- df_site$DateTime
  step_min <- as.numeric(median(diff(dt), na.rm = TRUE), units = "mins")
  if (is.na(step_min) || step_min <= 0) step_min <- 15
  
  full_seq <- seq(min(dt), max(dt), by = paste(step_min, "min"))
  df_site <- df_site %>%
    complete(DateTime = full_seq) %>%
    mutate(Site = site_name) %>%
    arrange(DateTime)
  
  n_pts_per_hole <- round((gap_hours * 60) / step_min)
  n <- nrow(df_site)
  if (n <= n_pts_per_hole * 2) {
    message("Skipping ", site_name, ": series too short for ", gap_hours, "-h holes")
    return(list(stats = NULL, wide = NULL))
  }
  
  # Non-overlapping random hole start indices
  possible_starts <- sample(1:(n - n_pts_per_hole), min(n_holes * 10, n - n_pts_per_hole))
  chosen <- integer(0)
  for (s in possible_starts) {
    if (length(chosen) >= n_holes) break
    if (all(abs(chosen - s) > n_pts_per_hole * 2)) chosen <- c(chosen, s)
  }
  chosen <- sort(chosen)
  
  # Index of all hole-punched rows, labeled by hole ID
  hole_idx <- unlist(lapply(seq_along(chosen), function(i) {
    data.frame(row = chosen[i]:(chosen[i] + n_pts_per_hole - 1),
               Hole_ID = i)
  }) %>% { do.call(rbind, .) } %>% list()) # coerce
  # cleaner:
  hole_idx <- do.call(rbind, lapply(seq_along(chosen), function(i)
    data.frame(row = chosen[i]:(chosen[i] + n_pts_per_hole - 1), Hole_ID = i)))
  
  # Wide comparison frame for this site
  wide_df <- df_site[hole_idx$row, "DateTime", drop = FALSE]
  wide_df$Site    <- site_name
  wide_df$Hole_ID <- hole_idx$Hole_ID
  
  results <- list()
  for (v in value_cols) {
    if (!v %in% names(df_site)) next
    orig <- df_site[[v]]
    if (!is.numeric(orig)) next
    
    punched <- orig
    for (s in chosen) punched[s:(s + n_pts_per_hole - 1)] <- NA
    
    filled <- tryCatch(
      na.approx(punched, x = df_site$DateTime, na.rm = FALSE),
      error = function(e) rep(NA_real_, length(punched))
    )
    
    # Add obs/pred columns to wide frame
    wide_df[[paste0(v, "_obs")]]  <- orig[hole_idx$row]
    wide_df[[paste0(v, "_pred")]] <- filled[hole_idx$row]
    wide_df[[paste0(v, "_resid")]] <- filled[hole_idx$row] - orig[hole_idx$row]
    
    # Stats
    o <- orig[hole_idx$row]
    p <- filled[hole_idx$row]
    keep <- !is.na(o) & !is.na(p)
    o <- o[keep]; p <- p[keep]
    if (length(o) < 3 || sd(o) == 0) next
    
    r2   <- cor(o, p)^2
    rmse <- sqrt(mean((o - p)^2))
    mae  <- mean(abs(o - p))
    bias <- mean(p - o)
    
    results[[v]] <- data.frame(
      Site = site_name, Variable = v, N = length(o),
      R2 = r2, RMSE = rmse, MAE = mae, Bias = bias,
      Gap_hours = gap_hours, N_holes = length(chosen),
      Step_min = step_min
    )
    
    # Plot
    pdf(file.path(out_dir, sprintf("%s_%s_obs_vs_pred.pdf", site_name, v)),
        width = 6, height = 6)
    plot(o, p, pch = 16, col = rgb(0, 0, 0, 0.4),
         xlab = "Observed", ylab = "Linearly gap-filled",
         main = sprintf("%s | %s\nR² = %.3f | RMSE = %.3f",
                        site_name, v, r2, rmse))
    abline(0, 1, col = "red", lwd = 2)
    dev.off()
  }
  
  list(stats = bind_rows(results),
       wide  = wide_df %>% arrange(DateTime))
}

# ---- STEP 5: Run per site -----------------------------------------------------------
site_results <- all_data %>%
  group_split(Site) %>%
  lapply(function(x) gap_fill_test(x, unique(x$Site)))

summary_stats <- bind_rows(lapply(site_results, `[[`, "stats"))
holes_wide    <- bind_rows(lapply(site_results, `[[`, "wide"))

print(summary_stats)

# ---- STEP 6: Write outputs ----------------------------------------------------------
write_csv(summary_stats, file.path(out_dir, "GapFill_Sensitivity_Summary.csv"))
write_csv(holes_wide,    file.path(out_dir, "GapFill_HolePunch_ObsVsPred_wide.csv"))

# Also per-site wide files (easier to inspect)
for (s in unique(holes_wide$Site)) {
  write_csv(filter(holes_wide, Site == s),
            file.path(out_dir, sprintf("HolePunch_ObsVsPred_%s.csv", s)))
}

# ---- STEP 7: Overall summary plot ---------------------------------------------------
if (nrow(summary_stats) > 0) {
  ggplot(summary_stats, aes(Site, R2, fill = Variable)) +
    geom_col(position = "dodge") +
    geom_hline(yintercept = 0.9, linetype = "dashed") +
    ylim(0, 1) +
    labs(title = sprintf("Linear gap-fill R² (%d-hour gaps)", gap_hours),
         y = expression(R^2)) +
    theme_bw()
  ggsave(file.path(out_dir, "R2_summary_bysite.pdf"), width = 8, height = 5)
}

message("Done. Outputs in: ", out_dir)
