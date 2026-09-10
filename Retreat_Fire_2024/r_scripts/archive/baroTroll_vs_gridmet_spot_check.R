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
p_load(readr,
       lubridate,
       tidyverse,
       ggplot2,
       patchwork)


# ------  STEP 1: Read in data ------------------------------------------
# Spot check barotroll air temperature to met data air temps below 0
gridmet <- read_csv("~/GitHub/rcsfa-ST1B-PRT-patterns_n_stuff/outputs_for_analysis/gridMET_helper/gridmet_precip_2024-10-01_to_2026-08-10.csv")

baro_dir <- "~/OneDrive - PNNL/Documents - RC-SFA/Study_PRT/BaroTROLL/03_ProcessedData_L2"

files <- list.files(baro_dir, pattern = "^L2_PRT_Air_.*_Barotroll\\.csv$",
                    full.names = TRUE)

# Extract site ID from filename (e.g. "M02" from "L2_PRT_Air_M02_2024_Barotroll.csv")
baro_all <- map_dfr(files, function(f) {
  site <- str_match(basename(f), "L2_PRT_Air_([^_]+)_")[, 2]
  read_csv(f, show_col_types = FALSE) %>%
    mutate(Site_ID = site)
})

sites <- sort(unique(baro_all$Site_ID))
cat("Sites found:", paste(sites, collapse = ", "), "\n")

# ------ STEP 2. Aggregate 15-min Barotroll data to daily means for M01 ----
baro_daily <- baro_all %>%
  mutate(Air_Temperature = ifelse(Air_Temperature <= -9999, NA, Air_Temperature),
         Date = as.Date(DateTime)) %>%
  group_by(Site_ID, Date) %>%
  summarise(Tmean_baro = mean(Air_Temperature, na.rm = TRUE),
            n_obs = sum(!is.na(Air_Temperature)),
            .groups = "drop") %>%
  filter(n_obs >= 48)

# --- STEP 3. Join with gridMET for M02 ---
comb_all <- baro_daily %>%
  inner_join(gridmet %>% select(Site_ID, Date, Tmean_gridmet = Tmean_C),
             by = c("Site_ID", "Date"))

# --- 3. Time series overlay ---
p_ts <- ggplot(comb, aes(x = Date)) +
  geom_line(aes(y = Tmean_gridmet, color = "gridMET"), linewidth = 0.6) +
  geom_line(aes(y = Tmean_baro,    color = "Barotroll (M02)"), linewidth = 0.6) +
  scale_color_manual(values = c("gridMET" = "steelblue",
                                "Barotroll (M02)" = "firebrick")) +
  labs(title = "Daily Mean Air Temperature: gridMET vs Barotroll (M01)",
       y = "Temperature (°C)", x = NULL, color = NULL) +
  theme_bw()

print(p_ts)

# --- 4. 1:1 plot helper with regression ---
plot_1to1 <- function(df, title) {
  if (nrow(df) < 3) {
    return(ggplot() + labs(title = paste(title, "(insufficient data)")) + theme_bw())
  }
  m <- lm(Tmean_baro ~ Tmean_gridmet, data = df)
  r2 <- summary(m)$r.squared
  b0 <- coef(m)[1]; b1 <- coef(m)[2]
  rmse <- sqrt(mean(residuals(m)^2))
  lab <- sprintf("y = %.2f + %.2fx\nR² = %.3f\nRMSE = %.2f\nn = %d",
                 b0, b1, r2, rmse, nrow(df))
  rng <- range(c(df$Tmean_gridmet, df$Tmean_baro), na.rm = TRUE)
  
  ggplot(df, aes(Tmean_gridmet, Tmean_baro)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE, color = "firebrick") +
    coord_equal(xlim = rng, ylim = rng) +
    annotate("text", x = rng[1], y = rng[2], hjust = 0, vjust = 1,
             label = lab, size = 3) +
    labs(title = title, x = "gridMET Tmean (°C)", y = "Barotroll Tmean (°C)") +
    theme_bw()
}

make_site_plots <- function(site_id, df) {
  p_ts <- ggplot(df, aes(x = Date)) +
    geom_line(aes(y = Tmean_gridmet, color = "gridMET"), linewidth = 0.5) +
    geom_line(aes(y = Tmean_baro,    color = "Barotroll"), linewidth = 0.5) +
    scale_color_manual(values = c("gridMET" = "steelblue",
                                  "Barotroll" = "firebrick")) +
    labs(title = paste0(site_id, ": Daily Mean Air Temperature"),
         y = "Temperature (°C)", x = NULL, color = NULL) +
    theme_bw()
  
  p_all <- plot_1to1(df, "All days")
  p_neg <- plot_1to1(df %>% filter(Tmean_gridmet < 0 | Tmean_baro < 0),
                     "(-) temp days")
  p_pos <- plot_1to1(df %>% filter(Tmean_gridmet >= 0 & Tmean_baro >= 0),
                     "(+) temp days")
  
  p_ts / (p_all + p_neg + p_pos) +
    plot_annotation(title = paste("Site", site_id))
}

# --- 5. Run for every site ---------------------------------------------------
site_plots <- comb_all %>%
  group_split(Site_ID) %>%
  set_names(map_chr(., ~ unique(.x$Site_ID))) %>%
  imap(~ make_site_plots(.y, .x))

# View interactively:
site_plots[["M01"]]
site_plots[["M02"]]
site_plots[["M03"]]
site_plots[["NF01"]]
site_plots[["SF01"]]


# Or save all to disk:
# dir.create(out_dir, showWarnings = FALSE)
# iwalk(site_plots, ~ ggsave(file.path(out_dir, paste0(.y, "_gridmet_vs_barotroll.png")),
#                            .x, width = 12, height = 8, dpi = 150))

# --- 6. Summary stats table --------------------------------------------------
summary_stats <- comb_all %>%
  mutate(subset = "all") %>%
  bind_rows(comb_all %>% filter(Tmean_gridmet < 0 | Tmean_baro < 0) %>% mutate(subset = "negative"),
            comb_all %>% filter(Tmean_gridmet >= 0 & Tmean_baro >= 0) %>% mutate(subset = "positive")) %>%
  group_by(Site_ID, subset) %>%
  summarise(n = n(),
            r2   = ifelse(n >= 3, summary(lm(Tmean_baro ~ Tmean_gridmet))$r.squared, NA),
            bias = mean(Tmean_baro - Tmean_gridmet, na.rm = TRUE),
            rmse = sqrt(mean((Tmean_baro - Tmean_gridmet)^2, na.rm = TRUE)),
            .groups = "drop")

# --- Combined "all sites" 1:1 plots -----------------------------------------
plot_1to1_bysite <- function(df, title) {
  if (nrow(df) < 3) {
    return(ggplot() + labs(title = paste(title, "(insufficient data)")) + theme_bw())
  }
  m <- lm(Tmean_baro ~ Tmean_gridmet, data = df)
  r2 <- summary(m)$r.squared
  b0 <- coef(m)[1]; b1 <- coef(m)[2]
  rmse <- sqrt(mean(residuals(m)^2))
  lab <- sprintf("y = %.2f + %.2fx\nR² = %.3f\nRMSE = %.2f\nn = %d",
                 b0, b1, r2, rmse, nrow(df))
  rng <- range(c(df$Tmean_gridmet, df$Tmean_baro), na.rm = TRUE)
  
  ggplot(df, aes(Tmean_gridmet, Tmean_baro)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
    geom_point(aes(color = Site_ID), alpha = 0.5) +
    geom_smooth(method = "lm", se = TRUE, color = "black") +
    coord_equal(xlim = rng, ylim = rng) +
    annotate("text", x = rng[1], y = rng[2], hjust = 0, vjust = 1,
             label = lab, size = 3) +
    labs(title = title, x = "gridMET Tmean (°C)", y = "Barotroll Tmean (°C)",
         color = "Site") +
    theme_bw()
}

p_all_sites <- plot_1to1_bysite(comb_all, "All days")
p_neg_sites <- plot_1to1_bysite(
  comb_all %>% filter(Tmean_gridmet < 0 | Tmean_baro < 0),
  "(-) temp")
p_pos_sites <- plot_1to1_bysite(
  comb_all %>% filter(Tmean_gridmet >= 0 & Tmean_baro >= 0),
  "(+) temp")

combined_plot <- (p_all_sites + p_neg_sites + p_pos_sites) +
  plot_annotation(title = "gridMET vs Barotroll — All Sites Combined")

print(combined_plot)

out_dir <- "~/GitHub/rc_sfa-fire-watch/Retreat_Fire_2024/r_scripts/archive"
ggsave(file.path(out_dir, "ALL_SITES_gridmet_vs_barotroll.png"),
       combined_plot, width = 15, height = 6, dpi = 150)


