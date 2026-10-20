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
# https://www.swordexplorer.com/

# ============================= Authorship ===========================
# Author: Jake Cavaiani
# 4 September 2026

# ---- Install/Load packages ----
rm(list = ls(all = TRUE))

library(pacman)
p_load(ncdf4,
       dplyr,
       ggplot2,
       RNetCDF,
       nwmTools)

results_file <- "~/Downloads/na_sword_v16_SOS_results_unconstrained_20230502T204408_20250502T204408_20251219T163700.nc"
nc_res <- open.nc(results_file)

# Reach metadata ####
reaches_res <- grp.inq.nc(nc_res, "reaches")$self
reach_id    <- as.character(var.get.nc(reaches_res, "reach_id"))
river_name  <- as.character(var.get.nc(reaches_res, "river_name"))
lon         <- as.numeric(var.get.nc(reaches_res, "x"))
lat         <- as.numeric(var.get.nc(reaches_res, "y"))

reaches_df <- data.frame(reach_id, river_name, lon, lat,
                         stringsAsFactors = FALSE)

# Find Yakima reaches ####
yakima_prefix <- "78261200"
yakima_df <- reaches_df %>%
  filter(startsWith(reach_id, yakima_prefix) |
           grepl("Yakima", river_name, ignore.case = TRUE))
print(yakima_df)
yakima_idx <- match(yakima_df$reach_id, reach_id)

# Load VLEN time + discharge (both are lists, one entry/reach) ####
time_vlen <- var.get.nc(reaches_res, "time")        # list, length = num_reaches

consensus_grp <- grp.inq.nc(nc_res, "consensus")$self
q_vlen <- var.get.nc(consensus_grp, "consensus_q")  # likely also a list

# Fill value flagged in attributes; also treat large negatives as NA
FILL <- -999999999999

clean_vec <- function(x) {
  x <- as.numeric(x)
  x[x <= FILL / 2] <- NA   # catches -1e12 fill
  x
}

# Build long-format discharge data frame ####
build_reach_ts <- function(i) {
  idx <- yakima_idx[i]
  t_raw <- time_vlen[[idx]]
  # Some reaches have a single scalar (-1e12) meaning no observations
  if (length(t_raw) < 2 && all(t_raw <= FILL / 2)) return(NULL)
  
  tsec <- clean_vec(t_raw)
  
  # Q may be list (vlen) or matrix depending on file
  q_raw <- if (is.list(q_vlen)) q_vlen[[idx]] else q_vlen[idx, ]
  qv    <- clean_vec(q_raw)
  
  # Pad in case lengths differ (shouldn't, but safety)
  n <- min(length(tsec), length(qv))
  
  data.frame(
    reach_id = yakima_df$reach_id[i],
    river    = yakima_df$river_name[i],
    date     = as.POSIXct(tsec[seq_len(n)], origin = "2000-01-01", tz = "UTC"),
    Q        = qv[seq_len(n)]
  )
}

ts_all <- bind_rows(lapply(seq_len(nrow(yakima_df)), build_reach_ts)) %>%
  filter(!is.na(Q), !is.na(date), Q > 0)

head(ts_all)
range(ts_all$date)

# Summary + plots ####
reach_summary <- ts_all %>%
  group_by(reach_id) %>%
  summarise(n_obs  = n(),
            Q_mean = mean(Q),
            Q_med  = median(Q),
            Q_min  = min(Q),
            Q_max  = max(Q),
            .groups = "drop")
print(reach_summary)

ggplot(ts_all, aes(date, Q)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 1) +
  facet_wrap(~ reach_id, scales = "free_y") +
  labs(title = "Yakima River SWOT discharge (consensus)",
       x = NULL, y = expression("Q (m"^3*"/s)")) +
  theme_minimal()

# write.csv(ts_all, "yakima_swot_discharge.csv", row.names = FALSE)
# 
# close.nc(nc_res)

# PLOT MAP OF YRB Q ####
# install.packages(c("sf", "ggrepel"))   # if needed
library(sf)
library(ggrepel)
library(rnaturalearth)

# 1. Pull node-level coords & their parent reach_id ####
nodes_grp     <- grp.inq.nc(nc_res, "nodes")$self
node_reach_id <- as.character(var.get.nc(nodes_grp, "reach_id"))
node_x        <- as.numeric(var.get.nc(nodes_grp, "x"))
node_y        <- as.numeric(var.get.nc(nodes_grp, "y"))

nodes_df <- data.frame(reach_id = node_reach_id,
                       lon = node_x, lat = node_y,
                       stringsAsFactors = FALSE)

# 2. Keep only Yakima reaches; flag which ones have Q ####
reaches_with_Q <- unique(ts_all$reach_id)

yakima_nodes <- nodes_df %>%
  filter(reach_id %in% yakima_df$reach_id) %>%
  mutate(has_Q = reach_id %in% reaches_with_Q)

# 3. Build one LINESTRING per reach from its ordered nodes ####
flowlines <- yakima_nodes %>%
  group_by(reach_id, has_Q) %>%
  arrange(.by_group = TRUE) %>%          # node order follows file order (upstream→down)
  summarise(geometry = st_sfc(st_linestring(cbind(lon, lat)), crs = 4326),
            .groups = "drop") %>%
  st_as_sf()

# 4. Reach centroids for labels + mean Q ####
reach_meanQ <- ts_all %>%
  group_by(reach_id) %>%
  summarise(Q_mean = mean(Q), n_obs = n(), .groups = "drop")

centroids <- yakima_df %>%
  left_join(reach_meanQ, by = "reach_id") %>%
  mutate(has_Q = reach_id %in% reaches_with_Q)

# 5. Basemap layers (Washington + neighbors + counties) ####
states   <- ne_states(country = "united states of america", returnclass = "sf")
wa       <- states[states$name == "Washington", ]
neighbors <- states[states$name %in% c("Oregon", "Idaho"), ]

# Zoom window around Yakima reaches
xlim <- range(yakima_df$lon) + c(-0.3, 0.3)
ylim <- range(yakima_df$lat) + c(-0.3, 0.3)

# Optional: major rivers of the US (for Columbia context)
rivers <- tryCatch(
  ne_download(scale = 10, type = "rivers_lake_centerlines",
              category = "physical", returnclass = "sf"),
  error = function(e) NULL
)

# 6. Combined map ####
p_map <- ggplot() +
  # basemap
  geom_sf(data = neighbors, fill = "grey97", color = "grey70", linewidth = 0.3) +
  geom_sf(data = wa,        fill = "grey92", color = "grey40", linewidth = 0.5) +
  { if (!is.null(rivers))
    geom_sf(data = rivers, color = "lightblue3", linewidth = 0.3) } +
  
  # Yakima flowlines
  geom_sf(data = flowlines,
          aes(color = has_Q, linewidth = has_Q)) +
  
  # Reach centroid points
  geom_point(data = centroids,
             aes(lon, lat, fill = has_Q),
             shape = 21, size = 2.5, color = "black") +
  
  # Labels only on reaches with Q
  geom_label_repel(data = filter(centroids, has_Q),
                   aes(lon, lat,
                       label = paste0(reach_id,
                                      "\n", round(Q_mean, 1), " m³/s",
                                      " (n=", n_obs, ")")),
                   size = 2.8, min.segment.length = 0, max.overlaps = 20,
                   box.padding = 0.4, label.size = 0.2,
                   fill = alpha("white", 0.8)) +
  
  # Styling
  scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey60"),
                     labels = c("TRUE" = "Valid SWOT Q",
                                "FALSE" = "No valid Q"),
                     name = NULL) +
  scale_fill_manual(values  = c("TRUE" = "steelblue", "FALSE" = "grey60"),
                    guide = "none") +
  scale_linewidth_manual(values = c("TRUE" = 1.3, "FALSE" = 0.6),
                         guide = "none") +
  
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(title    = "Yakima River — SWOT consensus discharge reaches",
       subtitle = "Washington State  |  v16 SoS unconstrained, May 2023 – May 2025",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "aliceblue", color = NA),
        panel.grid.major = element_line(color = "white", linewidth = 0.3),
        legend.position  = "bottom")

print(p_map)

#### NATIONAL WATER MODEL ################
# install.packages(c("arrow", "dplyr", "ggplot2", "lubridate"))
rm(list = ls(all = TRUE))

library(pacman)
p_load(arrow,
       dplyr,
       ggplot2,
       lubridate)

# 0. Verify arrow has S3 support ####
stopifnot(arrow_info()$capabilities["s3"])
# If this errors: 
#   Sys.setenv(LIBARROW_MINIMAL = "false", ARROW_S3 = "ON")
#   install.packages("arrow", type = "source"); restart R.

# 1. Oak Creek COMIDs ####
oak_comids <- tibble::tribble(
  ~site, ~comid,
  "M01",  24423655,
  "M02",  24423673,
  "M03",  24423679,
  "SF01", 24424753,
  "NF01", 24424703
)
print(oak_comids)

# 2. Open NWM v3.0 retrospective zarr on AWS ####
ds <- open_dataset(
  "s3://noaa-nwm-retrospective-3-0-pds/CONUS/zarr/chrtout.zarr",
  format = "zarr",
  s3_options = list(anonymous = TRUE)
)

# -----------------------------------------------------------
# 3. Filter to our 5 COMIDs for the time window of interest
# -----------------------------------------------------------
start_dt <- as.POSIXct("2010-01-01", tz = "UTC")
end_dt   <- as.POSIXct("2022-12-31", tz = "UTC")   # NWM v3.0 ends Jan 2023

oak_q <- ds %>%
  filter(feature_id %in% oak_comids$comid,
         time >= start_dt,
         time <= end_dt) %>%
  select(time, feature_id, streamflow) %>%
  collect() %>%
  rename(dateTime = time, Q_cms = streamflow) %>%
  left_join(oak_comids, by = c("feature_id" = "comid")) %>%
  mutate(site = factor(site, levels = c("NF01", "SF01", "M01", "M02", "M03")))

head(oak_q)

# -----------------------------------------------------------
# 4. Daily aggregation
# -----------------------------------------------------------
oak_daily <- oak_q %>%
  mutate(date = as.Date(dateTime)) %>%
  group_by(site, feature_id, date) %>%
  summarise(Q_cms = mean(Q_cms, na.rm = TRUE), .groups = "drop")

# -----------------------------------------------------------
# 5. Summary table
# -----------------------------------------------------------
oak_summary <- oak_daily %>%
  group_by(site, feature_id) %>%
  summarise(n_days = n(),
            Q_mean = mean(Q_cms),
            Q_med  = median(Q_cms),
            Q_min  = min(Q_cms),
            Q_max  = max(Q_cms),
            Q_p10  = quantile(Q_cms, 0.10),
            Q_p90  = quantile(Q_cms, 0.90),
            .groups = "drop")
print(oak_summary)

# -----------------------------------------------------------
# 6. Plots
# -----------------------------------------------------------
# 6A. Overlaid time series (log y helps because tribs are small)
ggplot(oak_daily, aes(date, Q_cms, color = site)) +
  geom_line(alpha = 0.8, linewidth = 0.3) +
  scale_y_log10() +
  labs(title = "Oak Creek — NWM v3.0 retrospective daily Q",
       x = NULL, y = expression("Q (m"^3*"/s, log)"),
       color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

# 6B. Faceted
ggplot(oak_daily, aes(date, Q_cms)) +
  geom_line(color = "steelblue", linewidth = 0.3) +
  facet_wrap(~ site, ncol = 1, scales = "free_y") +
  labs(title = "Oak Creek branches — NWM daily discharge",
       x = NULL, y = expression("Q (m"^3*"/s)")) +
  theme_minimal()

# 6C. Mean annual hydrograph (climatology) per site
oak_daily %>%
  mutate(doy = yday(date)) %>%
  group_by(site, doy) %>%
  summarise(Q_mean = mean(Q_cms),
            Q_p10  = quantile(Q_cms, 0.10),
            Q_p90  = quantile(Q_cms, 0.90),
            .groups = "drop") %>%
  ggplot(aes(doy, Q_mean)) +
  geom_ribbon(aes(ymin = Q_p10, ymax = Q_p90),
              fill = "steelblue", alpha = 0.25) +
  geom_line(color = "steelblue", linewidth = 0.7) +
  facet_wrap(~ site, scales = "free_y") +
  labs(title    = "Oak Creek climatology — NWM 2010–2022",
       subtitle = "Band = 10th–90th percentile across years",
       x = "Day of year", y = expression("Q (m"^3*"/s)")) +
  theme_minimal()

# -----------------------------------------------------------
# 7. Export
# -----------------------------------------------------------
write.csv(oak_daily, "oak_creek_nwm_daily.csv", row.names = FALSE)









# install.packages("remotes")
remotes::install_github("mikejohnson51/nwmHistoric")

library(nwmHistoric)
library(dplyr)
library(ggplot2)
library(lubridate)

oak_comids <- tibble::tribble(
  ~site,  ~comid,
  "M01",  24423655,
  "M02",  24423673,
  "M03",  24423679,
  "SF01", 24424753,
  "NF01", 24424703
)

# Pull all 5 sites (NWM v2.1 retrospective: 1979–2020, hourly)
oak_q <- bind_rows(lapply(seq_len(nrow(oak_comids)), function(i) {
  message("Pulling ", oak_comids$site[i], " (", oak_comids$comid[i], ")")
  d <- readNWMdata(comid     = oak_comids$comid[i],
                   startDate = "2010-01-01",
                   endDate   = "2018-12-31")
  d$site <- oak_comids$site[i]
  d
}))

head(oak_q)
str(oak_q)
