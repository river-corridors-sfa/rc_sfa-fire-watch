# ============================= Objectives =========================================
#
# Test r package: tsrobprep with some raw EXO Post Retreat Fire (PRT) data 
# 
# Status: in progress
#
# Review status: 
#
# Notes: 
#
# ============================= Authorship ========================================
#
# Author: Jake Cavaiani
# 18 September 2025
# 

# ============================== Libraries ========================================

library(pacman)
p_load(tidyverse,
       lubridate,
       googlesheets4,
       gsheet, 
       glmnet, 
       MASS, 
       Matrix, 
       mclust,
       quantreg,
       Rdpack, 
       splines, 
       textTinyR, 
       zoo, 
       tsrobprep)

# ================================= User inputs ==========================
# Read in time series data 
test <- read_csv("Retreat_Fire_2024/inputs/EXO/local_test_donotuse.csv", 
                                skip = 8) %>% 
  janitor::clean_names() %>% 
  mutate(DateTime_PST = as_datetime(paste(mdy(date_mm_dd_yyyy), time_hh_mm_ss)), .before = everything())


# plot temperature:
ggplot(test, aes(x = DateTime_PST, y = temp_c)) +
  geom_point() +
  theme_bw()

# temperature
temp <- test %>% 
  dplyr::select(DateTime_PST, temp_c)

# Aggregate the burst measurements
temp_aggregate <- temp %>% 
  mutate(datetime_15min = floor_date(DateTime_PST, unit = "15 minutes")) %>% 
  group_by(datetime_15min) %>%
  summarize(across(c(temp_c),
                   list(mean = ~ signif(mean(.x, na.rm = TRUE), 3))))

df_with_na <- temp_aggregate %>%
  mutate(temp_c_mean = if_else(row_number() %in% sample(n(), size = 1000), NA_real_, temp_c_mean)) 


auto_clean <- auto_data_cleaning(
  data = df_with_na$temp_c_mean, # Water Temperature data
  S = 96, # 96 intervals for daily seasonality (96 rows of 15 min data = 1 day of data)
  tau = NULL, # Performs lasso to determine tau
  no.of.last.indices.to.fix = nrow(df_with_na), # Fix all data points
  indices.to.fix = NULL, # Automatically fix indices
  detect.outliers.pars = list(method = c("IQR"),  # Use IQR for outlier detection
                              threshold = c(1.5)  # Common threshold for IQR
  ))

auto_clean <- auto_data_cleaning(
  data = df_with_na$temp_c_mean, # Water Temperature data
  S = 96, # Daily seasonality (96 intervals of 15-min data)
  tau = NULL, # Use lasso regression for auto tau determination
  no.of.last.indices.to.fix = nrow(df_with_na), # Fix all missing values
  indices.to.fix = NULL, # Automatically detect and fix indices
  detect.outliers.pars = list(
    method = c("IQR", "MAD"),  # Combine IQR and Median Absolute Deviation
    threshold = c(1.5, 3)     # IQR threshold 1.5, MAD threshold 3
  )
)


clean_temp <- df_with_na %>%
  add_column(Cleaned_temp = auto_clean$clean.data[,1, drop = T])

ggplot() +
  geom_line(data = clean_temp, aes(x = datetime_15min, y = Cleaned_temp), color = "red", size = 0.5) +
  geom_line(data = clean_temp, aes(x = datetime_15min, y = temp_c_mean), color = "black")


# CHAT GPT #
auto_clean <- auto_data_cleaning(
  data = df_with_na$temp_c_mean, # Water Temperature data
  S = 672, # 672 intervals for weekly seasonality
  tau = NULL, # Tuned tau for more stable trend adaptation
  no.of.last.indices.to.fix = nrow(df_with_na),
  indices.to.fix = NULL, # Automatically detect and fix indices
  detect.outliers.pars = list(
    method = c("MAD"),  # Use Median Absolute Deviation for robust outlier detection
    threshold = c(3)    # Aggressive correction for significant outliers
  )
)

View(auto_clean$clean.data)

clean_temp <- df_with_na %>%
  add_column(Cleaned_temp = auto_clean$clean.data[,1, drop = T])

ggplot() +
  geom_line(data = clean_temp, aes(x = datetime_15min, y = Cleaned_temp), color = "red", size = 0.5) +
  geom_line(data = clean_temp, aes(x = datetime_15min, y = temp_c_mean), color = "black")


# NA approximation 
df_with_na$temp_c_mean_na_approx <- na.approx(df_with_na$temp_c_mean, maxgap = Inf)

clean_temp_na_approx <- clean_temp %>% 
  add_column(na_approx = df_with_na$temp_c_mean_na_approx)

ggplot() +
  geom_line(data = clean_temp_na_approx, aes(x = datetime_15min, y = Cleaned_temp), color = "red", size = 0.5) +
  geom_line(data = clean_temp_na_approx, aes(x = datetime_15min, y = na_approx), color = "green", size = 0.5) +
  geom_line(data = clean_temp_na_approx, aes(x = datetime_15min, y = temp_c_mean), color = "black")



