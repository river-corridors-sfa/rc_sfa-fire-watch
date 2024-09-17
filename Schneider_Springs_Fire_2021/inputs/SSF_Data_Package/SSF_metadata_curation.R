# Prepare SSF Metadata
# 2023-10-04 to 2023-10-06
# Bibi Powers-McCormack


# Curation Notes
  # ssf_metadata_df_01 = fixes coordinate issues
  # ssf_metadata_df_02 = removes unnecessary columns
  # ssf_metadata_df_03 = renames columns
  # ssf_metadata_df_04 = adds in new columns from SSS
  # ssf_metadata_df_05 = reorders columns
  # ssf_metadata_df_06 = adds in new burned status column
  # inputs have a value assignment that ends with "_filepath" - there are 4 input files in this script
  # all other dfs temporary helper dfs
  # this script does all of the metadata manipulations except for the copy editing (for clarity, date spaces, and commas) which was done after exporting
      # the column "Site_Burned" was renamed to "Site_Affected_By_Fire" after exporting as well

# Code in this script that may be helpful for other circumstances
  # the code used to asses map coordinates
  # the final checks at the end


#### Prep ####
library(tidyverse)
library(easycsv)
library(sf)
library(mapview)
library(readxl)


#### >> Compare coordinates against geospatial ####

# load data: ssf metadata, geospatial ----
# the metadata was pulled from `Z:\RC3\00_Schneider_Springs_Fire_2023\00_Metadata`
ssf_metadata_filepath <- "C:\\Users\\powe419\\OneDrive - PNNL\\Documents - Core Richland and Sequim Lab-Field Team\\Data Generation and Files\\RC3\\05_Editable_ESS-DIVE_Format\\SSF_Data_Package\\SSF_Deploy_Sample_Retrieve_Metadata_2023-09-29.csv"
ssf_metadata_filepath <- file.choose() # select `SSF_Deploy_Sample_Retrieve_Metadata_2023-09-29.csv`
ssf_metadata <- read_csv(ssf_metadata_filepath)

geospatial_metadata_filepath <- "Z:\\00_Cross-SFA_ESSDIVE-Data-Package-Upload\\01_Study-Data-Package-Folders\\00_ARCHIVE-WHEN-PUBLISHED\\Cross-RCSFA_Geospatial_Data_Package_v2\\v2_RCSFA_Geospatial_Data_Package\\v2_RCSFA_Geospatial_Site_Information.csv"
geospatial_metadata_filepath <- file.choose() # select from the most recent geospatial dp
geospatial_metadata <- read_csv(geospatial_metadata_filepath)

# filter for only coordinate columns in SSF
ssf_coords_df_01 <- ssf_metadata %>% 
  select(Site_ID, Deploy_Latitude, Deploy_Longitude, Sample_Latitude, Sample_Longitude)

# filter for coordinates in geospatial for sites included in SSF
geospatial_coords_df_01 <-geospatial_metadata %>% 
  select(Site_ID, Latitude, Longitude) %>% 
  filter(Site_ID %in% ssf_coords_df_01$Site_ID) %>% 
  rename(geospatial_Latitude = Latitude,
         geospatial_Longitude = Longitude)

# join dfs
coordinates_df_01 <- geospatial_coords_df_01 %>% 
  left_join(ssf_coords_df_01)

# commented out because I didn't need to export it 
# # set directory
# dir <- easycsv::choose_dir()
# setwd(dir)
# getwd()
# 
# # export .csv ----
# write_csv(coordinates_df_01, "geospatial_coords_comparison.csv")


# function to compare coordinates ----

assess_mapcoords <- function(df, Site_ID) {
  current_site <- Site_ID
  
  # filter df to inputed site
  current_df <- df %>% 
    filter(Site_ID == current_site) %>% 
    print()
  
  # separate out each coordinate pair
  coords_geospatial <- st_as_sf(current_df, coords = c("geospatial_Longitude", "geospatial_Latitude"), crs = 4326)
  coords_deploy <- st_as_sf(current_df, coords = c("Deploy_Longitude", "Deploy_Latitude"), crs = 4326)
  coords_sample <- st_as_sf(current_df, coords = c("Sample_Longitude", "Sample_Latitude"), crs = 4326)
  
  # display map
  current_mapcoords <- 
    mapview(coords_deploy, col.regions = "yellow", map.types = "Esri.WorldImagery", zcol = "Site_ID") +
    mapview(coords_sample, col.regions = "green", map.types = "Esri.WorldImagery", zcol = "Site_ID") +
    mapview(coords_geospatial, col.regions = "blue", map.types = "Esri.WorldImagery", zcol = "Site_ID")
  
  return(current_mapcoords)
  
}

# assess coordinates
# note: if close to geospatial, don't change coordinates (it's okay for them to be close but not exact)
assess_mapcoords(coordinates_df_01, "S01") #looks good - use sample
assess_mapcoords(coordinates_df_01, "S02") #looks good - use either
assess_mapcoords(coordinates_df_01, "S03") #looks good - use sample
assess_mapcoords(coordinates_df_01, "S04") #looks good - use sample
assess_mapcoords(coordinates_df_01, "S08") # checked with Jake - use sample
assess_mapcoords(coordinates_df_01, "S10") # checked with Jake - use sample (on main channel)
assess_mapcoords(coordinates_df_01, "S11") # checked with Jake - use sample
assess_mapcoords(coordinates_df_01, "S15") # sample far from others - use deploy since closer to water
assess_mapcoords(coordinates_df_01, "S23") # checked with Jake - use sample
assess_mapcoords(coordinates_df_01, "S29") #looks good - use either
assess_mapcoords(coordinates_df_01, "S47R") #looks good - use either
assess_mapcoords(coordinates_df_01, "S49R") #looks good - use either
assess_mapcoords(coordinates_df_01, "S50P") #looks good - use either
assess_mapcoords(coordinates_df_01, "S54") # checked with Jake - use deploy
assess_mapcoords(coordinates_df_01, "S58") #looks good - use either
assess_mapcoords(coordinates_df_01, "T05P") #looks good - use sample
assess_mapcoords(coordinates_df_01, "T41") #looks good - use sample



#### >> FIX METADATA ####

#### >> Fix coordinates ####
sites_that_use_deploy_coords <- c("S15", "S54") # default is to use sample coords

ssf_metadata_df_01 <- ssf_metadata %>% 
  mutate(Sample_Latitude = case_when(Site_ID %in% sites_that_use_deploy_coords ~ Deploy_Latitude,
                               TRUE ~ Sample_Latitude),
         Sample_Longitude = case_when(Site_ID %in% sites_that_use_deploy_coords ~ Deploy_Longitude,
                               TRUE ~ Sample_Longitude),
         Sample_GPS_Accuracy_ft = case_when(Site_ID == "S54" ~ -9999, 
                                            TRUE ~ Sample_GPS_Accuracy_ft))


#### >> Rename column names ####

# load data: column name mapping ----
# read in column name mapping file
mapping_filepath <- "C:\\Users\\powe419\\OneDrive - PNNL\\Documents - Core Richland and Sequim Lab-Field Team\\Data Generation and Files\\RC3\\05_Editable_ESS-DIVE_Format\\SSF_Data_Package\\SSF_metadata_column_curation.xlsx"
mapping_filepath <- file.choose() # select SSF_metadata_column_curation.xlsx
column_names_mapping <- read_excel(mapping_filepath) %>% 
  filter(is.na(Publish)) %>%
  # create new column names
  mutate(new_column_headers = case_when(!is.na(SSS_metadata_column_headers) ~ SSS_metadata_column_headers,
                                        TRUE ~ SSF_metadata_column_headers)) %>%
  rename(old_column_headers = SSF_metadata_column_headers) %>%
  # select old and new columns names and keep columns we want to publish
  select(old_column_headers, new_column_headers) %>%
  filter(!is.na(old_column_headers)) # excludes the 2 columns we want to add - they will be added in later in the script


#### >> remove columns ####
ssf_metadata_df_02 <- ssf_metadata_df_01 %>% 
  select(column_names_mapping$old_column_headers)


#### >> rename column names ####
ssf_metadata_df_03 <- ssf_metadata_df_02
colnames(ssf_metadata_df_03) <- column_names_mapping$new_column_headers

# check renaming
data.frame(old_cols = colnames(ssf_metadata_df_02), new_cols = colnames(ssf_metadata_df_03))


#### >> add in new columns ####
# there will be 2 new columns joined in via the SSS data package

# load data: sss metadata ----
sss_metadata_filepath <- "Z:\\00_Cross-SFA_ESSDIVE-Data-Package-Upload\\01_Study-Data-Package-Folders\\00_ARCHIVE-WHEN-PUBLISHED\\SSS_Data_Package_v2\\v2_SSS_Data_Package\\v2_SSS_Field_Metadata.csv"
sss_metadata_filepath <- file.choose() # select the most recent SSS field metadata file: in this case I'm using `v2_SSS_Field_Metadata.csv` from the secret folder archive
sss_metadata <- read_csv(sss_metadata_filepath)

# create join file by filtering sss metadata down to 2 columns we are adding in
sss_join <- sss_metadata %>% 
  select(Site_ID, Intermittent_or_Perennial, Stream_Name)
  
# join in new columns
ssf_metadata_df_04 <- ssf_metadata_df_03 %>% 
  left_join(sss_join)

# check join
ssf_metadata_df_04 %>% select(Site_ID, Intermittent_or_Perennial, Stream_Name) %>% View()

# manually update T41 because it didn't exist in SSS
ssf_metadata_df_04 <- ssf_metadata_df_04 %>% 
  mutate(Intermittent_or_Perennial = case_when((Site_ID == "T41" ~ "Perennial"), TRUE ~ Intermittent_or_Perennial),
         Stream_Name = case_when((Site_ID == "T41" ~ "Naches River"), TRUE ~ Stream_Name))


#### >> reorder columns based on sss ####

# extract column headers from SSF
ssf_col_names <- data.frame(ssf_col_names = colnames(ssf_metadata_df_04))

# extract column headers from SSS
sss_col_names <-  data.frame(sss_col_names = colnames(sss_metadata))

# compare SSS and SSF columns
compare_cols <- sss_col_names %>% 
  full_join(ssf_col_names, keep = TRUE, by = c("sss_col_names" = "ssf_col_names"))

# filter for only cols in SSF
new_ssf_cols <- compare_cols %>% 
  filter(!is.na(ssf_col_names)) %>% 
  select(ssf_col_names)

# reorder columns to desired locations
new_ssf_cols <- c(
  "Parent_ID",
  "Site_ID",
  "Latitude",
  "Longitude",
  "GPS_Accuracy_ft",
  "Stream_Name",
  "Intermittent_or_Perennial",
  "Sample_Date",
  "Sample_Time_Arriving_PST",
  "Sample_Time_Leaving_PST",
  "Time_Zone",
  "Weather",
  "Sediment",
  "Canopy_Coverage",
  "Macrophyte_Coverage",
  "Algal_Mat_Coverage",
  "General_Vegetation",
  "Hydrogeomorphology",
  "River_Gradient",
  "Change_In_River_Width_m",
  "Sample_Notes",
  "Deploy_Date",
  "Deploy_River_Width_m",
  "Deploy_Water_Depth_cm",
  "Deploy_miniDOT_Riverbed_Distance_cm-1",
  "Deploy_miniDOT_Location-1",
  "Deploy_Depth_Hobo_Riverbed_Distance_cm-1",
  "Deploy_Depth_Hobo_Location-1",
  "Deploy_miniDOT_Riverbed_Distance_cm-2",
  "Deploy_miniDOT_Location-2",
  "Deploy_Depth_Hobo_Riverbed_Distance_cm-2",
  "Deploy_Depth_Hobo_Location-2",
  "Deploy_Notes",
  "Retrieve_Date",
  "Retrieve_River_Width_Method",
  "Retrieve_Water_Depth_cm", 
  "Retrieve_Sensors_Underwater", 
  "Retrieve_Depth_Transect_Number_At_Sensors", 
  "Retrieve_Swoffer_Transect_Number_At_Sensors", 
  "Retrieve_Slope_Between_Water_And_Flag", 
  "Retrieve_Slope_Notes", 
  "Retrieve_miniDOT_Riverbed_Distance_cm-1", 
  "Retrieve_miniDOT_Location-1", 
  "Retrieve_Depth_Hobo_Riverbed_Distance_cm-1", 
  "Retrieve_Depth_Hobo_Location-1", 
  "Retrieve_miniDOT_Riverbed_Distance_cm-2", 
  "Retrieve_miniDOT_Location-2", 
  "Retrieve_Depth_Hobo_Riverbed_Distance_cm-2", 
  "Retrieve_Depth_Hobo_Location-2", 
  "Retrieve_Water_Depth_At_Swoffer_ft", 
  "Retrieve_Notes" 
)

# reorder ssf
ssf_metadata_df_05 <- ssf_metadata_df_04 %>% 
  select(new_ssf_cols)

#### >> add new column to denote burned status ####

# manually pull in data that Brie shared
site_burned_status <- data.frame(
    Site_ID = c("S01", "S02", "S04", "S10", "S11", "T41", "S50P", "S58", "S23", "S08", "S03", "S54", "S49R", "S47R", "T05P", "S29", "S15"),
    Site_Burned = c("Burned", "Burned", "Burned", "Burned", "Burned", "Burned", "Burned", "Burned", "Reference", "Reference", "Reference", "Reference", "Reference", "Reference", "Reference", "Reference", "Reference")
  ) %>% 
  # convert Site_Burned to logical
  mutate(Site_Burned = case_when(Site_Burned == "Burned" ~ TRUE, 
                                Site_Burned == "Reference" ~ FALSE))


# add new column to data
ssf_metadata_df_06 <- ssf_metadata_df_05 %>% 
  left_join(site_burned_status) %>% 
  relocate(Site_Burned, .after = "Intermittent_or_Perennial")
 

 mutate_at(Site_Burned = Site_Burned, .after = "Intermittent_or_Perennial")



#### >> Clean up global environment ####
ssf_metadata <- ssf_metadata_df_06


# list all objects in the global environment
all_objects <- ls()

# id objects that do not contain "ssf_metadata" in their names
objects_to_remove <- all_objects[!grepl("ssf_metadata", all_objects)]

# remove objects that do not contain "ssf_metadata" in their names
rm(list = objects_to_remove)

# remove all that contain "df_"
rm(list = ls(pattern = "df_"))
rm(all_objects)
rm(objects_to_remove)


# export new metadata file
# # set directory
dir <- easycsv::choose_dir()
setwd(dir)
getwd()


# # export .csv ----
file_name <- paste0("SSF_Deploy_Sample_Retrieve_Metadata_", Sys.Date(), ".csv")
write_csv(ssf_metadata, file_name)

  
  


# --------------------------------------------------------------------------- #



#### >> RUN FINAL CHECKS ####

#### User Inputs ####
df_being_checked <- read_csv(file.choose())
df_being_checked <- ssf_metadata


# check class structure ----

# create new df for checks
check_class_str <- data.frame(column_name = as.character(), class_str = as.character())

# loop through ssf and identify class for each column
for (col_name in names(df_being_checked)) {
  # collect the class of the current column
  current_col_class <- class(df_being_checked[[col_name]])
  
  # add current col name and class to existing df
  check_class_str <- rbind(check_class_str, data.frame(column_name = col_name, class_str = toString(current_col_class)))
}

count(check_class_str, class_str)


# view character columns ----
character_columns <- check_class_str %>% 
  filter(class_str == "character")

df_being_checked %>% 
  select(all_of(character_columns$column_name)) %>% 
  View()

# view categorical counts
# function to return unique item counts for character columns as a list
get_counts_categorical <- function(data_frame) {
  char_cols <- sapply(data_frame, is.character)  # Identify character columns
  char_cols_names <- names(data_frame)[char_cols]  # Get names of character columns
  
  get_counts_categorical <- list()  # Initialize an empty list
  
  for (col_name in char_cols_names) {
    unique_counts <- table(data_frame[[col_name]])  # Count unique items
    get_counts_categorical[[col_name]] <- unique_counts  # Add to the list
  }
  
  return(get_counts_categorical)
}

# Call the function to get unique item counts as a list
get_counts_categorical <- get_counts_categorical(df_being_checked)



# view numeric columns ----
numeric_columns <- check_class_str %>% 
  filter(class_str == "numeric")

df_being_checked %>% 
  select(all_of(numeric_columns$column_name)) %>% 
  View() # looks good

# view numerical counts
# function to return unique item counts for numerical columns as a list
get_counts_numerical <- function(data_frame) {
  num_cols <- sapply(data_frame, is.numeric)  # Identify character columns
  num_cols_names <- names(data_frame)[num_cols]  # Get names of character columns
  
  get_counts_numerical <- list()  # Initialize an empty list
  
  for (col_name in num_cols_names) {
    unique_counts <- table(data_frame[[col_name]])  # Count unique items
    get_counts_numerical[[col_name]] <- unique_counts  # Add to the list
  }
  
  return(get_counts_numerical)
}

# Call the function to get unique item counts as a list
get_counts_numerical <- get_counts_numerical(df_being_checked)



# view logical columns ----
logical_columns <- check_class_str %>% 
  filter(class_str == "logical")

df_being_checked %>% 
  select(all_of(logical_columns$column_name)) %>% 
  View() # looks good


# view other columns ----
other_columns <- check_class_str %>% 
  filter(class_str != "character" & class_str != "numeric" & class_str != "logical")

df_being_checked %>% 
  select(all_of(other_columns$column_name)) %>% 
  View()


# check if any rows contain "NA" ----
df_being_checked[!complete.cases(df_being_checked), ] %>% 
  View()

# check to see if there are any commas ----
  # TRUE = has commas; FALSE = no commas
any(grepl(",", unlist(df_being_checked))) %>% 
  View()
