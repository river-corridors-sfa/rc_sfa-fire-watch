# SSF Post Curation Metadata Updates
# 2023-10-16
# Bibi Powers-McCormack
  
# Script Objective: After the initial curaiton, completed via the script `SSF_metadata_curation.R`, there are additional edits

# Curation Notes
  # ssf_metadata_df_01 = joins new Fire_Relationship column and removes the Site_Affected_By_Fire column


### PREP #######################################################################
library(tidyverse)


### LOAD IN DATA ###############################################################

# load in metadata
   # pulling in the current Field Metadata file
ssf_metadata_filepath <- "C:\\Users\\powe419\\OneDrive - PNNL\\Documents - Core Richland and Sequim Lab-Field Team\\Data Generation and Files\\RC3\\05_Editable_ESS-DIVE_Format\\SSF_Data_Package\\SSF_Data_Package\\SSF_Field_Metadata.csv"
ssf_metadata_filepath <- file.choose()
ssf_metadata <- read_csv(ssf_metadata_filepath)

# load in updated column
fire_relationship_df <- data.frame(
  Site_ID = c("S01", "S02", "S04", "S10", "S11", "T41", "S50P", "S58", "S23", "S08", "S03", "S54", "S49R", "S47R", "T05P", "S29", "S15"),
  Fire_Relationship = c("Site downstream of fire", "Site downstream of fire", "Site within fire boundary", "Site within fire boundary", "Site downstream of fire", "Site downstream of fire", "Site downstream of fire", "Site downstream of fire", "Site has no relationship to the fire", "Site has no relationship to the fire", "Site has no relationship to the fire", "Site has no relationship to the fire", "Site has no relationship to the fire", "Site has no relationship to the fire", "Site has no relationship to the fire", "Site has no relationship to the fire", "Site has no relationship to the fire")
)


### JOIN DATA ##################################################################

# join data
ssf_metadata_df_01 <- ssf_metadata %>% 
  left_join(fire_relationship_df) %>% 
  select(1:7, Fire_Relationship, everything()) %>% 
  
  
# drop "Site_Affected_By_Fire" column
  select(-Site_Affected_By_Fire)


### WRITE METADATA CSV #########################################################

out_dir <- "C:\\Users\\powe419\\OneDrive - PNNL\\Documents - Core Richland and Sequim Lab-Field Team\\Data Generation and Files\\RC3\\05_Editable_ESS-DIVE_Format\\SSF_Data_Package\\SSF_Data_Package\\"

# writing out with today's date. After exported, added the original date (2023-10-06) on to the "Field_Metadata.csv" file and moved it out of DP. Then removed today's date from today's export to make it the current version. 
write_csv(ssf_metadata_df_01, paste0(out_dir, "SSF_Field_Metadata_", Sys.Date(), ".csv"))

