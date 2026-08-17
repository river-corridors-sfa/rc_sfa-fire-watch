# =================================== Objectives =================================
#
# Objectives: I want to test a CQ script from: https://onlinelibrary.wiley.com/doi/full/10.1002/hyp.14456?casa_token=pz1ap4VTrSYAAAAA%3AquJ8dsxJjjrFkbo817njoktyUCem4eocZTiWC8FoncfzjPQaWDs7-ND-J-CwdQp38_Nge4CajwQMB2A
# REPO: https://github.com/dMillarARS/cQ_analysis
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
# 13 August 2026

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
       viridis)

source('./cQ_functions.R')
output_dir <- "~/GitHub/rc_sfa-fire-watch/Retreat_Fire_2024/output_for_analysis/cQ_TEST"
# ================================= Discharge from M01 ================================
# Oak Creek
siteNumber <- "12492900"

siteInfo <- readNWISsite(siteNumber)

# parameter codes: 
# 00060 - Discharge, cubic feet per second
# 32295 - Dissolved organic matter fluorescence (fDOM), water, in situ, concentration estimated from reference material, micrograms per liter as quinine sulfate equivalents (QSE)
# 32322 - Dissolved organic matter relative fluorescence (fDOM), water, in situ, relative fluorescence units (RFU)
# 00300 - Dissolved oxygen, water, unfiltered, milligrams per liter
# 00301 - Dissolved oxygen, water, unfiltered, percent of saturation
# 00400 - pH, water, unfiltered, field, standard units
# 00095 - Specific conductance, water, unfiltered, microsiemens per centimeter at 25 degrees Celsius
# 00010 - Temperature, water, degrees Celsius
# 00076 - Turbidity, water, unfiltered, nephelometric turbidity units

parametersOfInterest <- c("00060","00095")

# Read in the data
dataset <- readNWISuv(siteNumber,parametersOfInterest,
                      "2024-10-17","2026-08-13",tz="PST")


allInputData15Min <- dataset %>% 
  select(dateTime,
         X_00060_00000,
         X_00095_00000) %>%
  rename(datetime = dateTime,
         q_cfs = X_00060_00000,
         conc = X_00095_00000) %>%
  mutate(q_cms = q_cfs * 0.0283168)

#-------------------------------------------------------------

# Subset the data to the longest time period without missing concentration data
#---------------------------------

allInputData15Min$section_id <- numeric(length=nrow(allInputData15Min))

counter = 0

for (dt in 2:nrow(allInputData15Min)) {
  
  if (is.na(allInputData15Min$conc[dt]) == FALSE & is.na(allInputData15Min$conc[dt-1]) == TRUE) {
    
    counter = counter + 1 
    
    allInputData15Min$section_id[dt] = counter 
    
  }
  
  else if (is.na(allInputData15Min$conc[dt]) == FALSE & is.na(allInputData15Min$conc[dt-1]) == FALSE){
    
    allInputData15Min$section_id[dt] = allInputData15Min$section_id[dt-1]
    
  }
  
  else {allInputData15Min$section_id[dt] = 0}
  
  
}

LongestStretchID <-  allInputData15Min[complete.cases(allInputData15Min),] %>% 
  count(section_id) %>% 
  filter(n == max(n)) %>% 
  select(section_id)

allInputData15Min <- allInputData15Min %>% filter(section_id == as.numeric(LongestStretchID))

#---------------------------------

# Name for associated output data
dataSetName <- "Oak_Creek"

# Chose constituent for plot axes labels (SPC)
constit <- "SPC"

# Interpolate missing flow data (only a few cases where flow was 0 for one contiguous time step)...
allInputData15Min$q_cms[allInputData15Min$q_cms == 0] <- NA
interpFlow <- approx(allInputData15Min$datetime,
                     allInputData15Min$q_cms,
                     xout=allInputData15Min$datetime)

allInputData15Min$q_cms <- interpFlow$y

allInputData15Min <- allInputData15Min %>% 
  mutate(rescaled_conc = ((conc-min(conc))/(max(conc)-min(conc))*max(q_cms)))

# Vector containing candidate baseflow separation filter values
candidateFilterPara <- c(0.99,0.995,0.999)

# Vector containing candidate stormflow threshold values
candidateSfThresh <- c(0.01, 0.02, 0.05)

# Vector with interpolation intervals used for calculating HI
interp <- seq(0,1,0.01)

##########################################
# RUN ANALYSIS TO GET HYSTERESIS INDICES #
##########################################

batchRun1 <- batchRunBfAndEvSepForCQ(qInputs = allInputData15Min,
                                     bfSepPasses = 3,
                                     filterParam = candidateFilterPara,
                                     sfSmoothPasses = 4,
                                     sfThresh = candidateSfThresh,
                                     cInputs = allInputData15Min,
                                     timeStep = 15,
                                     minDuration = 12,
                                     maxDuration = 8000)

### TEST START ####
filterParaValue <- candidateFilterPara[1]
sfThreshValue <- candidateSfThresh[1]

baseFlow <- baseflowSeparation(
  datetime = allInputData15Min$datetime,
  streamflow = allInputData15Min$q_cms,
  filterPara = filterParaValue,
  passes = 3
)

cat("BASE FLOW\n")
str(baseFlow)
cat("NA datetime:", sum(is.na(baseFlow$datetime)), "\n")
cat("Rows:", nrow(baseFlow), "\n")

smoothStorm <- smoothStormFlow(
  dateTime = baseFlow$datetime,
  stormFlow = baseFlow$storm_flow,
  totFlow = baseFlow$total_flow,
  passes = 4
)
cat("\nSMOOTH STORM\n")
str(smoothStorm)
cat("NA datetime:", sum(is.na(smoothStorm$datetime)), "\n")
cat("Rows:", nrow(smoothStorm), "\n")

stormFlowWithIds <- baseFlowEventSep(
  smoothStFlow = smoothStorm,
  sfThresh = sfThreshValue
)

cat("\nSTORM FLOW WITH IDS\n")
str(stormFlowWithIds)
cat("Rows:", nrow(stormFlowWithIds), "\n")

eventOutput <- processStormEventsWithConc(
  stormIds = stormFlowWithIds,
  conc = allInputData15Min,
  timestep_min = 15,
  minDuration_hrs = 12,
  maxDuration_hrs = 8000,
  filterParam = filterParaValue,
  sfThresh = sfThreshValue
)

summary(stormFlowWithIds$smooth_st_flow)

sfThreshValue

cat("Filter parameter:", filterParaValue, "\n")
cat("Stormflow threshold:", sfThreshValue, "\n\n")

summary(stormFlowWithIds$smooth_st_flow)

cat("\nMaximum smooth stormflow:", 
    max(stormFlowWithIds$smooth_st_flow, na.rm = TRUE), "\n")

cat("Number above threshold:",
    sum(stormFlowWithIds$smooth_st_flow >= sfThreshValue, na.rm = TRUE), "\n")

### TEST END ####

eventsDataAll1 <- getAllStormEvents(batchRun = batchRun1,
                                    timestep_min = 15)

batchRunFlowsLF1 <- batchRunflowCompare(qData = allInputData15Min,
                                        bfSepPasses = 3,
                                        filterPara = candidateFilterPara,
                                        sfSmoothPasses = 4)

eventsData1 <- stormEventCalcs(batchRun = batchRun1,
                               timestep_min = 15)


stormCounts1 <- stormCounts(batchRun1)


hysteresisData1 <- getHysteresisIndices(batchRun = batchRun1,
                                        xForInterp = interp,
                                        eventsData = eventsData1)

#---------------------------------

#########################################
# PLOT AND SAVE DATA - EVENT SEPARATION #
#########################################

# Make subfolder in output directory to save hydrograph plots
dir.create(file.path(output_dir, "Hydrographs"), showWarnings = FALSE)

# 1) Plot and save the hydrograph with input data

initialHydrograph <- ggplot(allInputData15Min,aes(x=datetime, y=q_cms)) +
  geom_line(linewidth = 0.5, color="black") +
  xlab(NULL) +
  ylab(expression(paste("Total discharge (",m^3," ",s^-1,")"))) +
  theme_bw() +
  theme(text=element_text(size=18))

ggsave(file=file.path(output_dir,"Hydrographs",paste(dataSetName,"_TotalDischarge.jpeg")),
       initialHydrograph,
       width = 12, 
       height = 4, 
       units = "in",
       dpi=600)


# 2) Plot total discharge with baseflow

baseflowHydrograph <- ggplot() + 
  geom_line(data=batchRunFlowsLF1, aes(x=datetime, y=total_flow), size=0.5, color="black") +
  geom_line(data=batchRunFlowsLF1, aes(x=datetime, y=base_flow,color=filter_para), size=0.75) +
  scale_color_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab(expression(paste("Discharge (",m^3," ",s^-1,")"))) +
  theme_bw() +
  theme(legend.title = element_blank(),
        text=element_text(size=18))

ggsave(file=file.path(output_dir,"Hydrographs",paste(dataSetName,"_Baseflows.jpeg")),
       baseflowHydrograph,
       width = 14, 
       height = 4, 
       units = "in",
       dpi=600)


# 3) Plot smoothed storm flows

stormflowHydrograph <- ggplot() + 
  geom_line(data=batchRunFlowsLF1, aes(x=datetime, y=storm_flow,color=filter_para), size=0.75) +
  scale_color_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab(expression(paste("Storm flow (",m^3," ",s^-1,")"))) +
  theme_bw() +
  theme(legend.title = element_blank(),
        text=element_text(size=18))

ggsave(file=file.path(output_dir,"Hydrographs",paste(dataSetName,"_StormflowsOnly.jpeg")),
       stormflowHydrograph,
       width = 14, 
       height = 4, 
       units = "in",
       dpi=600)


# 3a) Plot smoothed storm flows with storm flow thresholds

stormflowThreshHydrograph <- ggplot() + 
  geom_line(data=batchRunFlowsLF1, aes(x=datetime, y=storm_flow,color=filter_para), size=0.75) +
  scale_color_brewer(palette = "Set1") +
  geom_hline(yintercept = candidateSfThresh, linetype = "dashed", color = "black",alpha=0.5) +
  xlab(NULL) +
  ylab(expression(paste("Storm flow (",m^3," ",s^-1,")"))) +
  theme_bw() +
  theme(legend.title = element_blank(),
        text=element_text(size=18))

ggsave(file=file.path(output_dir,"Hydrographs",paste(dataSetName,"_StormflowsOnlyWithThresholds.jpeg")),
       stormflowThreshHydrograph,
       width = 14, 
       height = 4, 
       units = "in",
       dpi=600)


# 4) Plot batch run event separation hydrographs

eventsDataShaded1 <- eventsData1 %>% mutate(start = as.POSIXct(start,
                                                               format("%Y-%m-%d %H:%M:%S"),tz="EST"),
                                            end = as.POSIXct(end,
                                                             format("%Y-%m-%d %H:%M:%S"),tz="EST"),
                                            tops = max(allInputData15Min$q_cms),
                                            bottoms = 0)

batchEventSepPlot <- ggplot() + 
  geom_rect(data=eventsDataShaded1, mapping=aes(xmin=start, 
                                                xmax=end, 
                                                ymin=bottoms, 
                                                ymax=tops), fill="green", color="red", alpha=0.2) +
  
  geom_line(data=allInputData15Min, aes(x=datetime, y=q_cms), size=0.8, color="blue") +
  geom_line(data=allInputData15Min, aes(x=datetime, y=rescaled_conc), size=0.5, color="black",linetype="dashed") +
  facet_wrap(~ run_id, ncol = 2) +
  scale_color_brewer(palette = "Set1") +
  xlab(NULL) +
  ylab(expression(paste("Discharge (",m^3," ",s^-1,")"))) +
  theme_bw() +
  theme(legend.title = element_blank(),
        text=element_text(size=18))


ggsave(file=file.path(output_dir,"Hydrographs",paste(dataSetName,"_BatchEventSeparationPlot.jpeg")),
       batchEventSepPlot,
       width = 14, 
       height = 10, 
       units = "in",
       dpi=600)

####################################
# PLOT AND SAVE DATA - c-Q RESULTS #
####################################
if (constit == "SPC") {
  
  makeCQPlotsNO3(batchRun1)
  makeHystFlushPlotsNO3(hysteresisData1)
  
} else if (constit == "TOC") {
  
  makeCQPlotsTOC(batchRun1)
  makeHystFlushPlotsTOC(hysteresisData1)
  
} else if (constit == "turbity") {
  
  makeCQPlotsTurb(batchRun1) 
  makeHystFlushPlotsTurb(hysteresisData1)
  
}

