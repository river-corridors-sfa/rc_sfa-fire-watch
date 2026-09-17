getWatersheds <- function(df = sites, massive = TRUE, make_pretty = TRUE){
  
  subset_sites <- df %>%
    distinct(comid, .keep_all = TRUE)
  
  # ---- Get upstream COMIDs for each site via the NLDI web service ----
  trace_upstream <- function(site_id, site_comid){
    ut <- tryCatch({
      nhdplusTools::navigate_nldi(
        nldi_feature = list(featureSource = "comid",
                            featureID = as.character(site_comid)),
        mode          = "UT",           # Upstream with Tributaries
        distance_km   = 9999            # effectively unlimited
      )
    }, error = function(e) NULL)
    
    if(is.null(ut) || length(ut$UT_flowlines) == 0) {
      # Fall back to at least the outlet itself
      return(tibble(origin = site_comid, comid = site_comid))
    }
    
    tibble(
      origin = site_comid,
      comid  = as.integer(unique(c(site_comid, ut$UT_flowlines$nhdplus_comid)))
    )
  }
  
  upstream_list <- purrr::map2_dfr(
    subset_sites$site,
    subset_sites$comid,
    trace_upstream
  ) %>%
    dplyr::distinct(origin, comid)
  
  # ---- Pull catchment polygons ----
  if(massive == FALSE){
    
    catchments <- vector("list", length = nrow(upstream_list))
    for(i in seq_len(nrow(upstream_list))){
      catchments[[i]] <- try(get_nhdplus(comid = upstream_list$comid[i],
                                         realization = 'catchment',
                                         t_srs = 4269))
    }
    catchments <- bind_rows(catchments) %>%
      dplyr::select(comid = featureid)
    
  } else {
    # Static, pre-downloaded CONUS catchment layer
    catchments <- readRDS('~/Documents/GitHub/rc_sfa-rc-3-wenas-meta/R_scripts/data/us_catchments.RDS') %>%
      dplyr::rename(comid = FEATUREID) %>%
      dplyr::filter(comid %in% upstream_list$comid)
  }
  
  # ---- Dissolve to watershed polygon per site ----
  site_watersheds <- merge(catchments, upstream_list, by = 'comid', all.x = FALSE) %>%
    group_by(origin) %>%
    dplyr::summarize() %>%
    dplyr::rename(comid = origin)
  
  if(make_pretty == TRUE){
    site_watersheds <- nngeo::st_remove_holes(site_watersheds)
  }
  
  return(site_watersheds)
}