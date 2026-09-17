getMaps <- function(x = sites$site,
                    y = sites$comid,
                    flowlines_path = '~/GitHub/rc_sfa-fire-watch/geospatial_data_functions/src/site_flowlines.gpkg',
                    out_dir        = '~/GitHub/rc_sfa-fire-watch/geospatial_data_functions/maps/'){
  
  # Make sure output directory exists
  if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Read flowlines once (outside the loop) for efficiency
  all_flowlines <- sf::st_read(flowlines_path, quiet = TRUE)
  
  make_one_map <- function(site_id, site_comid){
    
    flowlines  <- dplyr::filter(all_flowlines,   comid == site_comid)
    watersheds <- dplyr::filter(site_watersheds, comid == site_comid)
    points     <- dplyr::filter(sites,           site  == site_id)
    
    # Skip if any layer is empty
    if(nrow(watersheds) == 0 | nrow(points) == 0){
      message("Skipping ", site_id, " — missing watershed or point geometry.")
      return(invisible(NULL))
    }
    
    plot <- mapview(watersheds, col.regions = "#56B4E9", alpha.regions = 0.2,
                    lwd = 3, layer.name = "Watershed") +
      mapview(flowlines,  lwd = 8, color = "red",   layer.name = "Flowline") +
      mapview(points,     cex = 8, col.region = "black",
              layer.name = as.character(site_id))
    
    out_file <- file.path(out_dir, paste0(site_id, '.jpg'))
    
    # mapshot2() replaces the deprecated mapshot()
    tryCatch(
      mapview::mapshot2(plot, file = out_file),
      error = function(e) message("Failed to save map for ", site_id, ": ", e$message)
    )
    
    invisible(out_file)
  }
  
  # Handle either a single site or a vector of sites
  purrr::walk2(x, y, make_one_map)
}
