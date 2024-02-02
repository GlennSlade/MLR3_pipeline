#' Predict model across a SpatRaster
#' 
#' A simple wrapper of terra::predict to save things in the correct place.
#'
#' @param x a SpatRaster
#' @param mod an ML model
#' @param site_name the site name/prefix
#' @param .workers default 1. can parallelise if you want/have enough RAM
#' @param na.rm some algorithms don't handle NA so often best to use na.rm
#' @param out_data_dir the parent out directory.
predict_terra <- function(x, mod, site_name,
                          .workers=1, na.rm=TRUE,
                          out_data_dir = "data_out"){
  
  out_dir <- file.path(out_data_dir, site_name)
  if (!dir.exists(out_dir)){
    dir.create(out_dir, recursive = TRUE)
  } 
  
  .fname <- file.path(out_dir, paste0(site_name, "_Predict_Map.tif"))
  
  terra::predict(x, mod, cores=.workers, na.rm=na.rm, filename=.fname, overwrite=TRUE)
}