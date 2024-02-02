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
predict_terra_tile <- function(x, mod, site_name,
                          .workers=1, na.rm=TRUE,
                          out_data_dir = "data_out",
                          tile=FALSE,
                          tile_dims=5){
  
  out_dir <- file.path(out_data_dir, site_name)
  if (!dir.exists(out_dir)){
    dir.create(out_dir, recursive = TRUE)
  } 
  
  .fname <- file.path(out_dir, paste0(site_name, "_Predict_Map.tif"))
  
  ras_pred <- function(x, mod, .cores, na.rm, filename, overwrite=TRUE){
    terra::predict(x, mod, cores=.cores, na.rm=na.rm, filename=filename, overwrite=overwrite)
  }
  
  
  if (isFALSE(tile)){
    preds <- ras_pred(x, mod, .cores=.workers, na.rm=na.rm, filename=.fname)
  } else {
    tt <- tiler_template(x, tile_dims)
    
    .td <- file.path(tempdir(), "tile_.tif")
    
    .tiles <- terra::makeTiles(x, tt, filename=.td, extend=TRUE, na.rm=TRUE,
                               overwrite=TRUE, gdal=c("COMPRESS=DEFLATE"))
    
    pred.paths <- file.path(dirname(.tiles), paste0("predict_", basename(.tiles)))
    
    pred_return_src <- function(x, p){
      ras_pred(terra::rast(x), mod, .cores=.workers, na.rm=na.rm, filename=p) |> 
        terra::sources()
    }
    
    pred.tiles <- list(.tiles, pred.paths) |> 
      purrr::pmap(~pred_return_src(x=..1, p=..2), .progress=TRUE) 
    
    
    preds <- sf_warp_util(unlist(pred.tiles),
                          crs = terra::crs(x), 
                          destination = .fname) |> 
      terra::rast()
  }
  
  
  return(preds)
  
  
}


#' Easy Warp using sf::gdalutils
#'
#' stable and supports >memory rasters and cutline feature.

#'
#' @param sources list of raster sources.
#' @param destination out put destination. if NUll then a tempfile is created
#' @param resample resampling method, default near
#' @param compression the tif compression method to use - e.g. "DEFLATE" or "LZW"
#' @param options character vector with gdal options.
#' @param progress logical - should a progress bar be shown.
#' @param ... Not used.
#'
#' @return A raster file path.
#' @noRd
sf_warp_util <- function(sources,
                         crs,
                         destination,
                         resample= 'near',
                         compression="DEFLATE",
                         nodata=NULL,
                         options=NULL,
                         progress=TRUE,
                         ...) {
  
  if (is.null(destination)){
    destination <- tempfile(fileext = '.tif')
  }
  
  opts <- c(
    "-t_srs", crs,
    "-r", resample,
    "-overwrite",
    "-co", paste0("COMPRESS=", compression),
    options
  )
  
  if (!is.null(nodata)){
    opts <- c(opts, "-dstnodata", nodata)
  }
  
  
  sf::gdal_utils(
    util = "warp",
    source = sources,
    destination = destination,
    options = opts,
    quiet = progress
  )
  destination
}


#' Create template SpatRaster for tiling.
#'
#'
#' @param x 
#' @param target_dims 
#'
#' @return an empty raster to be used as a template
tiler_template <- function(x, target_dims = 5) {
  
  if (length(target_dims)==1){
    target_dims <- c(target_dims, target_dims) 
  } else if (length(target_dims)>2){
    stop("target_dims is the incorrect length - should be length 1 or 2.")
  }
  
  rast(
    nrows = target_dims[1],
    ncols = target_dims[2],
    crs = crs(x),
    extent = ext(x)
  )
}