


#' Build a multi band raster
#' 
#' build a stacked raster with all relevant layers.
#'
#' @param site_name character vector of length one. Should be the site prefix.
#' @param data_dir default "data_in". The parent directory of input data
#' @param out_data_dir default data_out". The parent directory of output data

build_cube_WV2 <- function(site_name, data_dir="data_in",
                       out_data_dir = "data_out"){
  
  # check the output directory and create if needed
  out_dir <- file.path(out_data_dir, site_name)
  if (!dir.exists(out_dir)){
    dir.create(out_dir, recursive = TRUE)
  } 
  
  # define input data directory
  base_dir <- file.path(data_dir, site_name)
  
  # get aoi
  aoi <- vect(file.path(base_dir, paste0(site_name, "_clip.shp")))
  
  # load and rename MS stack
  MS_CHM_stack <- rast(file.path(base_dir, 
                                 paste0(site_name, "_Corrected.tif")))
  
  names(MS_CHM_stack) <- c("blue", "green", "red", "nir")
  
  # load, rename and stack spectral indices
  add_band_names <- c("MSAVI", "MSAVI2", "MTVI", "NDVI", "SAVI")
  
  add_band_paths <- purrr::map(add_band_names,
             function(x){
               file.path(base_dir, 
                         paste0(site_name, "_", x, ".tif"))
             })

  
  # function to load and rename a raster - returns SpatRaster
  read_n_rename <- function(x, .name){
    x <- rast(x) 
    names(x) <- .name
    return(x)
  }
  
  # iterate over raster files and names, then stack and use project to crop/reproject
  # to the extent matching the MS raster which is already cropped etc.. Uses gdal warp
  # under the hood which is pure fire!
  add_band_stack <- purrr::map2(.x=add_band_paths, .y=add_band_names,
                                ~read_n_rename(.x, .y)) |> 
    rast() |> 
    terra::project(MS_CHM_stack)
  
  # Now let's stack this and the main MS file into one big raster stack. Let's 
  # use DEFLATE compression - it's much better on space than LZW (but is a little slower)
  
  all_bands <- c(MS_CHM_stack, add_band_stack) |> 
    mask(aoi)

  writeRaster(all_bands,
              file.path(out_data_dir, site_name, paste0(site_name, "_stack.tif")),
              wopt= list(gdal=c("COMPRESS=DEFLATE")),
              overwrite=TRUE)
  
}