


#' build data frame for ML inputs
#'
#' @param cube The raster cube from which to derive the covariates
#' @param site_name The site prefix 
#' @param data_dir The parent of the input data
#' @param lookup_file location of lookup table
#' @param out_data_dir parent of output data
#' @param df_type either "grid" or "point". grid returns all classified cells
#' whereas "point returns the mean of overlapping cells (much smaller df).
#' @param ... passed to `terra_read_rows` a custom function - could be for example
#' .prop = 0.1 if the function is too RAM intensive.
build_ml_df_WV2_E <- function(cube,
                        site_name, 
                        data_dir="data_in", 
                        lookup_file = file.path(data_dir, "Veg_type_lookup_list.xlsx"),
                        out_data_dir = "data_out",
                        df_type = "grid",
                        ...){
  # check the output directory and create if needed
  out_dir <- file.path(out_data_dir, site_name)
  if (!dir.exists(out_dir)){
    dir.create(out_dir, recursive = TRUE)
  } 
  # define input data directory
  base_dir <- file.path(data_dir, site_name)
  
  # load field survey points.
  fdp <- read_sf("data_in/WV2/Train_extract/WV2_equal_class_size_500_add.shp")
  #fdp <- read_sf("data_in/WV2/Train_extract/WV2_equal_class_size_280_train_add.shp")
 
  #data$x1 <- as.numeric(as.character(data$x1))
  
  st_crs(fdp) = 32734
  
  fdp
  
  
  fdp$Type <- as.numeric(as.character(fdp$Type))
 #fdp
  # clean up the training data get what we need.
  look_up <- read_xlsx(lookup_file, 
                       col_names=c("Type", "Class", "Description")) |> 
    select(!Description)
  
  
  veg_types <- look_up|>
    right_join(fdp, by = "Type", multiple = "all") |>
    select(!c(Photo, Notes, layer, path)) |>
    st_as_sf()
  
  if (df_type=="point"){
    out_df <- file.path(out_dir, paste0(site_name, "ML_in_Point_level.rds"))
    # Let's extract the intersecting raster values for the field data.
    # this is one way to do it by getting average values for each measured point
    # browser()
    ext_df <- exact_extract(cube, veg_types, fun="mean", progress=TRUE)
    pnt_df <-   bind_cols(veg_types, ext_df)|>
      st_centroid()
    .xy <- st_coordinates(pnt_df)
    pnt_df <- pnt_df |>
      mutate(x=.xy[,1],
             y=.xy[,2],
             Class=as.factor(Class)) |>
      st_drop_geometry() |>
      select(!c("fid", "Date"))|>
      rename_at(vars(starts_with('mean')), ~(gsub("mean.","",.x)))
    
    attr(pnt_df, 'CRS') <- crs(cube)
    
    saveRDS(pnt_df, out_df)
    
  } else if (df_type=="grid"){
    
    out_df <- file.path(out_dir, paste0(site_name, "ML_in_grid_level.rds"))
    # rasterize vector to same grid as cube.
    tv <- vect(veg_types) |> 
      rasterize(cube,
                field="Type")
    
    # raster with all bands and the class key
    comb_bands <- c(tv, cube)
    
    grid_df <- terra_read_rows(comb_bands,
                    ...) |>
      left_join(look_up, by = "Type") |> 
      mutate(Type=as.factor(Type))
    
    attr(grid_df, 'CRS') <- crs(cube)
    # attributes(grid_df)
    
    saveRDS(grid_df, out_df)
  }
  
  return(out_df)

}
