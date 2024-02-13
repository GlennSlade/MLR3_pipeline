# build_task <- function(df,
#                        site_name,
#                        drop_cols = c("Description")) {
build_task <- function(df,
                         site_name) {
  
    
  
  full_df <- readRDS(df) #|> 
  #  dplyr::select(!all_of(drop_cols)) 
  full_df <- as.data.frame(full_df)
  
  # define the mlr3 task
  task <- mlr3spatiotempcv::TaskClassifST$new(
    id = site_name,
    backend = full_df, 
    target = "Type",
    coordinate_names = c("x", "y"),
    coords_as_features = FALSE,
    crs = attributes(full_df)$CRS)
  return(task)
  
}