# load field survey points.
fdp <- read_sf("data_in/Dinaka/Dinaka_train.shp")
# clean up the training data get what we need.
look_up <- read_xlsx("data_in/Veg_type_lookup_list.xlsx", col_names=c("Type", "Description")) #|>   dplyr::select(!Description)
fdp$Type <- as.numeric(as.character(fdp$Type))

x
fdp
fdp

veg_types <- look_up|>
  right_join(fdp, by = "Type", multiple = "all") |>
  #dplyr::select(!c(Photo, Notes, layer, path)) |>
  st_as_sf()

ext_df <- exact_extract(x, veg_types, fun="mean", progress=TRUE)
pnt_df <-   bind_cols(veg_types, ext_df)|>
  st_centroid()
.xy <- st_coordinates(pnt_df)
pnt_df <- pnt_df |>
  dplyr::arrange(Type) |>
  mutate(x=.xy[,1],
         y=.xy[,2],
         Type=factor(Type, levels=unique(Type))) |>
  st_drop_geometry() |>
  #dplyr::select(!c("fid", "Date"))|>
  rename_at(vars(starts_with('mean')), ~(gsub("mean.","",.x)))

attr(pnt_df, 'CRS') <- crs(cube)

saveRDS(pnt_df, out_df)