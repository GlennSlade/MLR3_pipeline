fdp <- read_sf("data_in/Dinaka/Dinaka_train_poly.shp")
look_up <- read_xlsx("data_in/Dinaka/Veg_type_lookup_list.xlsx", col_names=c("Type", "Description")) #|>   dplyr::select(!Description)
look_up$Type <- as.numeric(as.character(look_up$Type))

veg_types <- look_up|>
  right_join(fdp, by = "Type", multiple = "all") |>
  #dplyr::select(!c(fid)) |>
  st_as_sf()

tv <- vect(veg_types) |> 
  rasterize(x,
            field="Type")
comb_bands <- c(tv, x)

grid_df <- terra_read_rows(comb_bands,
                           ...) |>
  left_join(look_up, by = "Type") |> 
  dplyr::arrange(Type) |> 
  mutate(Type=factor(Type, levels=unique(Type)))
unique(grid_df$Type)

attr(grid_df, 'CRS') <- crs(cube)
