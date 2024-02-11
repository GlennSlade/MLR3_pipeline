library (fasterize)
fdp <- read_sf("data_in/Dinaka/Dinaka_train.shp")
fdp4 <- read_sf("data_in/Dinaka/Dinaka_train_poly_10.shp")

look_up <- read_xlsx("data_in/Dinaka/Veg_type_lookup_list.xlsx", col_names=c("Type", "Description")) #|>   dplyr::select(!Description)
look_up$Type <- as.numeric(as.character(look_up$Type))
D <- rast("data_in/Dinaka/Dinaka_stack.tif") #import stacked image


#  Hey Glenn, just had a thought - maybe the bug is in terra::rasterize - instead try: fasterize:fasterize()

#you may may also need to st_as_sf() on your SpatVector to convert it as fasterize only accepts sf objects  
  
plot(D)
plot(fdp)
#plot(D+fdp)
fdp2 <- fdp|>  dplyr::select(!c(fid)) 
plot(fdp2)
attr(fdp2, 'CRS') <- crs(D)

r <- fasterize(fdp4, D, field = "Type", fun="mean")
plot(r)

fdpr <-fasterize::fasterize(fdp4)

plot(D)
plot(fdp2, add = TRUE)


fdp2 <- fdp|>  dplyr::select(!c(fid)) 

plot (fdp2)

plot(D)
plot(fdp, add=TRUE)


veg_types <- look_up|>
  right_join(fdp, by = "Type", multiple = "all") |>
  #dplyr::select(!c(fid)) |>
  st_as_sf()

ext_df <- exact_extract(D, fdp, fun="majority", progress=TRUE)


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
