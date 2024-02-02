library (terra)

a = rast("data_in/LS8/LS8_NDVI.tif")
a <- terra::project(a, y="EPSG:32734")

g <- rast("data_in/LS8/LS8_NDVI_2001-2011.tif")
g <- terra::project(g, y="EPSG:32734")


h = rast("data_in/LS8/LS8_09.tif")
h <- terra::project(h, y="EPSG:32734")


k <- rast("data_in/LS8/LS8_NDVI_2012-2022.tif")
k <- terra::project(k, y="EPSG:32734")


aoi <- vect("data_in/LS8/LS8_clip.shp")
aoi <- terra::project(aoi, y="EPSG:32734")


g <- terra::resample(g, h)
k <- terra::resample(k, h)
a<- terra::resample(a, h)
g
k
h
a

writeRaster(k, filename ="data_in/LS8/LS8_NDVI_2012-2022a.tif", overwrite=TRUE)

writeRaster(g, filename ="data_in/LS8/LS8_NDVI_2001-2011a.tif", overwrite=TRUE)

writeRaster(h, filename ="data_in/LS8/LS8_09a.tif", overwrite=TRUE)

writeRaster(a, filename ="data_in/LS8/LS8_NDVIa.tif", overwrite=TRUE)

