### Script to Extract Data from Drone survey Classifications based on WV2 pixel grid
### matching pixels
### Imports classified drone image data set and calculates fractional cover for each 
### class in each WV2 pixel and then samples that data to produce training data set
### for classification of WV2 image.

library(terra)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(MASS)
library(splines)
library(rgeos)
library(tidyverse)
library(viridis)
library(gridExtra)
library(DescTools)
library(sf)
library(exactextractr)
library(writexl) 
#library (raster)
library(dplyr)

#-------0. SET Parameters to use for training data set

#Fraction  cover to use to define training class 1.0 = 100%
#Change this to vary the pixel definitions
A = "0.99"

#Fraction  cover to use to define training class for Prosopis 1.0 = 100%
#Change this to vary the pixel definitions
P = "0.99"

#Number of pixels from each Class to use in training data

N = "30"


#----1. Read in files for Bokspits_1 etc Classified image data ----


Bokspits_1_Predict <- rast("data_out/Bokspits_1/Bokspits_1_Predict_Map.tif")
Bokspits_1_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF

Bokspits_1_WV2_grid_SF <- read_sf(dsn = 'data_in/WV2_Grids', layer = "Bokspits_1_WV2_grid")
Bokspits_1_WV2_grid_SF



#---2. Extract data for Bokspits_1 Grid


Bokspits_1_WV2_Extract <- exact_extract(Bokspits_1_Predict,Bokspits_1_WV2_grid_SF,"mode")
names(Bokspits_1_WV2_Extract) <- c('mode')
Bokspits_1_WV2_Extract_DF <-bind_cols(Bokspits_1_WV2_grid_SF,Bokspits_1_WV2_Extract)

Bokspits_1_WV2_Extract <- exact_extract(Bokspits_1_Predict,Bokspits_1_WV2_grid_SF,"majority" )
names(Bokspits_1_WV2_Extract) <- c('Type')
Bokspits_1_WV2_Extract_DF <- dplyr::mutate(Bokspits_1_WV2_Extract_DF, Type = Bokspits_1_WV2_Extract)

Bokspits_1_WV2_Extract <- exact_extract(Bokspits_1_Predict,Bokspits_1_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_1_WV2_Extract_DF <- dplyr::mutate(Bokspits_1_WV2_Extract_DF, Bokspits_1_WV2_Extract)

#----3. Read in files for Bokspits_2 etc Classified image data ----


Bokspits_2_Predict <- rast("data_out/Bokspits_2/Bokspits_2_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Bokspits_2_WV2_grid_SF <- read_sf(dsn = 'data_in/WV2_Grids', layer = "Bokspits_2_WV2_grid")


#---4.. Extract data for Bokspits_2 Grid


Bokspits_2_WV2_Extract <- exact_extract(Bokspits_2_Predict,Bokspits_2_WV2_grid_SF,"mode")
names(Bokspits_2_WV2_Extract) <- c('mode')
Bokspits_2_WV2_Extract_DF <-bind_cols(Bokspits_2_WV2_grid_SF,Bokspits_2_WV2_Extract)

Bokspits_2_WV2_Extract <- exact_extract(Bokspits_2_Predict,Bokspits_2_WV2_grid_SF,"majority" )
names(Bokspits_2_WV2_Extract) <- c('Type')
Bokspits_2_WV2_Extract_DF <- dplyr::mutate(Bokspits_2_WV2_Extract_DF, Type = Bokspits_2_WV2_Extract)

Bokspits_2_WV2_Extract <- exact_extract(Bokspits_2_Predict,Bokspits_2_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_2_WV2_Extract_DF <- dplyr::mutate(Bokspits_2_WV2_Extract_DF, Bokspits_2_WV2_Extract)

#----5. Read in files for Bokspits_3 etc Classified image data ----


Bokspits_3_Predict <- rast("data_out/Bokspits_3/Bokspits_3_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Bokspits_3_WV2_grid_SF <- read_sf(dsn = 'data_in/WV2_Grids', layer = "Bokspits_3_WV2_grid")


#---6. Extract data for Bokspits_3 Grid


Bokspits_3_WV2_Extract <- exact_extract(Bokspits_3_Predict,Bokspits_3_WV2_grid_SF,"mode")
names(Bokspits_3_WV2_Extract) <- c('mode')
Bokspits_3_WV2_Extract_DF <-bind_cols(Bokspits_3_WV2_grid_SF,Bokspits_3_WV2_Extract)

Bokspits_3_WV2_Extract <- exact_extract(Bokspits_3_Predict,Bokspits_3_WV2_grid_SF,"majority" )
names(Bokspits_3_WV2_Extract) <- c('Type')
Bokspits_3_WV2_Extract_DF <- dplyr::mutate(Bokspits_3_WV2_Extract_DF, Type = Bokspits_3_WV2_Extract)

Bokspits_3_WV2_Extract <- exact_extract(Bokspits_3_Predict,Bokspits_3_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_3_WV2_Extract_DF <- dplyr::mutate(Bokspits_3_WV2_Extract_DF, Bokspits_3_WV2_Extract)


#----7. Read in files for Struizendam_1 etc Classified image data ----


Struizendam_1_Predict <- rast("data_out/Struizendam_1/Struizendam_1_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Struizendam_1_WV2_grid_SF <- read_sf(dsn = 'data_in/WV2_Grids', layer = "Struizendam_1_WV2_grid")


#---8. Extract data for Struizendam_1 Grid


Struizendam_1_WV2_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_WV2_grid_SF,"mode")
names(Struizendam_1_WV2_Extract) <- c('mode')
Struizendam_1_WV2_Extract_DF <-bind_cols(Struizendam_1_WV2_grid_SF,Struizendam_1_WV2_Extract)

Struizendam_1_WV2_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_WV2_grid_SF,"majority" )
names(Struizendam_1_WV2_Extract) <- c('Type')
Struizendam_1_WV2_Extract_DF <- dplyr::mutate(Struizendam_1_WV2_Extract_DF, Type = Struizendam_1_WV2_Extract)

Struizendam_1_WV2_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_1_WV2_Extract_DF <- dplyr::mutate(Struizendam_1_WV2_Extract_DF, Struizendam_1_WV2_Extract)


#----9. Read in files for Struizendam_2 etc Classified image data ----


Struizendam_2_Predict <- rast("data_out/Struizendam_2/Struizendam_2_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Struizendam_2_WV2_grid_SF <- read_sf(dsn = 'data_in/WV2_Grids', layer = "Struizendam_2_WV2_grid")


#---10. Extract data for Struizendam_2 Grid


Struizendam_2_WV2_Extract <- exact_extract(Struizendam_2_Predict,Struizendam_2_WV2_grid_SF,"mode")
names(Struizendam_2_WV2_Extract) <- c('mode')
Struizendam_2_WV2_Extract_DF <-bind_cols(Struizendam_2_WV2_grid_SF,Struizendam_2_WV2_Extract)

Struizendam_2_WV2_Extract <- exact_extract(Struizendam_2_Predict,Struizendam_2_WV2_grid_SF,"majority" )
names(Struizendam_2_WV2_Extract) <- c('Type')
Struizendam_2_WV2_Extract_DF <- dplyr::mutate(Struizendam_2_WV2_Extract_DF, Type = Struizendam_2_WV2_Extract)

Struizendam_2_WV2_Extract <- exact_extract(Struizendam_2_Predict,Struizendam_2_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_2_WV2_Extract_DF <- dplyr::mutate(Struizendam_2_WV2_Extract_DF, Struizendam_2_WV2_Extract)

#----11. Read in files for Struizendam_3 etc Classified image data ----


Struizendam_3_Predict <- rast("data_out/Struizendam_3/Struizendam_3_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Struizendam_3_WV2_grid_SF <- read_sf(dsn = 'data_in/WV2_Grids', layer = "Struizendam_3_WV2_grid")


#---12. Extract data for Struizendam_3 Grid


Struizendam_3_WV2_Extract <- exact_extract(Struizendam_3_Predict,Struizendam_3_WV2_grid_SF,"mode")
names(Struizendam_3_WV2_Extract) <- c('mode')
Struizendam_3_WV2_Extract_DF <-bind_cols(Struizendam_3_WV2_grid_SF,Struizendam_3_WV2_Extract)

Struizendam_3_WV2_Extract <- exact_extract(Struizendam_3_Predict,Struizendam_3_WV2_grid_SF,"majority" )
names(Struizendam_3_WV2_Extract) <- c('Type')
Struizendam_3_WV2_Extract_DF <- dplyr::mutate(Struizendam_3_WV2_Extract_DF, Type = Struizendam_3_WV2_Extract)

Struizendam_3_WV2_Extract <- exact_extract(Struizendam_3_Predict,Struizendam_3_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_3_WV2_Extract_DF <- dplyr::mutate(Struizendam_3_WV2_Extract_DF, Struizendam_3_WV2_Extract)

#----13. Read in files for Struizendam_4 etc Classified image data ----


Struizendam_4_Predict <- rast("data_out/Struizendam_4/Struizendam_4_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Struizendam_4_WV2_grid_SF <- read_sf(dsn = 'data_in/WV2_Grids', layer = "Struizendam_4_WV2_grid")


#---14. Extract data for Struizendam_4 Grid


Struizendam_4_WV2_Extract <- exact_extract(Struizendam_4_Predict,Struizendam_4_WV2_grid_SF,"mode")
names(Struizendam_4_WV2_Extract) <- c('mode')
Struizendam_4_WV2_Extract_DF <-bind_cols(Struizendam_4_WV2_grid_SF,Struizendam_4_WV2_Extract)

Struizendam_4_WV2_Extract <- exact_extract(Struizendam_4_Predict,Struizendam_4_WV2_grid_SF,"majority" )
names(Struizendam_4_WV2_Extract) <- c('Type')
Struizendam_4_WV2_Extract_DF <- dplyr::mutate(Struizendam_4_WV2_Extract_DF, Type = Struizendam_4_WV2_Extract)

Struizendam_4_WV2_Extract <- exact_extract(Struizendam_4_Predict,Struizendam_4_WV2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_4_WV2_Extract_DF <- dplyr::mutate(Struizendam_4_WV2_Extract_DF, Struizendam_4_WV2_Extract)

#-----15. Combine data frames from each drone survey into one training data set for all survey areas

WV2_full_Extract_DF <- bind_rows(Bokspits_1_WV2_Extract_DF, Bokspits_2_WV2_Extract_DF,Bokspits_3_WV2_Extract_DF, Struizendam_1_WV2_Extract_DF, Struizendam_2_WV2_Extract_DF,Struizendam_3_WV2_Extract_DF,Struizendam_4_WV2_Extract_DF)
#need to replace NA with zeros

WV2_full_Extract_DF[is.na(WV2_full_Extract_DF)] <- 0

saveRDS(WV2_full_Extract_DF, "data_in/WV2e/WV2_pixel_extract_Full_DF.rds")
st_write(WV2_full_Extract_DF, "data_in/WV2e/WV2_pixel_extract_Full_DF.shp", append=FALSE)

#----16.  Select Parameters for defining training 


WV2_full_Extract_DF <-readRDS("data_in/WV2e/WV2_pixel_extract_Full_DF.rds")


#Need to define all frac_ in final version

#WV2_full_Extract_DF_99<- WV2_full_Extract_DF %>% filter_at(vars(frac_1,frac_2,frac_3,frac_5,frac_6,frac_7,frac_10), any_vars(. > 0.99))
#WV2_full_Extract_DF_P <- WV2_full_WV2_Extract_DF %>% filter_at(vars(frac_1), any_vars(. > paste0(P)))


WV2_full_Extract_DF_99_1 <- WV2_full_Extract_DF %>% filter (frac_1 > 0.99)
WV2_full_Extract_DF_99_2 <- WV2_full_Extract_DF %>% filter (frac_2 > 0.99)
WV2_full_Extract_DF_99_3 <- WV2_full_Extract_DF  %>% filter (frac_3 > 0.99)
WV2_full_Extract_DF_99_5 <- WV2_full_Extract_DF %>% filter (frac_5 > 0.99)
WV2_full_Extract_DF_99_6 <- WV2_full_Extract_DF  %>% filter (frac_6 > 0.99)
WV2_full_Extract_DF_99_7 <- WV2_full_Extract_DF  %>% filter (frac_7 > 0.99)
WV2_full_Extract_DF_99_10 <- WV2_full_Extract_DF %>% filter (frac_10 > 0.99)

WV2_full_train <- bind_rows(WV2_full_Extract_DF_99_1,WV2_full_Extract_DF_99_2,WV2_full_Extract_DF_99_3,WV2_full_Extract_DF_99_5,WV2_full_Extract_DF_99_6,WV2_full_Extract_DF_99_7)

saveRDS(WV2_full_train, "data_in/WV2e/WV2_pixel_extract_full_train_99.rds")
st_write(WV2_full_train, "data_in/WV2e/WV2_pixel_extract_full_train_99.shp", append=FALSE)


# Sampling the full training set to have equal class size of N

WV2_full_Extract_DF_99_1e <-WV2_full_Extract_DF_99_1 [ sample( which( WV2_full_Extract_DF_99_1$Type == "1" ) ,N ) , ]
WV2_full_Extract_DF_99_2e <-WV2_full_Extract_DF_99_2[ sample( which( WV2_full_Extract_DF_99_2$Type == "2" ) ,N) , ]
WV2_full_Extract_DF_99_3e <-WV2_full_Extract_DF_99_3[ sample( which( WV2_full_Extract_DF_99_3$Type == "3" ) ,N) , ]
WV2_full_Extract_DF_99_5e <-WV2_full_Extract_DF_99_5[ sample( which( WV2_full_Extract_DF_99_5$Type == "5" ) , N) , ]
WV2_full_Extract_DF_99_6e <-WV2_full_Extract_DF_99_6[ sample( which( WV2_full_Extract_DF_99_6$Type == "6" ) ,N ), ]
WV2_full_Extract_DF_99_7e <-WV2_full_Extract_DF_99_7[ sample( which( WV2_full_Extract_DF_99_7$Type == "7" ) ,N) , ]

WV2_full_traine <- bind_rows(WV2_full_Extract_DF_99_1e,WV2_full_Extract_DF_99_2e,WV2_full_Extract_DF_99_3e,WV2_full_Extract_DF_99_5e,WV2_full_Extract_DF_99_6e,WV2_full_Extract_DF_99_7e)

WV2_full_traine <- dplyr::select(WV2_full_traine,- frac_4)

WV2_full_traine <- dplyr::select(WV2_full_traine,- frac_8)
WV2_full_traine <- dplyr::select(WV2_full_traine,- frac_10)
#WV2_full_traine <- dplyr::select(WV2_full_traine,- frac_13)
WV2_full_traine <- dplyr::select(WV2_full_traine,- ...3)



saveRDS(WV2_full_traine, "data_in/WV2e/WV2_equal_class_size_30_train_99.rds")
st_write(WV2_full_traine, "data_in/WV2e/WV2_equal_class_size_30_train_99.shp", append=FALSE)

WV2_full_trainx=WV2_full_traine
WV2_full_trainx <- dplyr::select(WV2_full_trainx,- frac_1)
WV2_full_trainx <- dplyr::select(WV2_full_trainx,- frac_2)
WV2_full_trainx <- dplyr::select(WV2_full_trainx,- frac_3)
WV2_full_trainx <- dplyr::select(WV2_full_trainx,- frac_5)
WV2_full_trainx <- dplyr::select(WV2_full_trainx,- frac_6)
WV2_full_trainx <- dplyr::select(WV2_full_trainx,- frac_7)

saveRDS(WV2_full_trainx, "data_in/WV2e/WV2_equal_class_size_30_train_99_clean.rds")
st_write(WV2_full_trainx, "data_in/WV2e/WV2_equal_class_size_30_train_99_clean.shp", append=FALSE)



# ----- 16b making simpler training classes - Prosopis, sand, grass, other woody vegetation

DFX <-readRDS("data_in/WV2e/WV2_equal_class_size_30_train_99_clean.rds")

DFX <- DFX %>%                               # Replacing values
  mutate(Type = replace(Type, Type == 5, 6))

DFX <- DFX %>%                               # Replacing values
  mutate(Type = replace(Type, Type == 7, 6))

DFX_6 <-DFX[ sample( which( DFX$Type == "6" ) ,30 ), ]
DFX_1 <-DFX[ sample( which( DFX$Type == "1" ) ,30 ), ]
DFX_2 <-DFX[ sample( which( DFX$Type == "2" ) ,30 ), ]
DFX_3 <-DFX[ sample( which( DFX$Type == "3" ) ,30 ), ]

DFX_F <- bind_rows(DFX_1,DFX_2,DFX_3,DFX_6)

saveRDS(DFX_F, "data_in/WV2e/WV2_equal_class_size_30_train_99_simple.rds")
st_write(DFX_F, "data_in/WV2e/WV2_equal_class_size_30_train_99_simple.shp", append=FALSE)


#------17. Making mixed pixel training classes to try ..... -------------

#Making a High Density Bush (HD) class with >65% RT and the rest bare sand and grass

WV2_full_Extract_DF_H1<- filter(WV2_full_Extract_DF,frac_6 <0.94)
WV2_full_Extract_DF_H2<- filter(WV2_full_Extract_DF_H1,frac_6 >0.65)
WV2_full_Extract_DF_H3<- filter(WV2_full_Extract_DF_H2,frac_1 <0.02)
WV2_full_Extract_DF_H4<- filter(WV2_full_Extract_DF_H3,frac_5 <0.02)
WV2_full_Extract_DF_H5<- filter(WV2_full_Extract_DF_H4,frac_7 <0.02)
WV2_full_Extract_DF_H6<- filter(WV2_full_Extract_DF_H5,frac_4 <0.02)
WV2_full_Extract_DF_H7<- filter(WV2_full_Extract_DF_H6,frac_13 <0.02)
WV2_full_Extract_DF_HD<- filter(WV2_full_Extract_DF_H7,frac_10 <0.02)


# Majority value changed to 21 for high density RT  class

WV2_full_Extract_DF_HD <- WV2_full_Extract_DF_HD%>% mutate(majority = replace(majority,majority<=13,21))

WV2_full_Extract_DF_HD <-WV2_full_Extract_DF_HD[ sample( which( WV2_full_Extract_DF_HD$majority == "21" ) , N), ]


#Making a Medium  Density Bush (MD) class with >35% <65% RT and the rest bare sand and grass

WV2_full_Extract_DF_M1<- filter(WV2_full_Extract_DF,frac_6 <0.64)
WV2_full_Extract_DF_M2<- filter(WV2_full_Extract_DF_M1,frac_6 >0.35)
WV2_full_Extract_DF_M3<- filter(WV2_full_Extract_DF_M2,frac_1 <0.02)
WV2_full_Extract_DF_M4<- filter(WV2_full_Extract_DF_M3,frac_5 <0.02)
WV2_full_Extract_DF_M5<- filter(WV2_full_Extract_DF_M4,frac_7 <0.02)
WV2_full_Extract_DF_M6<- filter(WV2_full_Extract_DF_M5,frac_4 <0.02)
WV2_full_Extract_DF_M7<- filter(WV2_full_Extract_DF_M6,frac_13 <0.02)
WV2_full_Extract_DF_MD<- filter(WV2_full_Extract_DF_M7,frac_10 <0.02)

# Majority value changed to 31 for high density RT  class

WV2_full_Extract_DF_MD <- WV2_full_Extract_DF_MD%>% mutate(majority = replace(majority,majority<=13,31))

WV2_full_Extract_DF_MD <-WV2_full_Extract_DF_MD[ sample( which( WV2_full_Extract_DF_MD$majority == "31" ) , N ), ]


#Making a Low Density Bush (MD) class with >35% <65% RT and the rest bare sand and grass


WV2_full_Extract_DF_L1<- filter(WV2_full_Extract_DF,frac_6 <0.34)
WV2_full_Extract_DF_L2<- filter(WV2_full_Extract_DF_L1,frac_6 >0.05)
WV2_full_Extract_DF_L3<- filter(WV2_full_Extract_DF_L2,frac_1 <0.02)
WV2_full_Extract_DF_L4<- filter(WV2_full_Extract_DF_L3,frac_5 <0.02)
WV2_full_Extract_DF_L5<- filter(WV2_full_Extract_DF_L4,frac_7 <0.02)
WV2_full_Extract_DF_L6<- filter(WV2_full_Extract_DF_L5,frac_4 <0.02)
WV2_full_Extract_DF_L7<- filter(WV2_full_Extract_DF_L6,frac_13 <0.02)
WV2_full_Extract_DF_L8<- filter(WV2_full_Extract_DF_L7,frac_3 <0.94)
WV2_full_Extract_DF_L9<- filter(WV2_full_Extract_DF_L8,frac_2 <0.94)
WV2_full_Extract_DF_LD<- filter(WV2_full_Extract_DF_L9,frac_10 <0.02)


# Majority value cLanged to 41 for Low density RT  class

WV2_full_Extract_DF_LD <- WV2_full_Extract_DF_LD%>% mutate(majority = replace(majority,majority<=13,41))

WV2_full_Extract_DF_LD <-WV2_full_Extract_DF_LD[ sample( which( WV2_full_Extract_DF_LD$majority == "41" ) , N ), ]

#maje new training df with mixed classes added

WV2_full_train_M<- bind_rows(WV2_full_traine,WV2_full_Extract_DF_LD,WV2_full_Extract_DF_MD,WV2_full_Extract_DF_HD)

saveRDS(WV2_full_train_M, "data_in/WV2/WV2_equal_class_size_",paste0(N),"_train_and_mixed.rds")

st_write(WV2_full_train_M, dsn = "data_in/WV2/WV2_equal_class_size_",paste0(N),"_train_and_mixed.shp")


