### Script to Extract Data from Drone Classifications based on S2 pixel grid
### matching pixels
### Imports classified drone image data set and calculates fractional cover for each 
### class in each S2 pixel and then samples that data to produce training data set
### for classification of S2 image.

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
A = "0.85"

#Fraction  cover to use to define training class for Prosopis 1.0 = 100%
#Change this to vary the pixel definitions
P = "0.85"

#Number of pixels from each Class to use in training data

N = "60"


#----1. Read in files for Bokspits_1 etc Classified image data ----


Bokspits_1_Predict <- rast("data_out/Bokspits_1/Bokspits_1_Predict_Map.tif")
Bokspits_1_Predict

## NB You cant use exact extract with SpatVector so importing it a a spatial DF

Bokspits_1_S2_grid_SF <- read_sf(dsn = 'data_in/S2_Grids', layer = "Bokspits_1_S2_grid")
Bokspits_1_S2_grid_SF



#---2. Extract data for Bokspits_1 Grid


Bokspits_1_S2_Extract <- exact_extract(Bokspits_1_Predict,Bokspits_1_S2_grid_SF,"mode")
names(Bokspits_1_S2_Extract) <- c('mode')
Bokspits_1_S2_Extract_DF <-bind_cols(Bokspits_1_S2_grid_SF,Bokspits_1_S2_Extract)

Bokspits_1_S2_Extract <- exact_extract(Bokspits_1_Predict,Bokspits_1_S2_grid_SF,"majority" )
names(Bokspits_1_S2_Extract) <- c('Type')
Bokspits_1_S2_Extract_DF <- dplyr::mutate(Bokspits_1_S2_Extract_DF, Type = Bokspits_1_S2_Extract)

Bokspits_1_S2_Extract <- exact_extract(Bokspits_1_Predict,Bokspits_1_S2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_1_S2_Extract_DF <- dplyr::mutate(Bokspits_1_S2_Extract_DF, Bokspits_1_S2_Extract)

#----3. Read in files for Bokspits_2 etc Classified image data ----


Bokspits_2_Predict <- rast("data_out/Bokspits_2/Bokspits_2_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Bokspits_2_S2_grid_SF <- read_sf(dsn = 'data_in/S2_Grids', layer = "Bokspits_2_S2_grid")


#---4.. Extract data for Bokspits_2 Grid


Bokspits_2_S2_Extract <- exact_extract(Bokspits_2_Predict,Bokspits_2_S2_grid_SF,"mode")
names(Bokspits_2_S2_Extract) <- c('mode')
Bokspits_2_S2_Extract_DF <-bind_cols(Bokspits_2_S2_grid_SF,Bokspits_2_S2_Extract)

Bokspits_2_S2_Extract <- exact_extract(Bokspits_2_Predict,Bokspits_2_S2_grid_SF,"majority" )
names(Bokspits_2_S2_Extract) <- c('Type')
Bokspits_2_S2_Extract_DF <- dplyr::mutate(Bokspits_2_S2_Extract_DF, Type = Bokspits_2_S2_Extract)

Bokspits_2_S2_Extract <- exact_extract(Bokspits_2_Predict,Bokspits_2_S2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_2_S2_Extract_DF <- dplyr::mutate(Bokspits_2_S2_Extract_DF, Bokspits_2_S2_Extract)

#----5. Read in files for Bokspits_3 etc Classified image data ----


Bokspits_3_Predict <- rast("data_out/Bokspits_3/Bokspits_3_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Bokspits_3_S2_grid_SF <- read_sf(dsn = 'data_in/S2_Grids', layer = "Bokspits_3_S2_grid")


#---6. Extract data for Bokspits_3 Grid


Bokspits_3_S2_Extract <- exact_extract(Bokspits_3_Predict,Bokspits_3_S2_grid_SF,"mode")
names(Bokspits_3_S2_Extract) <- c('mode')
Bokspits_3_S2_Extract_DF <-bind_cols(Bokspits_3_S2_grid_SF,Bokspits_3_S2_Extract)

Bokspits_3_S2_Extract <- exact_extract(Bokspits_3_Predict,Bokspits_3_S2_grid_SF,"majority" )
names(Bokspits_3_S2_Extract) <- c('Type')
Bokspits_3_S2_Extract_DF <- dplyr::mutate(Bokspits_3_S2_Extract_DF, Type = Bokspits_3_S2_Extract)

Bokspits_3_S2_Extract <- exact_extract(Bokspits_3_Predict,Bokspits_3_S2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Bokspits_3_S2_Extract_DF <- dplyr::mutate(Bokspits_3_S2_Extract_DF, Bokspits_3_S2_Extract)


#----7. Read in files for Struizendam_1 etc Classified image data ----


Struizendam_1_Predict <- rast("data_out/Struizendam_1/Struizendam_1_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Struizendam_1_S2_grid_SF <- read_sf(dsn = 'data_in/S2_Grids', layer = "Struizendam_1_S2_grid")


#---8. Extract data for Struizendam_1 Grid


Struizendam_1_S2_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_S2_grid_SF,"mode")
names(Struizendam_1_S2_Extract) <- c('mode')
Struizendam_1_S2_Extract_DF <-bind_cols(Struizendam_1_S2_grid_SF,Struizendam_1_S2_Extract)

Struizendam_1_S2_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_S2_grid_SF,"majority" )
names(Struizendam_1_S2_Extract) <- c('Type')
Struizendam_1_S2_Extract_DF <- dplyr::mutate(Struizendam_1_S2_Extract_DF, Type = Struizendam_1_S2_Extract)

Struizendam_1_S2_Extract <- exact_extract(Struizendam_1_Predict,Struizendam_1_S2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_1_S2_Extract_DF <- dplyr::mutate(Struizendam_1_S2_Extract_DF, Struizendam_1_S2_Extract)


#----9. Read in files for Struizendam_2 etc Classified image data ----


Struizendam_2_Predict <- rast("data_out/Struizendam_2/Struizendam_2_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Struizendam_2_S2_grid_SF <- read_sf(dsn = 'data_in/S2_Grids', layer = "Struizendam_2_S2_grid")


#---10. Extract data for Struizendam_2 Grid


Struizendam_2_S2_Extract <- exact_extract(Struizendam_2_Predict,Struizendam_2_S2_grid_SF,"mode")
names(Struizendam_2_S2_Extract) <- c('mode')
Struizendam_2_S2_Extract_DF <-bind_cols(Struizendam_2_S2_grid_SF,Struizendam_2_S2_Extract)

Struizendam_2_S2_Extract <- exact_extract(Struizendam_2_Predict,Struizendam_2_S2_grid_SF,"majority" )
names(Struizendam_2_S2_Extract) <- c('Type')
Struizendam_2_S2_Extract_DF <- dplyr::mutate(Struizendam_2_S2_Extract_DF, Type = Struizendam_2_S2_Extract)

Struizendam_2_S2_Extract <- exact_extract(Struizendam_2_Predict,Struizendam_2_S2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_2_S2_Extract_DF <- dplyr::mutate(Struizendam_2_S2_Extract_DF, Struizendam_2_S2_Extract)

#----11. Read in files for Struizendam_3 etc Classified image data ----


Struizendam_3_Predict <- rast("data_out/Struizendam_3/Struizendam_3_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Struizendam_3_S2_grid_SF <- read_sf(dsn = 'data_in/S2_Grids', layer = "Struizendam_3_S2_grid")


#---12. Extract data for Struizendam_3 Grid


Struizendam_3_S2_Extract <- exact_extract(Struizendam_3_Predict,Struizendam_3_S2_grid_SF,"mode")
names(Struizendam_3_S2_Extract) <- c('mode')
Struizendam_3_S2_Extract_DF <-bind_cols(Struizendam_3_S2_grid_SF,Struizendam_3_S2_Extract)

Struizendam_3_S2_Extract <- exact_extract(Struizendam_3_Predict,Struizendam_3_S2_grid_SF,"majority" )
names(Struizendam_3_S2_Extract) <- c('Type')
Struizendam_3_S2_Extract_DF <- dplyr::mutate(Struizendam_3_S2_Extract_DF, Type = Struizendam_3_S2_Extract)

Struizendam_3_S2_Extract <- exact_extract(Struizendam_3_Predict,Struizendam_3_S2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_3_S2_Extract_DF <- dplyr::mutate(Struizendam_3_S2_Extract_DF, Struizendam_3_S2_Extract)

#----13. Read in files for Struizendam_4 etc Classified image data ----


Struizendam_4_Predict <- rast("data_out/Struizendam_4/Struizendam_4_Predict_Map.tif")

## NB You cant use exact extract with SpatVector so re-importing it a a spatial DF
Struizendam_4_S2_grid_SF <- read_sf(dsn = 'data_in/S2_Grids', layer = "Struizendam_4_S2_grid")


#---14. Extract data for Struizendam_4 Grid


Struizendam_4_S2_Extract <- exact_extract(Struizendam_4_Predict,Struizendam_4_S2_grid_SF,"mode")
names(Struizendam_4_S2_Extract) <- c('mode')
Struizendam_4_S2_Extract_DF <-bind_cols(Struizendam_4_S2_grid_SF,Struizendam_4_S2_Extract)

Struizendam_4_S2_Extract <- exact_extract(Struizendam_4_Predict,Struizendam_4_S2_grid_SF,"majority" )
names(Struizendam_4_S2_Extract) <- c('Type')
Struizendam_4_S2_Extract_DF <- dplyr::mutate(Struizendam_4_S2_Extract_DF, Type = Struizendam_4_S2_Extract)

Struizendam_4_S2_Extract <- exact_extract(Struizendam_4_Predict,Struizendam_4_S2_grid_SF,"frac" )# calculates fraction cover for each classification value
#a column is added with the FVC for each vegetation type frac1, frac2 etc...
Struizendam_4_S2_Extract_DF <- dplyr::mutate(Struizendam_4_S2_Extract_DF, Struizendam_4_S2_Extract)

#-----15. Combine data frames from each drone survey into one training data set for all survey areas

S2_full_Extract_DF <- bind_rows(Bokspits_1_S2_Extract_DF, Bokspits_2_S2_Extract_DF,Bokspits_3_S2_Extract_DF, Struizendam_1_S2_Extract_DF, Struizendam_2_S2_Extract_DF,Struizendam_3_S2_Extract_DF,Struizendam_4_S2_Extract_DF)
#need to replace NA with zeros

S2_full_Extract_DF[is.na(S2_full_Extract_DF)] <- 0

saveRDS(S2_full_Extract_DF, "data_in/S2/S2_pixel_extract_Full_DF.rds")
st_write(S2_full_Extract_DF, "data_in/S2/S2_pixel_extract_Full_DF.shp", append=FALSE)

#----16.  Select Parameters for defining training 


S2_full_Extract_DF <-readRDS("data_in/S2/S2_pixel_extract_Full_DF.rds")


#Need to define all frac_ in final version

#S2_full_Extract_DF_85<- S2_full_Extract_DF %>% filter_at(vars(frac_1,frac_2,frac_3,frac_5,frac_6,frac_7,frac_10), any_vars(. > 0.85))
#S2_full_Extract_DF_P <- S2_full_S2_Extract_DF %>% filter_at(vars(frac_1), any_vars(. > paste0(P)))


S2_full_Extract_DF_85_1 <- S2_full_Extract_DF %>% filter (frac_1 > 0.75)
S2_full_Extract_DF_85_2 <- S2_full_Extract_DF %>% filter (frac_2 > 0.75)
S2_full_Extract_DF_85_3 <- S2_full_Extract_DF  %>% filter (frac_3 > 0.75)
S2_full_Extract_DF_85_5 <- S2_full_Extract_DF %>% filter (frac_5 > 0.75)
S2_full_Extract_DF_85_6 <- S2_full_Extract_DF  %>% filter (frac_6 > 0.55)
S2_full_Extract_DF_85_7 <- S2_full_Extract_DF  %>% filter (frac_7 > 0.75)
S2_full_Extract_DF_85_10 <- S2_full_Extract_DF %>% filter (frac_10 > 0.75)

S2_full_train <- bind_rows(S2_full_Extract_DF_85_1,S2_full_Extract_DF_85_2,S2_full_Extract_DF_85_3,S2_full_Extract_DF_85_5,S2_full_Extract_DF_85_6,S2_full_Extract_DF_85_7)

saveRDS(S2_full_train, "data_in/S2/S2_pixel_extract_full_train_75.rds")
st_write(S2_full_train, "data_in/S2/S2_pixel_extract_full_train_75.shp", append=FALSE)


# Sampling the full training set to have equal class size of N

S2_full_Extract_DF_85_1e <-S2_full_Extract_DF_85_1 [ sample( which( S2_full_Extract_DF_85_1$Type == "1" ) ,N ) , ]
S2_full_Extract_DF_85_2e <-S2_full_Extract_DF_85_2[ sample( which( S2_full_Extract_DF_85_2$Type == "2" ) ,N) , ]
S2_full_Extract_DF_85_3e <-S2_full_Extract_DF_85_3[ sample( which( S2_full_Extract_DF_85_3$Type == "3" ) ,N) , ]
#S2_full_Extract_DF_85_5e <-S2_full_Extract_DF_85_5[ sample( which( S2_full_Extract_DF_85_5$Type == "5" ) , N) , ]
S2_full_Extract_DF_85_6e <-S2_full_Extract_DF_85_6[ sample( which( S2_full_Extract_DF_85_6$Type == "6" ) ,N ), ]
S2_full_Extract_DF_85_7e <-S2_full_Extract_DF_85_7[ sample( which( S2_full_Extract_DF_85_7$Type == "7" ) ,N) , ]

S2_full_traine <- bind_rows(S2_full_Extract_DF_85_1e,S2_full_Extract_DF_85_2e,S2_full_Extract_DF_85_3e,S2_full_Extract_DF_85_6e)#,S2_full_Extract_DF_85_7e)

S2_full_traine <- dplyr::select(S2_full_traine,- frac_4)

S2_full_traine <- dplyr::select(S2_full_traine,- frac_8)
S2_full_traine <- dplyr::select(S2_full_traine,- frac_10)
S2_full_traine <- dplyr::select(S2_full_traine,- frac_7)
S2_full_traine <- dplyr::select(S2_full_traine,- frac_5)
S2_full_traine <- dplyr::select(S2_full_traine,- ...3)

#S2_full_traine <- dplyr::select(S2_full_traine,- blue)
#S2_full_traine <- dplyr::select(S2_full_traine,- green)
#S2_full_traine <- dplyr::select(S2_full_traine,- red)
#S2_full_traine <- dplyr::select(S2_full_traine,- nir)

S2_full_traine <- mutate_all(S2_full_traine, ~replace_na(.,0))


saveRDS(S2_full_traine, "data_in/S2/S2_equal_class_size_60_train_65.rds")
st_write(S2_full_traine, "data_in/S2/S2_equal_class_size_60_train_65.shp", append=FALSE)

S2_full_trainx=S2_full_traine
S2_full_trainx <- dplyr::select(S2_full_trainx,- frac_1)
S2_full_trainx <- dplyr::select(S2_full_trainx,- frac_2)
S2_full_trainx <- dplyr::select(S2_full_trainx,- frac_3)
#S2_full_trainx <- dplyr::select(S2_full_trainx,- frac_5)
S2_full_trainx <- dplyr::select(S2_full_trainx,- frac_6)
#S2_full_trainx <- dplyr::select(S2_full_trainx,- frac_7)

saveRDS(S2_full_trainx, "data_in/S2/S2_equal_class_size_60_train_65_clean.rds")
st_write(S2_full_trainx, "data_in/S2/S2_equal_class_size_60_train_65_clean.shp", append=FALSE)



# ----- 16b making simpler training classes - Prosopis, sand, grass, other woody vegetation

DFX <-readRDS("data_in/S2/S2_equal_class_size_400_train_85_clean.rds")

DFX <- DFX %>%                               # Replacing values
  mutate(Type = replace(Type, Type == 5, 6))

DFX <- DFX %>%                               # Replacing values
  mutate(Type = replace(Type, Type == 7, 6))

DFX_6 <-DFX[ sample( which( DFX$Type == "6" ) ,400 ), ]
DFX_1 <-DFX[ sample( which( DFX$Type == "1" ) ,400 ), ]
DFX_2 <-DFX[ sample( which( DFX$Type == "2" ) ,400 ), ]
DFX_3 <-DFX[ sample( which( DFX$Type == "3" ) ,400 ), ]

DFX_F <- bind_rows(DFX_1,DFX_2,DFX_3,DFX_6)

saveRDS(DFX_F, "data_in/S2/S2_equal_class_size_400_train_85_simple.rds")
st_write(DFX_F, "data_in/S2/S2_equal_class_size_400_train_85_simple.shp", append=FALSE)


#------17. Making mixed pixel training classes to try ..... -------------

#Making a High Density Bush (HD) class with >65% RT and the rest bare sand and grass

S2_full_Extract_DF_H1<- filter(S2_full_Extract_DF,frac_6 <0.94)
S2_full_Extract_DF_H2<- filter(S2_full_Extract_DF_H1,frac_6 >0.65)
S2_full_Extract_DF_H3<- filter(S2_full_Extract_DF_H2,frac_1 <0.02)
S2_full_Extract_DF_H4<- filter(S2_full_Extract_DF_H3,frac_5 <0.02)
S2_full_Extract_DF_H5<- filter(S2_full_Extract_DF_H4,frac_7 <0.02)
S2_full_Extract_DF_H6<- filter(S2_full_Extract_DF_H5,frac_4 <0.02)
S2_full_Extract_DF_H7<- filter(S2_full_Extract_DF_H6,frac_13 <0.02)
S2_full_Extract_DF_HD<- filter(S2_full_Extract_DF_H7,frac_10 <0.02)


# Majority value changed to 21 for high density RT  class

S2_full_Extract_DF_HD <- S2_full_Extract_DF_HD%>% mutate(majority = replace(majority,majority<=13,21))

S2_full_Extract_DF_HD <-S2_full_Extract_DF_HD[ sample( which( S2_full_Extract_DF_HD$majority == "21" ) , N), ]


#Making a Medium  Density Bush (MD) class with >35% <65% RT and the rest bare sand and grass

S2_full_Extract_DF_M1<- filter(S2_full_Extract_DF,frac_6 <0.64)
S2_full_Extract_DF_M2<- filter(S2_full_Extract_DF_M1,frac_6 >0.35)
S2_full_Extract_DF_M3<- filter(S2_full_Extract_DF_M2,frac_1 <0.02)
S2_full_Extract_DF_M4<- filter(S2_full_Extract_DF_M3,frac_5 <0.02)
S2_full_Extract_DF_M5<- filter(S2_full_Extract_DF_M4,frac_7 <0.02)
S2_full_Extract_DF_M6<- filter(S2_full_Extract_DF_M5,frac_4 <0.02)
S2_full_Extract_DF_M7<- filter(S2_full_Extract_DF_M6,frac_13 <0.02)
S2_full_Extract_DF_MD<- filter(S2_full_Extract_DF_M7,frac_10 <0.02)

# Majority value changed to 31 for high density RT  class

S2_full_Extract_DF_MD <- S2_full_Extract_DF_MD%>% mutate(majority = replace(majority,majority<=13,31))

S2_full_Extract_DF_MD <-S2_full_Extract_DF_MD[ sample( which( S2_full_Extract_DF_MD$majority == "31" ) , N ), ]


#Making a Low Density Bush (MD) class with >35% <65% RT and the rest bare sand and grass


S2_full_Extract_DF_L1<- filter(S2_full_Extract_DF,frac_6 <0.34)
S2_full_Extract_DF_L2<- filter(S2_full_Extract_DF_L1,frac_6 >0.05)
S2_full_Extract_DF_L3<- filter(S2_full_Extract_DF_L2,frac_1 <0.02)
S2_full_Extract_DF_L4<- filter(S2_full_Extract_DF_L3,frac_5 <0.02)
S2_full_Extract_DF_L5<- filter(S2_full_Extract_DF_L4,frac_7 <0.02)
S2_full_Extract_DF_L6<- filter(S2_full_Extract_DF_L5,frac_4 <0.02)
S2_full_Extract_DF_L7<- filter(S2_full_Extract_DF_L6,frac_13 <0.02)
S2_full_Extract_DF_L8<- filter(S2_full_Extract_DF_L7,frac_3 <0.94)
S2_full_Extract_DF_L9<- filter(S2_full_Extract_DF_L8,frac_2 <0.94)
S2_full_Extract_DF_LD<- filter(S2_full_Extract_DF_L9,frac_10 <0.02)


# Majority value cLanged to 41 for Low density RT  class

S2_full_Extract_DF_LD <- S2_full_Extract_DF_LD%>% mutate(majority = replace(majority,majority<=13,41))

S2_full_Extract_DF_LD <-S2_full_Extract_DF_LD[ sample( which( S2_full_Extract_DF_LD$majority == "41" ) , N ), ]

#maje new training df with mixed classes added

S2_full_train_M<- bind_rows(S2_full_traine,S2_full_Extract_DF_LD,S2_full_Extract_DF_MD,S2_full_Extract_DF_HD)

saveRDS(S2_full_train_M, "data_in/S2/S2_equal_class_size_",paste0(N),"_train_and_mixed.rds")

st_write(S2_full_train_M, dsn = "data_in/S2/S2_equal_class_size_",paste0(N),"_train_and_mixed.shp")


