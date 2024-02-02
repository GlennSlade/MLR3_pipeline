## Script to read in and manipulate MLr3 results files for benchmarking
##
library(tidyverse)
library(openxlsx)
library(viridis)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(MASS)
library(splines)
library(gridExtra)
library(DescTools)
library(sf)
library(writexl)
library(cowplot)
library (dplyr)
library(writexl)
#library(diffeR)

## Plotting theme
theme_fancy <- function() {
  theme_bw() +
    theme(
      text = element_text(family = "Helvetica"),
      axis.text = element_text(size = 8, color = "black"),
      axis.title = element_text(size = 8, color = "black"),
      axis.line.x = element_line(size = 0.3, color = "black"),
      axis.line.y = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(size = 0.3, color = "black"),
      panel.border = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
      plot.title = element_text(
        size = 8,
        vjust = 1,
        hjust = 0.5,
        color = "black"
      ),
      legend.text = element_text(size = 8, color = "black"),
      legend.title = element_text(size = 8, color = "black"),
      legend.position = c(0.9, 0.9),
      legend.key.size = unit(0.9, "line"),
      legend.background = element_rect(
        color = "black",
        fill = "transparent",
        size = 2,
        linetype = "blank"
      )
    )
}
#windowsFonts("Helvetica" = windowsFont("Helvetica")) # Ensure font is mapped correctly

# Bokspits_1 Confusion data 
df <- read.xlsx("data_out/Confusion/res.preds_svm_Bokspits_1.xlsx")
df <- df[,-1]
#Confusion2 <- as.matrix(df)
#p <- categorySourcesPlot(ctmatrix=Confusion2)
#p2 <- p + theme_fancy()+ ggtitle("Bokspits_1 Classification results - using all input data") 
#p2

prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[5]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['5',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Bokspits_1")
df2 <- df2 %>%  mutate(input_data = "5_CHM_ALLVI")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)


df_master <- df2

df <- read.xlsx("data_out/Confusion/res.preds_svm_Bokspits_1_5.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[5]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['5',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Bokspits_1")
df2 <- df2 %>%  mutate(input_data = "5")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Bokspits_1_5_CHM.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[5]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['5',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Bokspits_1")
df2 <- df2 %>%  mutate(input_data = "5_CHM")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Bokspits_1_5_CHM_NDVI.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[5]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['5',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Bokspits_1")
df2 <- df2 %>%  mutate(input_data = "5_CHM_NDVI")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

# Bokspits_2 Confusion data 
df <- read.xlsx("data_out/Confusion/res.preds_svm_Bokspits_2.xlsx")
df <- df[,-1]
#Confusion2 <- as.matrix(df)
#p <- categorySourcesPlot(ctmatrix=Confusion2)
#p2 <- p + theme_fancy()+ ggtitle("Bokspits_2 Classification results - using all input data") 
#p2

prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[5]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['5',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Bokspits_2")
df2 <- df2 %>%  mutate(input_data = "5_CHM_ALLVI")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)


df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Bokspits_2_5.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[5]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['5',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Bokspits_2")
df2 <- df2 %>%  mutate(input_data = "5")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Bokspits_2_5_CHM.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[5]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['5',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Bokspits_2")
df2 <- df2 %>%  mutate(input_data = "5_CHM")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Bokspits_2_5_CHM_NDVI.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[5]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['5',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Bokspits_2")
df2 <- df2 %>%  mutate(input_data = "5_CHM_NDVI")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

# Bokspits_3
df <- read.xlsx("data_out/Confusion/res.preds_svm_Bokspits_3.xlsx")
df <- df[,-1]
#Confusion2 <- as.matrix(df)
#p <- categorySourcesPlot(ctmatrix=Confusion2)
#p2 <- p + theme_fancy()+ ggtitle("Bokspits_3 Classification results - using all input data") 
#p2

prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[5]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['5',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Bokspits_3")
df2 <- df2 %>%  mutate(input_data = "5_CHM_ALLVI")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)


df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Bokspits_3_5.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[5]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['5',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Bokspits_3")
df2 <- df2 %>%  mutate(input_data = "5")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Bokspits_3_5_CHM.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[5]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['5',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Bokspits_3")
df2 <- df2 %>%  mutate(input_data = "5_CHM")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Bokspits_3_5_CHM_NDVI.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[5]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['5',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Bokspits_3")
df2 <- df2 %>%  mutate(input_data = "5_CHM_NDVI")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

# Struizendam_1
df <- read.xlsx("data_out/Confusion/res.preds_svm_Struizendam_1.xlsx")
df <- df[,-1]
#Confusion2 <- as.matrix(df)
#p <- categorySourcesPlot(ctmatrix=Confusion2)
#p2 <- p + theme_fancy()+ ggtitle("Struizendam_1 Classification results - using all input data") 
#p2

prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[4]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['4',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Struizendam_1")
df2 <- df2 %>%  mutate(input_data = "5_CHM_ALLVI")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)


df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Struizendam_1_5.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[4]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['4',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Struizendam_1")
df2 <- df2 %>%  mutate(input_data = "5")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Struizendam_1_5_CHM.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[4]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['4',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Struizendam_1")
df2 <- df2 %>%  mutate(input_data = "5_CHM")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Struizendam_1_5_CHM_NDVI.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[4]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['4',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Struizendam_1")
df2 <- df2 %>%  mutate(input_data = "5_CHM_NDVI")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

# Struizendam_2
df <- read.xlsx("data_out/Confusion/res.preds_svm_Struizendam_2.xlsx")
df <- df[,-1]
#Confusion2 <- as.matrix(df)
#p <- categorySourcesPlot(ctmatrix=Confusion2)
#p2 <- p + theme_fancy()+ ggtitle("Struizendam_2 Classification results - using all input data") 
#p2

prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[4]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['4',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Struizendam_2")
df2 <- df2 %>%  mutate(input_data = "5_CHM_ALLVI")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)


df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Struizendam_2_5.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[4]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['4',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Struizendam_2")
df2 <- df2 %>%  mutate(input_data = "5")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Struizendam_2_5_CHM.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[4]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['4',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Struizendam_2")
df2 <- df2 %>%  mutate(input_data = "5_CHM")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Struizendam_2_5_CHM_NDVI.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[4]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['4',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Struizendam_2")
df2 <- df2 %>%  mutate(input_data = "5_CHM_NDVI")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

# Struizendam_3
df <- read.xlsx("data_out/Confusion/res.preds_svm_Struizendam_3.xlsx")
df <- df[,-1]
#Confusion2 <- as.matrix(df)
#p <- categorySourcesPlot(ctmatrix=Confusion2)
#p2 <- p + theme_fancy()+ ggtitle("Struizendam_3 Classification results - using all input data") 
#p2

prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  



df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Struizendam_3")
df2 <- df2 %>%  mutate(input_data = "5_CHM_ALLVI")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)



df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Struizendam_3_5.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  



df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Struizendam_3")
df2 <- df2 %>%  mutate(input_data = "5")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)

df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Struizendam_3_5_CHM.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  



df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Struizendam_3")
df2 <- df2 %>%  mutate(input_data = "5_CHM")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)

df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Struizendam_3_5_CHM_NDVI.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  



df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Struizendam_3")
df2 <- df2 %>%  mutate(input_data = "5_CHM_NDVI")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)

df_master<- bind_rows(df2,df_master)

# Struizendam_4
df <- read.xlsx("data_out/Confusion/res.preds_svm_Struizendam_4.xlsx")
df <- df[,-1]
#Confusion2 <- as.matrix(df)
#p <- categorySourcesPlot(ctmatrix=Confusion2)
#p2 <- p + theme_fancy()+ ggtitle("Struizendam_4 Classification results - using all input data") 
#p2

prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[3]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['3',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Struizendam_4")
df2 <- df2 %>%  mutate(input_data = "5_CHM_ALLVI")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)


df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Struizendam_4_5.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[3]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['3',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Struizendam_4")
df2 <- df2 %>%  mutate(input_data = "5")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Struizendam_4_5_CHM.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[3]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['3',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Struizendam_4")
df2 <- df2 %>%  mutate(input_data = "5_CHM")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_Struizendam_4_5_CHM_NDVI.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[3]
c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['3',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "Struizendam_4")
df2 <- df2 %>%  mutate(input_data = "5_CHM_NDVI")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_30_99_WV2e.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[4]

c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['4',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "WV2")
df2 <- df2 %>%  mutate(input_data = "30_99")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_400_95_WV2e.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[4]

c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['4',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "WV2")
df2 <- df2 %>%  mutate(input_data = "400_95")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_WV2.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`5`[4]

c <-sum(df$`5`)  
CT_user <- CT/c

row <-df['4',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "WV2")
df2 <- df2 %>%  mutate(input_data = "point")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)

df <- read.xlsx("data_out/Confusion/res.preds_svm_30_99_simple_WV2e.xlsx")
df <- df[,-1]
prosopis <- df$`1`[1]
c <-sum(df$`1`)  
Pros_user <- prosopis/c

row <-df['1',]
r <- sum(row)  
Pros_prod <- prosopis/r  

CT <- df$`6`[4]

c <-sum(df$`6`)  
CT_user <- CT/c

row <-df['4',]
r <- sum(row)  
CT_prod <- CT/r 

df2 <-  tibble(id = 1)
df2 <- df2 %>%  mutate(survey = "WV2")
df2 <- df2 %>%  mutate(input_data = "30_90_simple")
df2 <- df2 %>%  mutate(Pros_user = Pros_user)
df2 <- df2 %>% mutate(Pros_prod = Pros_prod)
df2 <- df2 %>%  mutate(CT_user = CT_user)
df2 <- df2 %>% mutate(CT_prod = CT_prod)
df_master<- bind_rows(df2,df_master)


write.xlsx(df_master, "data_out/Confusion/confusion_master.xlsx")
