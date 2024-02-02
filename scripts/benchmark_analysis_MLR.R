## Script to read in and manipulate MLr3 results files for benchmarking
##
library(tidyverse)
library(openxlsx)

# Bokspits_1 benchmark data 
df <- read.xlsx("data_out/Bench/MLR_bench_results_Bokspits_1.xlsx")
df <- df %>%  mutate(survey = "Bokspits_1")
df <- df %>%  mutate(input_data = "5_CHM_ALLVI")
df_master <- df

df <- read.xlsx("data_out/Bench/MLR_bench_results_Bokspits_1_5.xlsx")
df <- df %>%  mutate(survey = "Bokspits_1")
df <- df %>%  mutate(input_data = "5")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Bokspits_1_5_CHM.xlsx")
df <- df %>%  mutate(survey = "Bokspits_1")
df <- df %>%  mutate(input_data = "5_CHM")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Bokspits_1_5_CHM_NDVI.xlsx")
df <- df %>%  mutate(survey = "Bokspits_1")
df <- df %>%  mutate(input_data = "5_CHM_NDVI")
df_master<- bind_rows(df,df_master)

# Bokspits_2
df <- read.xlsx("data_out/Bench/MLR_bench_results_Bokspits_2.xlsx")
df <- df %>%  mutate(survey = "Bokspits_2")
df <- df %>%  mutate(input_data = "5_CHM_ALLVI")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Bokspits_2_5.xlsx")
df <- df %>%  mutate(survey = "Bokspits_2")
df <- df %>%  mutate(input_data = "5")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Bokspits_2_5_CHM.xlsx")
df <- df %>%  mutate(survey = "Bokspits_2")
df <- df %>%  mutate(input_data = "5_CHM")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Bokspits_2_5_CHM_NDVI.xlsx")
df <- df %>%  mutate(survey = "Bokspits_2")
df <- df %>%  mutate(input_data = "5_CHM_NDVI")
df_master<- bind_rows(df,df_master)
#Bokspits_3
df <- read.xlsx("data_out/Bench/MLR_bench_results_Bokspits_3.xlsx")
df <- df %>%  mutate(survey = "Bokspits_3")
df <- df %>%  mutate(input_data = "5_CHM_ALLVI")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Bokspits_3_5.xlsx")
df <- df %>%  mutate(survey = "Bokspits_3")
df <- df %>%  mutate(input_data = "5")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Bokspits_3_5_CHM.xlsx")
df <- df %>%  mutate(survey = "Bokspits_3")
df <- df %>%  mutate(input_data = "5_CHM")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Bokspits_3_5_CHM_NDVI.xlsx")
df <- df %>%  mutate(survey = "Bokspits_3")
df <- df %>%  mutate(input_data = "5_CHM_NDVI")
df_master<- bind_rows(df,df_master)

#Struizendam_1
df <- read.xlsx("data_out/Bench/MLR_bench_results_Struizendam_1.xlsx")
df <- df %>%  mutate(survey = "Struizendam_1")
df <- df %>%  mutate(input_data = "5_CHM_ALLVI")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Struizendam_1_5.xlsx")
df <- df %>%  mutate(survey = "Struizendam_1")
df <- df %>%  mutate(input_data = "5")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Struizendam_1_5_CHM.xlsx")
df <- df %>%  mutate(survey = "Struizendam_1")
df <- df %>%  mutate(input_data = "5_CHM")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Struizendam_1_5_CHM_NDVI.xlsx")
df <- df %>%  mutate(survey = "Struizendam_1")
df <- df %>%  mutate(input_data = "5_CHM_NDVI")
df_master<- bind_rows(df,df_master)

#Struizendam_2
df <- read.xlsx("data_out/Bench/MLR_bench_results_Struizendam_2.xlsx")
df <- df %>%  mutate(survey = "Struizendam_2")
df <- df %>%  mutate(input_data = "5_CHM_ALLVI")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Struizendam_2_5.xlsx")
df <- df %>%  mutate(survey = "Struizendam_2")
df <- df %>%  mutate(input_data = "5")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Struizendam_2_5_CHM.xlsx")
df <- df %>%  mutate(survey = "Struizendam_2")
df <- df %>%  mutate(input_data = "5_CHM")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Struizendam_2_5_CHM_NDVI.xlsx")
df <- df %>%  mutate(survey = "Struizendam_2")
df <- df %>%  mutate(input_data = "5_CHM_NDVI")
df_master<- bind_rows(df,df_master)
                      
#Struizendam_3
df <- read.xlsx("data_out/Bench/MLR_bench_results_Struizendam_3.xlsx")
df <- df %>%  mutate(survey = "Struizendam_3")
df <- df %>%  mutate(input_data = "5_CHM_ALLVI")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Struizendam_3_5.xlsx")
df <- df %>%  mutate(survey = "Struizendam_3")
df <- df %>%  mutate(input_data = "5")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Struizendam_3_5_CHM.xlsx")
df <- df %>%  mutate(survey = "Struizendam_3")
df <- df %>%  mutate(input_data = "5_CHM")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Struizendam_3_5_CHM_NDVI.xlsx")
df <- df %>%  mutate(survey = "Struizendam_3")
df <- df %>%  mutate(input_data = "5_CHM_NDVI")
df_master<- bind_rows(df,df_master)

#Struizendam_4

df <- read.xlsx("data_out/Bench/MLR_bench_results_Struizendam_4.xlsx")
df <- df %>%  mutate(survey = "Struizendam_4")
df <- df %>%  mutate(input_data = "5_CHM_ALLVI")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Struizendam_4_5.xlsx")
df <- df %>%  mutate(survey = "Struizendam_4")
df <- df %>%  mutate(input_data = "5")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Struizendam_4_5_CHM.xlsx")
df <- df %>%  mutate(survey = "Struizendam_4")
df <- df %>%  mutate(input_data = "5_CHM")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_Struizendam_4_5_CHM_NDVI.xlsx")
df <- df %>%  mutate(survey = "Struizendam_4")
df <- df %>%  mutate(input_data = "5_CHM_NDVI")
df_master<- bind_rows(df,df_master)

#WV2

df <- read.xlsx("data_out/Bench/MLR_bench_results_WV2.xlsx")
df <- df %>%  mutate(survey = "WV2")
df <- df %>%  mutate(input_data = "points")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_30_99_WV2e.xlsx")
df <- df %>%  mutate(survey = "WV2")
df <- df %>%  mutate(input_data = "30_99")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_400_95_WV2e.xlsx")
df <- df %>%  mutate(survey = "WV2")
df <- df %>%  mutate(input_data = "400_95")
df_master<- bind_rows(df,df_master)

df <- read.xlsx("data_out/Bench/MLR_bench_results_30_99_simple_WV2e.xlsx")
df <- df %>%  mutate(survey = "WV2")
df <- df %>%  mutate(input_data = "30_99")
df_master<- bind_rows(df,df_master)

write.xlsx(df_master, "data_out/Bench/bench_master.xlsx")
