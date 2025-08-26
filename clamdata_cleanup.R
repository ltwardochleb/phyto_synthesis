#Data wrangling and integrating clam  data
#7/28/2025

#Read in packages
library(readxl)
library(tidyverse)

#read in EMP clam data and USGS clam data
EMP_clam <- read.csv("Data/EMP biomass, GR, FR 2020-2023.csv")
USGS_clam <- read_excel("Data/Monthly Bivalve Metrics All - from USGS through 2019.xlsx")

#rename station names
EMP_clam <- EMP_clam %>% mutate(station_name = str_extract(Station, "\\w+(?=[-]?)")) %>% mutate(station_name = paste0("EMP ", station_name))
USGS_clam <- USGS_clam %>% mutate(station_name = str_extract(Station, "\\w+(?=L|R|C)")) %>% mutate(station_name = ifelse(is.na(station_name), Station, station_name)) %>% mutate(station_name = paste0("EMP ", station_name))                   

#changing USGS data to long format
USGS_clam_2 <- USGS_clam %>% pivot_wider(names_from = Clam, values_from = c(Clam_Density_no_m2, Biomass_gAFDM_m2, Grazing_Rate_m3_m2_d, Filtration_Rate_m3_m2_d, Average_length_mm, Recruits_no_0.05m2))
