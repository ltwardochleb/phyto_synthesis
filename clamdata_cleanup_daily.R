#Data wrangling and integrating clam  data
#9/9/2025

#Read in packages
library(readxl)
library(tidyverse)
library(lubridate)
library(tidyverse)
library(mgcv)
library(sf)

#############################
# COMBINE THE CLAM DATASETS #
#############################

#read in EMP clam data and USGS clam data
EMP_clam <- read.csv("Data/EMP biomass, GR, FR 2020-2023.csv")
USGS_clam <- read_excel("Data/Monthly Bivalve Metrics All - from USGS through 2019.xlsx")

#rename station names
EMP_clam <- EMP_clam %>% mutate(station_name = str_extract(Station, "\\w+(?=[-]?)")) %>% mutate(station_name = paste0("EMP ", station_name))
EMP_clam$Station <-gsub("-", "", EMP_clam$Station)
USGS_clam <- USGS_clam %>% mutate(station_name = str_extract(Station, "\\w+(?=L|R|C)")) %>% mutate(station_name = ifelse(is.na(station_name), Station, station_name)) %>% mutate(station_name = paste0("EMP ", station_name))                   

#changing USGS data to long format
USGS_clam_2 <- USGS_clam %>% pivot_wider(names_from = Clam, values_from = c(Clam_Density_no_m2, Biomass_gAFDM_m2, Grazing_Rate_m3_m2_d, Filtration_Rate_m3_m2_d, Average_length_mm, Recruits_no_0.05m2))

# add a date column to EMP_clam data
EMP_clam_2 <- EMP_clam %>%
  mutate(Month = match(Month, month.name))

EMP_clam_2$Date <- make_date(EMP_clam_2$Year, EMP_clam_2$Month, 1)

usgs_clam_3 <- USGS_clam_2 %>%
  rename(
    Corbicula_biomass = Biomass_gAFDM_m2_CF,
    Potamocorbula_biomass = Biomass_gAFDM_m2_PA,
    Corbicula_GR = Grazing_Rate_m3_m2_d_CF,
    Potamocorbula_GR = Grazing_Rate_m3_m2_d_PA,
    Corbicula_FR = Filtration_Rate_m3_m2_d_CF,
    Potamocorbula_FR = Filtration_Rate_m3_m2_d_PA
    
  )

usgs = subset(usgs_clam_3, select = c(Date, Station, station_name, Corbicula_biomass, Potamocorbula_biomass, Corbicula_GR, Potamocorbula_GR, Corbicula_FR, Potamocorbula_FR, Latitude, Longitude))
emp = subset(EMP_clam_2, select = c(Date, Station, station_name, Corbicula_biomass, Potamocorbula_biomass, Corbicula_GR, Potamocorbula_GR, Corbicula_FR, Potamocorbula_FR, Latitude, Longitude))

# combine the usgs and emp clam datasets
clam_df <- rbind(usgs, emp)


# write clam data to csv
write.csv(clam_df, "Data/combined_clams_dataset_daily.csv")

#########################################################
# COMBINE INTEGRATED DAY FLOW DATASET WITH CLAM DATASET #
#########################################################

# To combine with the day flow dataset, we need to assign regions to clam_df and then get average monthly values by region
edi_integrate <- read_csv("Data/edi_df_integrate_daily.csv")
#read in region data
regions <- st_read("Regions_shp/Rosies_regions_edited.shp")

#change the crs to UTM zone 10N (EPSG 32610)
regions<- st_transform(regions, crs =32610)

#Attach Regions and Clean up Data

edi_integrate.sf <- edi_integrate %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  #convert to UTMs so it's in the same coordinate reference system as the Delta shapefile
  st_transform(crs = 32610)

edi_integrate <- st_join(edi_integrate.sf, regions["Regions"], join = st_intersects, left= FALSE) %>%  
  filter(!is.na(Regions)) %>%
  st_drop_geometry() 

edi_integrate_clean <- edi_integrate %>% 
  dplyr::select(Date, Station,Regions, 
                Chlorophyll,
                DissAmmonia,
                TotAmmonia,
                Secchi,
                TotPhos,
                DissNitrateNitrite,
                Temperature, 
                TurbidityNTU,
                Conductivity,
                SAC,
                Index)


clam_df.sf <- clam_df %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  #convert to UTMs so it's in the same coordinate reference system as the Delta shapefile
  st_transform(crs = 32610)

clam_df <- st_join(clam_df.sf, regions["Regions"], join = st_intersects, left= FALSE) %>%  
  filter(!is.na(Regions)) %>%
  st_drop_geometry() 

clam_df$Date <- as.Date(clam_df$Date, format="%Y/%m/%d")
edi_integrate_clean$Date <- as.Date(edi_integrate_clean$Date, format= "%m/%d/%Y")
# Make the integrated dataset

integrated_df <- full_join(clam_df, edi_integrate_clean, by=c("Date", "Station"), relationship="many-to-many")

integrated_df <- integrated_df %>%
  mutate(Regions = coalesce(Regions.x, Regions.y)) %>%
  select(-Regions.x, -Regions.y)
# Remove Suisun Marsh

integrated_df <- integrated_df[integrated_df$Regions != "Suisun Marsh", ]

integrated_df <- integrated_df %>%
  mutate(Month = month(Date))
# Write to csv
write.csv(integrated_df, "Data/integrated_edi_dayflow_clams_daily.csv")  
