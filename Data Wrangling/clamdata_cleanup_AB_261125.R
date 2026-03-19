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
#replace non-detects in USGS dataset with "0"
USGS_clam <- USGS_clam %>% mutate(station_name = str_extract(Station, "\\w+(?=L|R|C)")) %>% mutate(station_name = ifelse(is.na(station_name), Station, station_name)) %>% mutate(station_name = paste0("EMP ", station_name))%>%
  mutate(Biomass_gAFDM_m2 = ifelse(Biomass_gAFDM_m2 == "N/D", 0, Biomass_gAFDM_m2))%>%mutate(Grazing_Rate_m3_m2_d = ifelse(Grazing_Rate_m3_m2_d == "N/D", 0, Grazing_Rate_m3_m2_d))%>% 
  mutate(Filtration_Rate_m3_m2_d = ifelse(Filtration_Rate_m3_m2_d == "N/D", 0, Filtration_Rate_m3_m2_d)) %>%
  mutate(across(Biomass_gAFDM_m2:Filtration_Rate_m3_m2_d, as.numeric))%>%
  select(-c(Clam_Density_no_m2, Recruits_no_0.05m2, Average_length_mm))

str(USGS_clam)
#changing USGS data to wide format
USGS_clam_2 <- USGS_clam %>% pivot_wider(names_from = Clam, values_from = c(Biomass_gAFDM_m2, Grazing_Rate_m3_m2_d, Filtration_Rate_m3_m2_d))%>%
#add month and year columns to USGS dataset
  mutate(Month= month(Date))%>%mutate(Year=year(Date))
str(USGS_clam_2)
USGS_clam_2[is.na(USGS_clam_2)]<-0

# add a date column to EMP_clam data
EMP_clam_2 <- EMP_clam %>%
  mutate(Month = match(Month, month.name))
EMP_clam_2$Date <- make_date(EMP_clam_2$Year, EMP_clam_2$Month, 1)

# not taking monthly mean before combining
#usgs_cols <- names(USGS_clam_2)
#usgs_cols_numeric <- usgs_cols[-1:-5]
# Loop through each column name in the list
#for (col in usgs_cols_numeric) {
  # Check if the column exists in the dataframe to avoid errors
#  if (col %in% names(USGS_clam_2)) {
#    USGS_clam_2[[col]] <- as.numeric(USGS_clam_2[[col]])}
#  }

#monthly_mean_usgs <- USGS_clam_2 %>%
 # mutate(Date = floor_date(Date, "month")) %>%
 # group_by(Date, station_name, Latitude, Longitude, Station) %>%
 # summarize(across(all_of(usgs_cols_numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")

colnames(USGS_clam_2)
colnames(EMP_clam_2)
USGS_clam_2 <- USGS_clam_2 %>%
  rename(
    Corbicula_biomass = Biomass_gAFDM_m2_CF,
    Potamocorbula_biomass = Biomass_gAFDM_m2_PA,
    Corbicula_GR = Grazing_Rate_m3_m2_d_CF,
    Potamocorbula_GR = Grazing_Rate_m3_m2_d_PA,
    Corbicula_FR = Filtration_Rate_m3_m2_d_CF,
    Potamocorbula_FR = Filtration_Rate_m3_m2_d_PA)

#usgs = subset(USGS, select = c(Date, Station, station_name, 
                                            #Corbicula_biomass, Potamocorbula_biomass, 
                                            #Corbicula_GR, Potamocorbula_GR, 
                                            #Corbicula_FR, Potamocorbula_FR, 
                                           # Latitude, Longitude))
#add the values from the two species from USGS dataset to get single biomass, GR, and FR value
usgs <- USGS_clam_2%>%group_by(Date, Station)%>%mutate(Biomass=sum(Corbicula_biomass, Potamocorbula_biomass))%>%
  mutate(Grazing_rate=sum(Corbicula_GR, Potamocorbula_GR))%>%mutate(Filtration_rate=sum(Corbicula_FR, Potamocorbula_FR))
  #pivot_longer(cols = Corbicula_biomass:Potamocorbula_FR,
               #names_to = "parameter",
               #values_to = "value") %>%
  #mutate(parameter = case_when(str_ends(parameter,"GR") ~ "Clam_grazing",
                             #str_ends(parameter,"biomass") ~ "Clam_biomass",
                               #str_ends(parameter,"FR") ~ "Clam_filtration")) %>%
  #pivot_wider(names_from = "parameter", values_from = "value", values_fn = ~mean(.x))
  

#emp = subset(EMP_clam_2, select = c(Date, Station, station_name, Corbicula_biomass, Potamocorbula_biomass, Corbicula_GR, Potamocorbula_GR, Corbicula_FR, Potamocorbula_FR, Latitude, Longitude))


#ALEX EDIT - remove species separation 
emp <- EMP_clam_2 %>%group_by(Date, Station)%>%
  #pivot_longer(cols = Corbicula_biomass:Potamocorbula_FR,
               #names_to = "parameter",
               #values_to = "value") %>%
  mutate(Biomass=sum(Corbicula_biomass, Potamocorbula_biomass))%>%
  mutate(Grazing_rate=sum(Corbicula_GR, Potamocorbula_GR))%>%mutate(Filtration_rate=sum(Corbicula_FR, Potamocorbula_FR))
  #mutate(parameter = case_when(str_ends(parameter,"GR") ~ "Clam_grazing",
                               #str_ends(parameter,"biomass") ~ "Clam_biomass",
                              # str_ends(parameter,"FR") ~ "Clam_filtration")) %>%
 # filter(!is.nan(value)) %>%
  #pivot_wider(names_from = "parameter", values_from = "value", values_fn = ~mean(.x))

# combine the usgs and emp clam datasets
# I think an rbind() is more appropriate here....
#combined_df <- left_join(usgs, emp)
combined_df <- rbind(usgs, emp)

# write clam data to csv
## Old - separated by species: 
# write.csv(combined_df, "Data/combined_clams_dataset.csv")
write.csv(combined_df, "Data/combined_clams_dataset2.csv")

#########################################################
# COMBINE INTEGRATED DAY FLOW DATASET WITH CLAM DATASET #
#########################################################

# To combine with the day flow dataset, we need to assign regions to combined_df and then get average monthly values by region
edi_integrate <- read_csv("Data/edi_df_integrate_monthly.csv")

edi_integrate <- edi_integrate %>% select(-c("Year","Month")) %>%
                 mutate(year_month = as.Date(year_month)) %>%
                 mutate(Month = month(year_month))%>%
                 mutate(Year = year(year_month))
                          
#read in region data
regions <- st_read("Data/Regions_shp/Rosies_regions_edited.shp")

#change the crs to UTM zone 10N (EPSG 32610)
regions<- st_transform(regions, crs =32610)

#Attach Regions and Clean up Data

edi_integrate.sf <- edi_integrate %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  #convert to UTMs so it's in the same coordinate reference system as the Delta shapefile
st_transform(crs = 32610)

edi_integrate <- st_join(edi_integrate.sf, regions["Regions"], join = st_intersects, left= TRUE) %>%  
  filter(!is.na(Regions)) %>%
  st_drop_geometry() 

#colnames(edi_integrate)
#Calculate monthly average values for each region
# wq_r_sum <- edi_integrate %>% 
#   group_by(Regions,year_month) %>%
#   summarize_if(is.numeric,mean,na.rm=TRUE) %>%
#   mutate(year_month = ymd(year_month)) %>%
#   dplyr::select(year_month, Month, Regions, 
#            Chlorophyll,
#            DissAmmonia,
#            TotAmmonia,
#            Secchi,
#            TotPhos,
#            DissNitrateNitrite,
#            Temperature, 
#            TurbidityNTU,
#            Conductivity,
#            SAC,
#            OUT,
#            Index)



combined_df.sf <- combined_df %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  #convert to UTMs so it's in the same coordinate reference system as the Delta shapefile
  st_transform(crs = 32610)

combined_df <- st_join(combined_df.sf, regions["Regions"], join = st_intersects, left= TRUE) %>%  
  filter(!is.na(Regions)) %>%
  st_drop_geometry() 

names(combined_df)[names(combined_df) == "Date"] <- "year_month"

#Calculate monthly average values for each region_ edit: Laura need to redo this
# combined_df_sum <- combined_df %>% 
#   group_by(Regions,year_month) %>%
#   summarize_if(is.numeric,mean,na.rm=TRUE) %>%
#   mutate(year_month = ymd(year_month))


# Make the integrated dataset

integrated_df <- full_join(combined_df, edi_integrate, by=c("Station", "Month", "Year", "year_month", "Regions"))


integrated_df <- integrated_df %>%
  group_by(Month, Year, Regions) %>%
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)))


# Assign seasons using ifelse statements

## Alex Edit: shifted seasons up one month because it's easier to handle 
## (e.g. Winter = Jan-Mar, Spring = Apr - Jun, etc.)
integrated_df$season <- ifelse(integrated_df $Month %in% c(1,2,3), "Winter",
                               ifelse(integrated_df $Month %in% c(4,5,6), "Spring",
                                      ifelse(integrated_df$Month %in% c(7,8,9), "Summer",
                                             "Autumn")))
# Remove Suisun Marsh

integrated_df <- integrated_df[integrated_df$Regions != "Suisun Marsh", ]

#READ IN MAX DAYFLOW/VARIANCES ----------------
dayflow <- read_csv("Data/flow_variables.csv") %>%
  select(-Year,-Month,-Season)

dayflow$year_month <- as.Date(dayflow$year_month)

dayflow <- dayflow %>%
  mutate(Month = month(year_month)) %>%
  mutate(Year = year(year_month)) %>%
  subset(select = -year_month)

wq <- left_join(integrated_df,dayflow,by=c("Year", "Month"))  %>%
  mutate(Date = make_date(year = Year, month = Month, day = 1))  %>%
  relocate(Date)%>%
  subset(select = -c(Corbicula_density, Potamocorbula_density,Corbicula_recruits, 
                     Potamocorbula_recruits, Corbicula_length, Potamocorbula_length,
                     Potamocorbula_biomass, Corbicula_biomass,
                     Potamocorbula_GR, Corbicula_GR,
                     Potamocorbula_FR, Corbicula_FR))

#ADD SEASONAL LAG

#seasonal lag

#Step 1: summarize by season 
wq_r_sum_lagseason <- wq %>%
  group_by(Regions,Year,season) %>%
  summarize(across(where(is.numeric),\(x) mean(x, na.rm = TRUE)))
  #summarize(across(DissAmmonia:OUT,\(x) mean(x, na.rm = TRUE)),across(SACmax_s:OUT_max_var_sm,\(x) mean(x, na.rm = TRUE))) 

#Step 2: Add a column for the following season.
#This seems counter-intuitive because the lag should be the prior season,
#but this is a merge column: it will merge back with the prior season
#in the original table. 
wq_r_sum_lagseason <- wq_r_sum_lagseason %>%
  rename_with(~paste0("lag",.x)) %>%
  rename(Year = "lagYear",season = "lagseason",Regions="lagRegions") %>% 
  mutate(lagseason = case_when(
    season == "Spring" ~ "Summer",
    season == "Summer" ~ "Autumn",
    season == "Autumn" ~ "Winter",
    season == "Winter" ~ "Spring" 
  )) %>%
  mutate(lagseasonyear = case_when(
    season == "Autumn" ~ Year+1,
    !(season == "Autumn") ~ Year
  )) %>%
  mutate(lagseasonyear = paste0(lagseason,lagseasonyear)) %>%
  ungroup %>%
  select(lagseasonyear,Regions,lagDissAmmonia,lagSecchi,lagTotPhos,
         lagDissNitrateNitrite,lagTemperature,lagTurbidityNTU,lagConductivity,
         lagSAC,lagOUT,lagIndex, lagSACmax_s:lagOUT_max_var_sm)

#Step 3: Merge back with original dataset, retain all of the original dataset. 
wq_r_sum_season <- wq %>% 
  mutate(seasonyear = paste0(season,year(Date)))%>%
  merge(wq_r_sum_lagseason,by.x=c("Regions","seasonyear"),by.y=c("Regions","lagseasonyear"),all.x=T) %>% 
  select(-seasonyear)%>%
  relocate(Date)

l <- names(wq_r_sum_season)[-c(1:4, 56)]

wq_r_sum_season_long <- wq_r_sum_season %>% pivot_longer(l, names_to = "variable", values_to= "value")

# Write to csv
#write.csv(wq_r_sum_season, "Data/regional_integrated_dataset2.csv")                           
#New
#write.csv(wq_r_sum_season_long, "Data/regional_integrated_dataset_long.csv")   
write.csv(wq_r_sum_season, "Data/regional_integrated_dataset3.csv")
