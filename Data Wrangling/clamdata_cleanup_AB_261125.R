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

# USGS dataset is daily while EMP is monthly. Taking the monthly means of the USGS dataset before combining the two. 
usgs_cols <- names(USGS_clam_2)
usgs_cols_numeric <- usgs_cols[-1:-5]
# Loop through each column name in the list
for (col in usgs_cols_numeric) {
  # Check if the column exists in the dataframe to avoid errors
  if (col %in% names(USGS_clam_2)) {
    USGS_clam_2[[col]] <- as.numeric(USGS_clam_2[[col]])}
  }

monthly_mean_usgs <- USGS_clam_2 %>%
  mutate(Date = floor_date(Date, "month")) %>%
  group_by(Date, station_name, Latitude, Longitude, Station) %>%
  summarize(across(all_of(usgs_cols_numeric), ~mean(.x, na.rm = TRUE)), .groups = "drop")

colnames(monthly_mean_usgs)
colnames(EMP_clam_2)
monthly_mean_usgs <- monthly_mean_usgs %>%
  rename(
    Corbicula_biomass = Biomass_gAFDM_m2_CF,
    Potamocorbula_biomass = Biomass_gAFDM_m2_PA,
    Corbicula_GR = Grazing_Rate_m3_m2_d_CF,
    Potamocorbula_GR = Grazing_Rate_m3_m2_d_PA,
    Corbicula_FR = Filtration_Rate_m3_m2_d_CF,
    Potamocorbula_FR = Filtration_Rate_m3_m2_d_PA
    
  )

usgs = subset(monthly_mean_usgs, select = c(Date, Station, station_name, 
                                            Corbicula_biomass, Potamocorbula_biomass, 
                                            Corbicula_GR, Potamocorbula_GR, 
                                            Corbicula_FR, Potamocorbula_FR, 
                                            Latitude, Longitude))
#ALEX EDIT - remove species separation
usgs <- usgs %>%
  pivot_longer(cols = Corbicula_biomass:Potamocorbula_FR,
               names_to = "parameter",
               values_to = "value") %>%
  mutate(parameter = case_when(str_ends(parameter,"GR") ~ "Clam_grazing",
                               str_ends(parameter,"biomass") ~ "Clam_biomass",
                               str_ends(parameter,"FR") ~ "Clam_filtration")) %>%
  filter(!is.nan(value)) %>%
  pivot_wider(names_from = "parameter", values_from = "value", values_fn = ~mean(.x))
  

emp = subset(EMP_clam_2, select = c(Date, Station, station_name, Corbicula_biomass, Potamocorbula_biomass, Corbicula_GR, Potamocorbula_GR, Corbicula_FR, Potamocorbula_FR, Latitude, Longitude))


#ALEX EDIT - remove species separation 
emp <- emp %>%
  pivot_longer(cols = Corbicula_biomass:Potamocorbula_FR,
               names_to = "parameter",
               values_to = "value") %>%
  mutate(parameter = case_when(str_ends(parameter,"GR") ~ "Clam_grazing",
                               str_ends(parameter,"biomass") ~ "Clam_biomass",
                               str_ends(parameter,"FR") ~ "Clam_filtration")) %>%
  filter(!is.nan(value)) %>%
  pivot_wider(names_from = "parameter", values_from = "value", values_fn = ~mean(.x))

# combine the usgs and emp clam datasets
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

edi_integrate <- st_join(edi_integrate.sf, regions["Regions"], join = st_intersects, left= TRUE) %>%  
  filter(!is.na(Regions)) %>%
  st_drop_geometry() 

colnames(edi_integrate)
#Calculate monthly average values for each region
wq_r_sum <- edi_integrate %>% 
  group_by(Regions,year_month) %>%
  summarize_if(is.numeric,mean,na.rm=TRUE) %>%
  mutate(year_month = ymd(year_month)) %>%
  dplyr::select(year_month, Month, Regions, 
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
           OUT,
           Index)



combined_df.sf <- combined_df %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  #convert to UTMs so it's in the same coordinate reference system as the Delta shapefile
  st_transform(crs = 32610)

combined_df <- st_join(combined_df.sf, regions["Regions"], join = st_intersects, left= TRUE) %>%  
  filter(!is.na(Regions)) %>%
  st_drop_geometry() 

names(combined_df)[names(combined_df) == "Date"] <- "year_month"

#Calculate monthly average values for each region
combined_df_sum <- combined_df %>% 
  group_by(Regions,year_month) %>%
  summarize_if(is.numeric,mean,na.rm=TRUE) %>%
  mutate(year_month = ymd(year_month))

# Make the integrated dataset

integrated_df <- full_join(combined_df_sum, wq_r_sum, by=c("Regions", "year_month"))


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

wq_r_sum <- merge(integrated_df,dayflow,by="year_month")


#ADD SEASONAL LAG

#seasonal lag

#Step 1: summarize by season 
wq_r_sum_lagseason <- wq_r_sum %>%
  mutate(year = year(year_month)) %>%
  group_by(Regions,year,season) %>%
  summarize(across(DissAmmonia:OUT,\(x) mean(x, na.rm = TRUE)),across(SACmax_s:OUT_max_var_sm,\(x) mean(x, na.rm = TRUE))) 

#Step 2: Add a column for the following season.
#This seems counter-intuitive because the lag should be the prior season,
#but this is a merge column: it will merge back with the prior season
#in the original table. 
wq_r_sum_lagseason <- wq_r_sum_lagseason %>%
  rename_with(~paste0("lag",.x)) %>%
  rename(year = "lagyear",season = "lagseason",Regions="lagRegions") %>% 
  mutate(lagseason = case_when(
    season == "Spring" ~ "Summer",
    season == "Summer" ~ "Autumn",
    season == "Autumn" ~ "Winter",
    season == "Winter" ~ "Spring" 
  )) %>%
  mutate(lagseasonyear = case_when(
    season == "Autumn" ~ year+1,
    !(season == "Autumn") ~ year
  )) %>%
  mutate(lagseasonyear = paste0(lagseason,lagseasonyear)) %>%
  ungroup %>%
  select(lagseasonyear,Regions,lagDissAmmonia,lagSecchi,lagTotPhos,
         lagDissNitrateNitrite,lagTemperature,lagTurbidityNTU,lagConductivity,
         lagSAC,lagOUT,lagSACmax_s:lagOUT_max_var_sm)

#Step 3: Merge back with original dataset, retain all of the original dataset. 
wq_r_sum_season <- wq_r_sum %>% 
  mutate(seasonyear = paste0(season,year(year_month)))%>%
  merge(wq_r_sum_lagseason,by.x=c("Regions","seasonyear"),by.y=c("Regions","lagseasonyear"),all.x=T) 


# Write to csv
write.csv(wq_r_sum_season, "Data/regional_integrated_dataset2.csv")                           
