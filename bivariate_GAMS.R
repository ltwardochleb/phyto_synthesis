#Alex attempt at setting up bivariate GAMs for determining threshold parameters

library(tidyverse)
library(mgcv)
library(MuMIn)
library(gratia)
library(sf)

#Setting up GAMs for threshold

wq <- read.csv("Data/edi_df_integrate_monthly.csv")

#1. Assign region to each station
# do we already have a regional assignment somewhere?

regions<-st_read("Regions_shp/Rosies_regions_edited.shp") %>%
  filter(!is.na(Regions)) %>%
  st_transform(26910)

wq.sf <- wq %>%
  filter(!is.na(Longitude) & !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  #convert to UTMs so it's in the same coordinate reference system as the Delta shapefile
  st_transform(crs = 26910)

wq <- st_join(wq.sf, regions["Regions"], join = st_intersects, left= TRUE) %>%  
  filter(!is.na(Regions)) %>%
  st_drop_geometry()

#2. Calculate monthly average values for each region

wq_r_sum <- wq %>% 
  group_by(Regions,year_month) %>%
  summarize_if(is.numeric,mean,na.rm=TRUE)

#3. Assign a season to each month 
wq$season <- replicate(nrow(wq),NA)
wq$season[wq$Month %in% c(12,1,2)] <- "Winter"
wq$season[wq$Month %in% c(3,4,5)] <- "Spring"
wq$season[wq$Month %in% c(6,7,8)] <- "Summer"
wq$season[wq$Month %in% c(9,10,11)] <- "Fall"


