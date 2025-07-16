#Alex attempt at setting up bivariate GAMs for determining threshold parameters

library(tidyverse)
library(mgcv)
library(MuMIn)
library(gratia)
library(sf)

wq <- read.csv("Data/edi_df_integrate_monthly.csv")

#1. Assign region to each station --------
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

#2. Assign a season to each month --------
wq$season <- replicate(nrow(wq),NA)
wq$season[wq$Month %in% c(12,1,2)] <- "Winter"
wq$season[wq$Month %in% c(3,4,5)] <- "Spring"
wq$season[wq$Month %in% c(6,7,8)] <- "Summer"
wq$season[wq$Month %in% c(9,10,11)] <- "Fall"

wq_season <- wq %>% group_by(Month) %>%
  summarize(season=unique(season))

#3. Calculate monthly average values for each region --------

wq_r_sum <- wq %>% 
  group_by(Regions,year_month) %>%
  summarize_if(is.numeric,mean,na.rm=TRUE) %>%
  merge(wq_season,by="Month",all.x=T)

#4. Assign chlorophyll thresholds? --------
#idk if we ever decided on this but I can change it if I need to, will stick with 5 for now
wq$bloom <- replicate(nrow(wq),NA)
wq$bloom[wq$Chlorophyll >= 5] <- as.factor(1)
wq$bloom[wq$Chlorophyll < 5] <- as.factor(0)

#5. set up the gam -----------
# NH4 + PO4 + (NO2+NO3) + temperature + turbidity + conductivity + Sac inflow + Sac Valley index + clam biomass + Season + lag(Sac inflow) + previous year(Sac Valley index) + lag(NH4) + lag(PO4) + lag (NO2+NO3) + (1|station) + (1|Month)

colnames(wq)

###### TO DO ######
#NEED TO PULL IN SAC VALLEY INDEX TO OG DATA INTEGRATION
#Laura to pull in clam biomass

#questions:
#1. How do I implement lag?
#2. How do I implement more than two categories? (season)
#3. what is the right k value? 



m <- gam(bloom ~ s(TotAmmonia,TotPhos,DissNitrateNitrite,Temperature,TurbidityNTU,Conductivity,SAC,k=500),
    data = wq,
    family = binomial,
    method = 'REML')

summary(m)
