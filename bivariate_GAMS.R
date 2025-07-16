#Alex attempt at setting up bivariate GAMs for determining threshold parameters

library(tidyverse)
library(mgcv)
library(MuMIn)
library(gratia)
library(sf)

wq <- read.csv("Data/edi_df_integrate_monthly.csv") %>%
  filter(ymd(year_month) >= ymd(19790101))

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
  merge(wq_season,by="Month",all.x=T) %>%
  select(c("year_month", "Month", "Regions", 
           "TotAmmonia", "DissAmmonia",
           "TotPhos","DissNitrateNitrite",
           "Temperature", 
           "TurbidityNTU","TurbidityFNU",
           "Conductivity",
           "SAC",
           "season",
           "Chlorophyll"))
  

#Filter dataset to variables we're interested in - predictors and chlorophyll
#produce figure to see data gaps by region (see data exploration)
#Focus on NAs over time
#look at study approach document for global model

#4. Assign chlorophyll thresholds? --------
#Used 5 as a placeholder 
wq_r_sum$Bloom <- replicate(nrow(wq_r_sum),NA)
wq_r_sum$Bloom[wq_r_sum$Chlorophyll >= 5] <- 1
wq_r_sum$Bloom[wq_r_sum$Chlorophyll < 5] <- 0

#use chlorophyll for now

#Data exploration - WQ sum



#5. set up the gam -----------
# NH4 + PO4 + (NO2+NO3) + temperature + turbidity + conductivity + Sac inflow + Sac Valley index + clam biomass + Season + lag(Sac inflow) + previous year(Sac Valley index) + lag(NH4) + lag(PO4) + lag (NO2+NO3) + (1|station) + (1|Month)

colnames(wq_r_sum)

###### TO DO ######
#NEED TO PULL IN SAC VALLEY INDEX TO OG DATA INTEGRATION
#Laura to pull in clam biomass

#questions:
#1. How do I implement lag?
#2. How do I implement more than two categories? (season)
#3. what is the right k value? 


#regional summary by month is the input

m <- gam(Bloom ~ Temperature+SAC,
    data = wq_r_sum,
    family = binomial,
    method = 'ML') #look into the methods - MLE

#want some of the predictors to be linear - look at Shaela's figures
## No linear equivalent for s, just naked in the function
## bs = "cc" categorizes 
#want some of the interaction terms
#don't need to specify k - can do check after

help(te)
help(s) #s basically fits a polynomial relationship

k.check(m)

help(gam)
summary(m)



