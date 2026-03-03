library(tidyverse)
library(sf)
library(mapview)
library(lubridate)

#read in dayflow .csv

dayflow <- read_csv("./Data/Dayflow/dayflow-results-1997-2023.csv")
wy_type <- read_csv("./Data/Dayflow/wy_class.csv")

#Aggregate by month
df_month <- dayflow %>% 
  group_by(Year,Month) %>% 
  summarise_if(is.numeric,mean) %>%
  select(c("Year","Month","SAC","SJR")) #just looking at SAC and SJR for now

#Put month and year in same column and 
#(could probably do this faster as a mutate)
df_month$Date <- paste(df_month$Year,df_month$Month,sep="-")
df_month$Date <- ym(df_month$Date)

#add water year column by adding 3 months
df_month$water_year <- as.numeric(format(df_month$Date %m+% months(3),"%Y"))

#create seasonal period column
df_month$period <- character(length(nrow(df_month)))
df_month$period[df_month$Month >= 12 | df_month$Month <= 2] <- "Winter"
df_month$period[df_month$Month >= 3 & df_month$Month <= 5] <- "Spring"
df_month$period[df_month$Month >= 6 & df_month$Month <= 8] <- "Summer"
df_month$period[df_month$Month >= 9 & df_month$Month <= 11] <- "Fall" #can change or add periods based on study time frame

#pivot wider then summarize by year/season
df_wide<- df_month %>%
  filter(!period == "Fall"& !period == "Winter") %>%
  pivot_wider(names_from=period,values_from=c(SAC,SJR)) %>%
  group_by(water_year) %>%
  summarize(across(`SAC_Spring`:`SJR_Summer`,\(x) mean(x,na.rm=TRUE)))%>%
  mutate(across(`SAC_Spring`:`SJR_Summer`,list(perc=percent_rank)))

df_wide$SAC_type <- character(length(nrow(df_month)))
df_wide$SAC_type[df_wide$SAC_Spring_perc >= 0.5&df_wide$SAC_Summer_perc >= 0.5] <- "wet-to-wet"
df_wide$SAC_type[df_wide$SAC_Spring_perc < 0.5&df_wide$SAC_Summer_perc >= 0.5] <- "dry-to-wet"
df_wide$SAC_type[df_wide$SAC_Spring_perc >= 0.5&df_wide$SAC_Summer_perc < 0.5] <- "wet-to-dry"
df_wide$SAC_type[df_wide$SAC_Spring_perc < 0.5&df_wide$SAC_Summer_perc < 0.5] <- "dry-to-dry"

df_wide$SJR_type <- character(length(nrow(df_month)))
df_wide$SJR_type[df_wide$SJR_Spring_perc >= 0.5&df_wide$SJR_Summer_perc >= 0.5] <- "wet-to-wet"
df_wide$SJR_type[df_wide$SJR_Spring_perc < 0.5&df_wide$SJR_Summer_perc >= 0.5] <- "dry-to-wet"
df_wide$SJR_type[df_wide$SJR_Spring_perc >= 0.5&df_wide$SJR_Summer_perc < 0.5] <- "wet-to-dry"
df_wide$SJR_type[df_wide$SJR_Spring_perc < 0.5&df_wide$SJR_Summer_perc < 0.5] <- "dry-to-dry"

#merge with Water year type based on runoff from DWR
df_wide <- df_wide %>% merge(wy_type[c("WY","SAC Yr-type","SJR Yr-type")],by.x="water_year",by.y="WY")
  
#very rough plot of Sac and SJR
ggplot() +
  geom_line(data=df_month,
            aes(x=Date,y=SAC),
            color="red") +
  geom_line(data=df_month,
            aes(Date,SJR))

#bar chart by WY for avg sac flow in spring, colored by WY type
ggplot(df_wide,aes(water_year,`SAC_Spring`)) +
  geom_col(aes(fill=`SAC Yr-type`))


#How did Ellen define a wet/dry spring/summer? 

