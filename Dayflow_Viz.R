library(tidyverse)
library(sf)
library(mapview)
library(lubridate)

#read in dayflow .csv

dayflow <- read_csv("./Data/Dayflow/dayflow-results-1997-2023.csv")

#Aggregate by month
df_month <- dayflow %>% 
  group_by(Year,Month) %>% 
  summarise_if(is.numeric,mean) %>%
  select(c("Year","Month","SAC","SJR")) #just looking at SAC and SJR for now

#Put month and year in same column and 
#(could probably do this faster as a mutate)
df_month$Date <- paste(df_month$Year,df_month$Month,sep="-")
df_month$Date <- ym(df_month$Date)

#very rough plot of Sac and SJR
ggplot() +
  geom_line(data=df_month,
            aes(x=Date,y=SAC),
            color="red") +
  geom_line(data=df_month,
            aes(Date,SJR))

#more ideas - split out into critically dry, dry, wet, and average. There's likely some guidance on this somewhere. 
#Aggregate by season?
#How did Ellen define a wet/dry winter/spring? 

