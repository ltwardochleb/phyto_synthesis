library(tidyverse)
library(sf)
library(mapview)
library(lubridate)

#read in dayflow .csv

dayflow <- read_csv("./Data/Dayflow/dayflow-results-1997-2023.csv")
df_month <- dayflow %>% 
  group_by(Year,Month) %>% 
  summarise_if(is.numeric,mean) %>%
  select(c("Year","Month","SAC","SJR"))

df_month$Date <- paste(df_month$Year,df_month$Month,sep="-")
df_month$Date <- ym(df_month$Date)
help(format)

ggplot() +
  geom_line(data=df_month,
            aes(x=Date,y=SAC),
            color="red") +
  geom_line(data=df_month,
            aes(Date,SJR))
