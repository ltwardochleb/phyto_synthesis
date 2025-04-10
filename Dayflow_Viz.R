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
df_month$period[df_month$Month >= 10 | df_month$Month <= 3] <- "Oct-Mar"
df_month$period[df_month$Month >= 4 & df_month$Month <= 9] <- "Apr-Sep" #can change or add periods based on study time frame

#pivot wider then summarize by year/season
df_wide<- df_month %>% 
  pivot_wider(names_from=period,values_from=c(SAC,SJR)) %>%
  group_by(water_year) %>%
  summarize(across(`SAC_Oct-Mar`:`SJR_Apr-Sep`,\(x) sum(x,na.rm=TRUE)))

#merge with Water year type based on runoff from DWR
df_wide <- df_wide %>% merge(wy_type,by.x="water_year",by.y="WY")
  
#very rough plot of Sac and SJR
ggplot() +
  geom_line(data=df_month,
            aes(x=Date,y=SAC),
            color="red") +
  geom_line(data=df_month,
            aes(Date,SJR))

#bar chart by WY for Sac during non-irrigation season
ggplot(df_wide,aes(water_year,`SAC_Oct-Mar`)) +
  geom_col(aes(fill=`SAC Yr-type`))


#more ideas - split out into critically dry, dry, wet, and average. There's likely some guidance on this somewhere. 
#Aggregate by season?
#How did Ellen define a wet/dry spring/summer? 

