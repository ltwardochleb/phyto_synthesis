#Dayflow and Discrete WQ Data integration - edi
library(tidyverse)
library(lubridate)

#Dayflow -------------
#QUESTION: does it make more sense to do the month average or match with the discrete WQ date and summarize there?
df1 <- read_csv("Data/Dayflow/dayflow-results-1970-1983.csv") %>%
  select(c("Date","Year","Month","SAC","SJR","OUT")) #Sac, SJR, Delta Outflow

df2 <- read_csv("Data/Dayflow/dayflow-results-1984-1996.csv") %>%
  select(c("Date","Year","Month","SAC","SJR","OUT")) #Sac, SJR, Delta Outflow

df3 <- read_csv("Data/Dayflow/dayflow-results-1997-2023.csv") %>%
  select(c("Date","Year","Month","SAC","SJR","OUT")) #Sac, SJR, Delta Outflow

dayflow <- rbind(df1,df2,df3)

rm(df1)
rm(df2)
rm(df3)

#Aggregate by month
df_month <- dayflow %>% 
  group_by(Year,Month) %>% 
  summarise_if(is.numeric,mean) %>% 
  mutate(year_month = parse_date(paste(Year,Month),"%Y %m"))

rm(dayflow)

#EDI ----------

source("edi.731.7.r") #clean edi file

rm("dt2") #remove extra df

#add a column for month
colnames(dt1)

#create year_month column and aggregate to monthly data.
#QUESTION: what do I do with < values? filter them out? Convert to 0? 
dt1_mo <- dt1 %>% 
  mutate(Date = as.Date(as.character(Date))) %>%
  mutate(year_month = parse_date(paste(year(Date),month(Date)),"%Y %m")) %>%
  group_by(Source,Station,Latitude,Longitude,year_month) %>%
  summarise_if(is.numeric,mean)

dt_df <- merge(dt1_mo,df_month,by="year_month",all.x=T)

#QUESTION: how far back are we going? 