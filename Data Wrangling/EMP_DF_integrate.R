#Dayflow and Discrete WQ Data integration
library(tidyverse)
library(lubridate)

df1 <- read_csv("Data/Dayflow/dayflow-results-1970-1983.csv") %>%
  select(c("Date","Year","Month","SAC","SJR","OUT")) #Sac, SJR, Delta Outflow

df2 <- read_csv("Data/Dayflow/dayflow-results-1984-1996.csv") %>%
  select(c("Date","Year","Month","SAC","SJR","OUT")) #Sac, SJR, Delta Outflow

df3 <- read_csv("Data/Dayflow/dayflow-results-1997-2023.csv") %>%
  select(c("Date","Year","Month","SAC","SJR","OUT")) #Sac, SJR, Delta Outflow

dayflow <- rbind(df1,df2,df3)

# DAYFLOW AGGREGATION --------------

#Aggregate by month
df_month <- dayflow %>% 
  group_by(Year,Month) %>% 
  summarise_if(is.numeric,mean) %>% 
  mutate(year_month = parse_date(paste(Year,Month),"%Y %m"))

#FROM KYLE'S EMP CLEAN SCRIPT ------------

### Load EMP data
EMP_df<-read.csv("EMP_DWQ_1975_2023.csv", header=TRUE)


#### Clean data ####
EMP_df <- EMP_df %>%
  mutate(DissAmmonia = ifelse(DissAmmonia_Sign== "<" &DissAmmonia>0.05,NA, DissAmmonia))%>%
  mutate(DissAmmonia = ifelse(DissAmmonia<0.05, 0.05, DissAmmonia)) %>%
  mutate(DissOrthophos = ifelse(DissOrthophos_Sign== "<" &DissOrthophos>0.3,NA, DissOrthophos))%>%
  mutate(DissOrthophos = ifelse(DissOrthophos<0.05, 0.05, DissOrthophos))%>%
  mutate(DissNitrateNitrite = ifelse(DissNitrateNitrite_Sign== "<" &DissNitrateNitrite>1,NA, DissNitrateNitrite))%>%
  mutate(DissNitrateNitrite = ifelse(DissNitrateNitrite<0.05, 0.05, DissNitrateNitrite))

#Reformat
colnames(EMP_df)

EMP_df <- EMP_df %>%
  subset(select = c(Station, Date, Time, Chla, DissAmmonia,DissNitrateNitrite,DissOrthophos,WaterTempSurface, TurbiditySurface,
                    TSS,WaterDepth ,DOSurface,SpCndSurface, pHSurface))%>%
  mutate(DIN_mg_l_mean= ifelse(is.na(DissAmmonia)|is.na(DissNitrateNitrite), NA, DissNitrateNitrite+DissAmmonia)) %>%
  rename(chla_ug_l= Chla,
         no23_mg_l=DissNitrateNitrite,
         nh4_mg_l= DissAmmonia,
         po4_mg_l=DissOrthophos,
         Temp_C =WaterTempSurface,
         Turbidity_FNU =TurbiditySurface ,
         TSS_mg_l= TSS,
         WaterDepth_ft =WaterDepth,
         DO_mg_l = DOSurface,
         SpCnd_uS_cm = SpCndSurface,
         pH = pHSurface,
         Site_Abbrev = Station) %>%
  mutate(Year_Month = as.yearmon(Date, "%Y-%m-%d"))%>%
  mutate(month = substr(Year_Month,1,3)) %>%
  mutate(Winter = ifelse(month=="Dec"|month=="Jan"|month=="Feb", 1, 0)) %>%
  mutate(spring = ifelse(month=="Mar"|month=="Apr"|month=="May", 1, 0)) %>%
  mutate(summer = ifelse(month=="Jun"|month=="Jul"|month=="Aug", 1, 0)) %>%
  mutate(fall = ifelse(month=="Sep"|month=="Oct"|month=="Nov", 1, 0)) %>%
  group_by(Site_Abbrev, Year_Month) %>%
  mutate(n_rows = n()) %>% # Prior to 1998, especially 1992, sites can have 2 measurements per month
  unique() %>%
  mutate(year_month = as.Date(Year_Month))%>%
  mutate(Month_day = format(as.Date(year_month), "%m-%d"))%>%
  mutate(Season= ifelse(Month_day <format(as.Date("03-01",format="%m-%d"), "%m-%d")|Month_day >=format(as.Date("12-01","%m-%d"), "%m-%d"), "Winter", NA)) %>%
  mutate(Season= ifelse(Month_day <format(as.Date("06-01","%m-%d"), "%m-%d")&Month_day >=format(as.Date("03-01","%m-%d"), "%m-%d"), "Spring", Season))%>%
  mutate(Season= ifelse(Month_day <format(as.Date("09-01","%m-%d"), "%m-%d")&Month_day >=format(as.Date("06-01","%m-%d"), "%m-%d"), "Summer", Season))%>%
  mutate(Season= ifelse(Month_day <format(as.Date("12-01","%m-%d"), "%m-%d")&Month_day >=format(as.Date("09-01","%m-%d"), "%m-%d"), "Fall", Season)) %>%
  # subset(year_month >= as.Date("2014-01-01",format = "%Y-%m-%d")) %>%
  mutate(Year_Month=as.character(Year_Month),
         year_month= as.character(year_month))%>%
  subset(select = -c(Month_day))

#Remove sites we do not consider to be in the Delta
#Combine sites that had slight location change
unique(EMP_df$Site_Abbrev)
EMP_df <- EMP_df %>%
  subset(Site_Abbrev != "D41"&Site_Abbrev != "D41A"&Site_Abbrev != "D42"&Site_Abbrev != "D6"&Site_Abbrev != "NZ002"&Site_Abbrev != "NZ004"&
           Site_Abbrev != "NZ325") %>%
  mutate(Site_Abbrev = ifelse(Site_Abbrev =="P12", "P12A", Site_Abbrev))%>%
  mutate(Site_Abbrev = ifelse(Site_Abbrev =="P10", "P10A", Site_Abbrev))%>%
  mutate(Site_Abbrev = ifelse(Site_Abbrev =="MD7", "MD7A", Site_Abbrev))%>%
  mutate(Site_Abbrev = ifelse(Site_Abbrev =="MD10", "MD10A", Site_Abbrev))%>%
  mutate(Site_Abbrev = ifelse(Site_Abbrev =="C3", "C3A", Site_Abbrev))%>%
  mutate(Site_Abbrev = ifelse(Site_Abbrev =="C10", "C10A", Site_Abbrev))%>%
  mutate(Site_Abbrev = ifelse(Site_Abbrev =="S42", "NZS42", Site_Abbrev))

#MERGE EMP DWQ WITH DAYFLOW

EMP_comb <- merge(EMP_df,df_month,by="year_month",all.x=TRUE)
