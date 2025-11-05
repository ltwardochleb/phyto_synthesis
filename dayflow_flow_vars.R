#Integrate monthly and seasonal max dayflow

library(tidyverse)

df1 <- read_csv("Data/Dayflow/dayflow-results-1970-1983.csv") %>%
  select(Year,Month,Date,SAC,OUT)
df2 <- read_csv("Data/Dayflow/dayflow-results-1984-1996.csv") %>%
  select(Year,Month,Date,SAC,OUT)
df3 <- read_csv("Data/Dayflow/dayflow-results-1997-2023.csv") %>%
  select(Year,Month,Date,SAC,OUT)

df_tot <- rbind(df1,df2,df3) %>%
  mutate(Season = case_when(Month >= 1 & Month <= 3 ~ "Winter",
                            Month >= 4 & Month <= 6 ~ "Spring",
                            Month >= 7 & Month <= 9 ~ "Summer",
                            Month >= 10 & Month <= 12 ~ "Autumn")) 

for(i in 1:(length(df_tot$SAC))) {
  if(i<3) {df_tot$SAC_flow_var[i] <- NA  #First values can't be calculated because of the 7-day window construction
  } else if (i < length(df_tot$SAC - 4)) {df_tot$SAC_flow_var[i] <- max(df_tot$SAC[(i-2):(i+4)])-min(df_tot$SAC[(i-2):(i+4)])
  } else {df_tot$SAC_flow_var[i] <- NA} #Final values can't be calculated because of the 7-day window construction
}


for(i in 1:(length(df_tot$OUT))) {
  if(i<3) {df_tot$OUT_flow_var[i] <- NA  #First values can't be calculated because of the 7-day window construction
  } else if (i < length(df_tot$OUT - 4)) {df_tot$OUT_flow_var[i] <- max(df_tot$OUT[(i-2):(i+4)])-min(df_tot$OUT[(i-2):(i+4)])
  } else {df_tot$OUT_flow_var[i] <- NA} #Final values can't be calculated because of the 7-day window construction
}


df_monthmax <- df_tot %>%
  group_by(Year,Month) %>%
  summarize(SACmax_mo = max(SAC),OUTmax_mo = max(OUT),SACmean_mo = mean(SAC),OUTmean_mo = mean(OUT)) %>%
  mutate(Season = case_when(Month >= 1 & Month <= 3 ~ "Winter",
                            Month >= 4 & Month <= 6 ~ "Spring",
                            Month >= 7 & Month <= 9 ~ "Summer",
                            Month >= 10 & Month <= 12 ~ "Autumn"))

df_seasmax <- df_tot %>%
  group_by(Year,Season) %>%
  summarize(SACmax_s = max(SAC),OUTmax_s = max(OUT))

df_seasmo_max <- df_monthmax %>% 
  group_by(Year,Season) %>%
  summarize(SACmax_sm = max(SACmean_mo),OUTmax_sm = max(OUTmean_mo))
  
df_max <- df_monthmax %>%
  merge(df_seasmax,by=c("Year","Season"),all.x=T) %>%
  merge(df_seasmo_max,by=c("Year","Season"),all.x=T) %>%
  mutate(year_month = parse_date(paste(Year,Month),"%Y %m")) %>%
  filter(year_month > ymd("1978-9-1")) %>%
  select(-SACmean_mo,-OUTmean_mo)

write
