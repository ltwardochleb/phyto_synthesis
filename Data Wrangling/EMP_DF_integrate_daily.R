#Integrating Dayflow dataset without binning
#Dayflow and Discrete WQ Data integration - edi
library(tidyverse)
library(lubridate)

#Dayflow -------------

df1 <- read_csv("Data/Dayflow/dayflow-results-1970-1983.csv") %>%
  select(c("Date","Year","Month","SAC","SJR","OUT","EXPORT")) #Sac, SJR, Delta Outflow

df2 <- read_csv("Data/Dayflow/dayflow-results-1984-1996.csv") %>%
  select(c("Date","Year","Month","SAC","SJR","OUT","EXPORT")) #Sac, SJR, Delta Outflow

df3 <- read_csv("Data/Dayflow/dayflow-results-1997-2023.csv") %>%
  select(c("Date","Year","Month","SAC","SJR","OUT","EXPORTS")) #Sac, SJR, Delta Outflow

names(df3)[7] <- "EXPORT"

dayflow <- rbind(df1,df2,df3)
dayflow$Date <- as.Date(dayflow$Date, format="%m/%d/%Y")
rm(df1)
rm(df2)
rm(df3)
rm(df_month)

#merge with SVI

svi <- read_csv("Data/Dayflow/wy_class_ind.csv")
dates <- as.data.frame(seq.Date(ymd(19501201),ymd(20221201),by="day")) %>%
  rename(Date=names(.)) %>%
  mutate(WY = as.numeric(format(ymd(Date) %m+% months(3),"%Y")))

svi <- merge(svi,dates,by="WY",all.y=T) %>% select(c("Date","Yr-type","Index"))
dayflow_wy <- merge(dayflow,svi,by="Date", format = "%Y/%m/%d")
rm(dayflow)

#bring in edi data

source("edi.731.7.r") #clean edi file

rm("dt2") #remove extra df

#add a column for month
colnames(dt1)

#create year_month column and aggregate to monthly data.
#QUESTION: what do I do with < values? filter them out? 

#change < values according to DWR Method:
# Therefore, we interpolated values for the NDs and evaluated the robustness of our statistical tests using interpolated data. For each analyte, we ran the type-II ANOVA three times, where for each run, we substituted NDs with values randomly drawn from a uniform distribution between 0.01 and the lab’s reporting limit for the analyte.


LOD <- function(data,colname_sign) {
  index <- grep(colname_sign,colnames(data))
  data[[index+1]][data[[index]] %in% "<" & !(data[[index+1]] == 0) & !is.na(data[[index+1]])] <-
    runif(n=nrow(data[data[[index]] %in% "<"& !(data[[index+1]] == 0) & !is.na(data[[index+1]]),]),
          min=0,
          max=data[[index+1]][data[[index]] %in% "<"& 
                                !(data[[index+1]] == 0) &
                                !is.na(data[[index+1]])]) #or whatever factor we want to replace with
  return(data)
}

colname_sign <- dt1 %>% select(ends_with("Sign")) %>% colnames

for(n in 1:21) {
  dt1 <- LOD(dt1,colname_sign[n])
}

# AGGREGATE TO MONTH AND MERGE ---------------
dt1 <- dt1 %>% 
  mutate(Date = as.Date(as.character(Date)))
dt_df <- merge(dt1,dayflow_wy,by="Date",all.y=T)
write.csv(dt_df,"Data/edi_df_integrate_daily.csv")
