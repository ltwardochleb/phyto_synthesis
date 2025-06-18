#Dayflow and Discrete WQ Data integration - edi
library(tidyverse)
library(lubridate)

#Dayflow -------------

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
  dt1 <- LOD2(dt1,colname_sign[n])
}

# AGGREGATE TO MONTH AND MERGE ---------------
dt1_mo <- dt1 %>% 
  mutate(Date = as.Date(as.character(Date))) %>%
  mutate(year_month = parse_date(paste(year(Date),month(Date)),"%Y %m")) %>%
  group_by(Source,Station,Latitude,Longitude,year_month) %>%
  summarise_if(is.numeric,mean)
help(mutate)
dt_df <- merge(dt1_mo,df_month,by="year_month",all.x=T)
write.csv(dt_df,"Data/edi_df_integrate_monthly.csv")


# OLD WIP -------------


# replace_blw_rl <- function(df, min_val = 0, seed = 1) {
#   # Pull out values that are below the RL
#   df_blw_rl <- df %>% filter(Sign == "<")
#   
#   # Replace below RL values with simulated ones
#   withr::with_seed(
#     # Set seed for reproducibility
#     seed = seed,
#     df_blw_rl_sim <- df_blw_rl %>% 
#       mutate(Result = round(runif(nrow(df_blw_rl), min = min_val, max = Result), 6))
#   )
#   
#   # Add simulated values back to main data frame
#   df %>% filter(Sign != "<") %>% bind_rows(df_blw_rl_sim)
# }

# data <- dt1
# colname_sign <- "VSS_Sign"
# index <- grep(colname_sign,colnames(data))
# data[[index+1]][data[[index]] %in% "<" & !(data[[index+1]] == 0) & !is.na(data[[index+1]])] <-
#   runif(n=nrow(data[data[[index]] %in% "<"& !(data[[index+1]] == 0) & !is.na(data[[index+1]]),]),
#         min=0,
#         max=data[[index+1]][data[[index]] %in% "<"& 
#                               !(data[[index+1]] == 0) &
#                               !is.na(data[[index+1]])])#or whatever factor we want to replace with#or whatever factor we want to replace with


# 
# LOD2 <- function(data,colname_sign) {
#   index <- grep(colname_sign,colnames(data))
#   data[[index+1]][data[[index]] %in% "<" & !(data[[index+1]] == 0) & !is.na(data[[index+1]])] <-
#     runif(n=nrow(data[data[[index]] %in% "<"& !(data[[index+1]] == 0) & !is.na(data[[index+1]]),]),
#           min=0,
#           max=data[[index+1]][data[[index]] %in% "<"& 
#                                 !(data[[index+1]] == 0) &
#                                 !is.na(data[[index+1]])])#or whatever factor we want to replace with#or whatever factor we want to replace with
#   return(data)
# }
# 
# colname_sign <- dt1 %>% select(ends_with("Sign")) %>% colnames
# 
# for(n in 1:21) {
#   dt1 <- LOD2(dt1,colname_sign[n])
# }















