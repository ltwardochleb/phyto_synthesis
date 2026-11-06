### pSEM analysis of chlorophyll data

library(piecewiseSEM)
library(tidyverse)
library(nlme)
library(slider)
library(cimir)
library(purrr)
library(zoo)

#### Load data ####
# variables to include in the analysis:
# c = c("Chlorophyll","logchla", "Regions", "Date","Year", "Month",  "season","SAC",
#       "EXPORT","logEXPORT","OUT", "DissAmmonia",
#       "TotPhos","DissNitrateNitrite", 
#       "Temperature","lagTemperature","Secchi", 
#       "Conductivity","season_num", "DissOrthophos",  "clam_step","logDissNitrateNitrite","logDissAmmonia","logDissOrthophos","logConductivity","logSAC","logOUT")#, "Biomass","Grazing_rate","Filtration_rate", "SJR","lagSJR", "Index","lagIndex")

df <- read.csv("Data/regional_integrated_dataset3.csv" )
df$Date = as.Date(df$Date)

df_test <-df %>% subset(select = c(Date, Regions,Secchi,TurbidityNTU, TurbidityFNU))

df_test <-df %>% subset(select = c(Date, Regions,DissNitrateNitrite,lagDissNitrateNitrite))
#lag value is mean of previous season in region


# 1979 is the start of the study period.
df <- subset(df,df$Date >= "1979-01-01")
# 2022 is the end of the study period
df <- subset(df,df$Date <= "2022-12-31")

# Changing categorical variables to factors
# need to change regions to factor farther down so dataframe splitting by region works properly
#df$Regions = as.factor(df$Regions)
df$season = as.factor(df$season)
df$season_num = as.numeric(df$season)


# Replacing clam variables with a step function (pre/post potamocorbula introduction to the Bay in 1987)
df <- df%>%mutate(clam_step=if_else(Year>1987, "post clam", "pre clam")) 
df$clam_step <- as.factor(df$clam_step)

# Check all dates present
start_date <- min(df$Date)
end_date <-  max(df$Date)

# Generate the sequence of dates
date_sequence <- seq.Date(from = start_date, to = end_date, by = "month")
date_sequence <- as.data.frame(date_sequence)


Site_list<- unique(df$Regions)
Site_list<- as.data.frame(Site_list)
#cross join with full site list
Sites_date<-cross_join(date_sequence,Site_list)
Sites_date <- Sites_date %>%
  rename(Date =date_sequence ,
         Regions=Site_list)
# left_join with discrete sample
df<- left_join(Sites_date,df) # 1980 January Central missing, added in as NA's

colnames(df)
df_Si_lag<- df  %>%
  group_by(season, Regions) %>%
  mutate(DissSilica_av = mean(DissSilica, na.rm= TRUE),
         Chlorophyll_av= mean(Chlorophyll, na.rm= TRUE)) %>%
  subset(select = c(Date,Year, season, Regions, DissSilica_av,Chlorophyll_av)) %>%
  unique()%>%
  arrange(Date)%>%
  group_by( Regions)%>%
  mutate(lagDissSilica = lag(DissSilica_av),
         lagChlorophyll = lag(Chlorophyll_av)) %>%
  subset(select = c(Year, season, Regions, lagDissSilica,lagChlorophyll)) %>%
  unique()
  
df<- left_join( df, df_Si_lag)

df<- df %>%
  mutate(log_chl_plus1 = log(Chlorophyll+1))%>%
  mutate(lagDIN=ifelse(!is.na(lagDissNitrateNitrite) & !is.na(lagDissAmmonia), lagDissNitrateNitrite+lagDissAmmonia, NA)) %>%
  group_by(Regions) %>%
  arrange(Date) %>%
  mutate(DissSilica_lag_1mo = lag(DissSilica),
         DissSilica_lag_5mo = lag(DissSilica, n = 5), # This might be the way to go
         DissSilica_lag_3mo_av = slide_index_mean(DissSilica, i = Date, before = ~ .x %m-% months(3), after = days(-1), complete = FALSE, na_rm = TRUE)) %>%
  mutate(year_month = as.yearmon(Date))
  
  
### Download precipitation data
remove_key()
cimir::set_key("791fb4ea-2cac-47f3-8a06-582ab58592c7") ### My key, change this for publication
is_key_set()

station_metadata <- cimis_station()

# 2. Extract and format the active periods 
station_active_ranges <- station_metadata %>%
  select(StationId, Name, County, Status, ConnectDate, DisconnectDate) %>%
  mutate(
    ConnectDate = as.Date(ConnectDate),
    # If it's active, it's still running "Today" rather than the placeholder 2100 date
    EndDate = if_else(Status == "Active", Sys.Date(), as.Date(DisconnectDate))
  )

# 3. View the date ranges
delta_stations <- c(47) 

# 3. Specify data items to download (Daily parameters)
# Learn more via cimis_items("Daily")
data_items <- c(
  "day-precip"
)

# 4. Generate annual date windows (CIMIS limits queries to 1-year max)
# Note: Data begins mid-1982, but loop runs through requested period safely
years <- 1985:2022
date_ranges <- data.frame(
  start = paste0(years, "-01-01"),
  end = paste0(years, "-12-31"),
  stringsAsFactors = FALSE
)

print("Starting CIMIS data download loop...")

# 5. Fetch data sequentially using a safe map loop
delta_data_list <- purrr::map2(date_ranges$start, date_ranges$end, function(s_date, e_date) {
  Sys.sleep(1) # Polite pause to avoid hitting API rate limits
  
  tryCatch({
    message(paste("Downloading:", s_date, "to", e_date))
    
    # Query API via cimir
    df <- cimis_data(
      targets = delta_stations, 
      start.date = s_date, 
      end.date = e_date, 
      items = data_items,
      measure.unit = "M" # "M" for Metric, "E" for English
    )
    return(df)
  }, error = function(e) {
    message(paste("No data found or error for period:", s_date, "to", e_date))
    return(NULL)
  })
})

# 6. Bind into one clean data frame
final_delta_data <- bind_rows(delta_data_list)

final_delta_data <-final_delta_data %>%
  mutate(Date= as.Date(Date)) %>%
  mutate(year_month = as.yearmon(Date)) %>%
  group_by(year_month) %>%
  mutate(precip_month_sum = sum(Value)) %>%
  subset(select = c(year_month, precip_month_sum)) %>%
  unique() 

date_sequence <- seq.Date(from = min(final_delta_data$year_month), to = max(final_delta_data$year_month), by = "month")
date_sequence <- as.data.frame(date_sequence) %>%
  mutate(year_month = as.yearmon(date_sequence)) %>%
  subset(select= c(year_month))

final_delta_data <-left_join(date_sequence, final_delta_data)

final_delta_data <-final_delta_data%>%
  ungroup() %>%
  arrange(year_month) %>%
  mutate(precip_month_lag_1mo = lag(precip_month_sum)) 
  



df<-left_join(df, final_delta_data)

### visualize data ####

ggplot(df,aes(x =Date, y = Chlorophyll, color = Regions ))+
  geom_line()+
  theme_classic(base_size = 24)

ggplot(df,aes(x =Date, y = Salinity, color = Regions ))+
  geom_line()+
  theme_classic(base_size = 24)

ggplot(df,aes(x =TurbidityFNU, y = TurbidityNTU, color = Regions ))+
  geom_smooth(method= "lm")+
  theme_classic(base_size = 24)

# looks pretty close to normal, large sample size helps
ggplot(df, aes(x = log_chl_plu1)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ Regions) +
  labs(
    x = "Chlorophyll (chl)",
    y = "Count"
  ) +
  theme_minimal(base_size = 24)

ggplot(df, aes(x = Temperature)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ Regions) +
  labs(
    x = "Chlorophyll (chl)",
    y = "Count"
  ) +
  theme_minimal(base_size = 24)

ggplot(df, aes(x = Conductivity, y =log_chl_plus1 )) +
  geom_smooth(method = "lm") +
  facet_wrap(~ Regions) +
  labs(
    x = "Chlorophyll (chl)",
    y = "Count"
  ) +
  theme_minimal(base_size = 24)

# 
# # log transforming some variables to better visualize
# df$logDissNitrateNitrite <- log(df$DissNitrateNitrite)
# df$logDissAmmonia <- log(df$DissAmmonia)
# df$logDissOrthophos <- log(df$DissOrthophos)
# df$logConductivity <- log(df$Conductivity)
# df$logSAC <- log(df$SAC)
# df$logOUT <- log(df$OUT)
# df$logEXPORT <- log(df$EXPORT)
# 
# df <- subset(df, select = c)


## Remove outliers {.tabset}

### Investigating problem variables

histogram(df$logDissAmmonia)
boxplot(df$logDissAmmonia)
histogram(df$logDissOrthophos)
boxplot(df$logDissOrthophos)
histogram(df$logDissNitrateNitrite)
boxplot(df$logDissNitrateNitrite)


### Remove outliers based on z-score

# removing outliers based on z-score, assuming the log transformations of the targeted variables have a normal distribution.
# 
# -3 - 3 is a commonly used range beyond which values are considered outliers.
# https://statsandr.com/blog/outliers-detection-in-r/#z-scores 
#   
  
# 
# clean_df_z <- df 
# 
# clean_df_z$logDissAmmonia[abs(scale(clean_df_z$logDissAmmonia)) > 3.29] <- NA
# 
# clean_df_z$logDissNitrateNitrite[abs(scale(clean_df_z$logDissNitrateNitrite)) > 3.29] <- NA
# 
# clean_df_z$logDissOrthophos[abs(scale(clean_df_z$logDissOrthophos)) > 3.29] <- NA
# 
# outliers_z <- anti_join(df, clean_df_z)
# 
# df <- clean_df_z
# 


df_subset <- df  %>%
  subset(select = c("Date", "season", "Month", "Regions","Chlorophyll", "log_chl_plus1", "Temperature",
                    "OUT","lagDIN", "lagDissNitrateNitrite", "lagDissAmmonia", 
                    "Conductivity",  "TurbidityNTU", "lagDissSilica", "lagChlorophyll", "lagOUT", "precip_month_lag_1mo",
                    "lagDissOrthophos"))

df_subset_test <- df_subset %>% na.omit() #2098 rows remain

### run pSEM ####

#### Suisuin Bay
Region = "Suisun Bay"
Region = "South"
Region = "Confluence"
Region ="North Delta"
Region = "Central"

phytoplankton_psem <- psem(
  lme(OUT ~ precip_month_lag_1mo+lagOUT, random = ~ 1 | Month, na.action = na.omit,  data = df_subset %>% filter(Regions==Region)) ,
  lme(Temperature ~ OUT+precip_month_lag_1mo+lagOUT, random = ~ 1 | Month, na.action = na.omit,  data = df_subset %>% filter(Regions==Region)) ,
  lme(TurbidityNTU ~ OUT+precip_month_lag_1mo+lagOUT, random = ~ 1 | Month, na.action = na.omit,  data = df_subset %>% filter(Regions==Region)) ,
  lme(Conductivity ~ OUT+precip_month_lag_1mo+lagOUT, random = ~ 1 | Month, na.action = na.omit,  data = df_subset %>% filter(Regions==Region)) ,
  lme(lagDIN ~ lagOUT+precip_month_lag_1mo, random = ~ 1 | Month, na.action = na.omit,  data = df_subset %>% filter(Regions==Region)) ,
  lme(lagDissSilica ~ lagOUT+precip_month_lag_1mo, random = ~ 1 | Month, na.action = na.omit,  data = df_subset %>% filter(Regions==Region)),
  lme(lagDissOrthophos ~ lagOUT+precip_month_lag_1mo, random = ~ 1 | Month, na.action = na.omit,  data = df_subset %>% filter(Regions==Region)) ,
  
  lme(log_chl_plus1 ~ OUT+lagOUT+Temperature+lagDIN+ TurbidityNTU+ Conductivity+lagDissSilica+precip_month_lag_1mo+lagDissOrthophos,
      random = ~ 1 | Month, na.action = na.omit,  data = df_subset %>% filter(Regions==Region) )
  
)

summary(phytoplankton_psem)

DF_phytoplankton_psem<-coefs(phytoplankton_psem, standardize = "scale")

#Directed separation tests state if there is a significant relationship in an untested relationship.
# C is significant which suggests links are not included in the model that would be significant
# As sample size increases, the probability of recovering a “significant” P-vaue increases, reducing the potential for a good-fitting model.
#Sample size may be too large for FishersC to be helpful

#Standardize standard error and get confidence intervals
names(DF_phytoplankton_psem)[names(DF_phytoplankton_psem) == 'Std.Estimate'] <- 'Std_Estimate'
names(DF_phytoplankton_psem)[names(DF_phytoplankton_psem) == 'Std.Error'] <- 'Std_Error'
names(DF_phytoplankton_psem)[names(DF_phytoplankton_psem) == ''] <- 'Significance'

DF_phytoplankton_psem <- DF_phytoplankton_psem %>%
  mutate(std_error_std_estimate = (Std_Estimate/Estimate)*Std_Error) %>%
  mutate(lower_ci_95 = Std_Estimate - 1.96*std_error_std_estimate)%>%
  mutate(upper_ci_95 = Std_Estimate + 1.96*std_error_std_estimate)
