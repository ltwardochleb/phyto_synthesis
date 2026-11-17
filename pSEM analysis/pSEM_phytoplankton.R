### pSEM analysis of chlorophyll data

library(piecewiseSEM)
library(tidyverse)
library(nlme)
library(slider)
library(cimir)
library(purrr)
library(zoo)
library(imputeTS)

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
# Make DIN variable and fill in missing flow values for 1980 January
df<-df%>%
  mutate(DIN=ifelse(!is.na(DissNitrateNitrite) & !is.na(DissAmmonia), DissNitrateNitrite+DissAmmonia, NA))  %>%
  group_by(Date) %>%
  fill(OUT, .direction = "up")%>%
  fill(SAC, .direction = "up")%>%
  fill(SJR, .direction = "up")
  
# Get % missing data
df_NA <- df %>%
  group_by(Regions) %>%
  mutate(NA_chl =100*sum(is.na(Chlorophyll))/n() ) %>%
  mutate(NA_turb =100*sum(is.na(TurbidityNTU))/n() ) %>%
  mutate(NA_NH4 =100*sum(is.na(DissAmmonia))/n() ) %>%
  mutate(NA_NO23 =100*sum(is.na(DissNitrateNitrite))/n() )%>%
  mutate(NA_DIN =100*sum(is.na(DIN))/n() ) %>%
  mutate(NA_PO4 =100*sum(is.na(DissOrthophos))/n() )%>%
  mutate(NA_Si =100*sum(is.na(DissSilica))/n() )%>%
  subset(select = c(Regions, NA_chl, NA_turb,NA_NH4,  NA_NO23, NA_PO4,NA_Si,NA_DIN)) %>%
  unique()

ggplot(df, aes(x = Date, y = Regions,color = !is.na(TurbidityNTU), fill = !is.na(TurbidityNTU))) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis_c( option = "magma",  direction = 1,  name = "Completeness (%)") +
  scale_x_date(date_breaks = "2 years", date_labels = "%b %Y") +
  labs(  title = "Turbidity",  x = "Date",  y = "Region" ) +
  theme_classic(base_size = 24) +
  theme( axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank() )

ggplot(df, aes(x = Date, y = Regions,color = !is.na(DissOrthophos), fill = !is.na(DissOrthophos))) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis_c( option = "magma",  direction = 1,  name = "Completeness (%)") +
  scale_x_date(date_breaks = "2 years", date_labels = "%b %Y") +
  labs(  title = "Phosphate",  x = "Date",  y = "Region" ) +
  theme_classic(base_size = 24) +
  theme( axis.text.x = element_text(angle = 45, hjust = 1),
         panel.grid = element_blank() )

ggplot(df, aes(x = Date, y = TurbidityNTU,color = Regions)) +
  geom_line()+
  theme_classic(base_size = 24)
  
### Interpolate data
df <- df%>%
  arrange(Regions, Date) %>%
  group_by(Regions) %>%
  mutate(
    ts_val = ts(DissOrthophos, frequency = 12),
    DissOrthophos = as.numeric(na_kalman(ts_val, model = "StructTS", smooth = TRUE)),
    
    ts_val = ts(DissAmmonia, frequency = 12),
    DissAmmonia = as.numeric(na_kalman(ts_val, model = "StructTS", smooth = TRUE)),
    
    ts_val = ts(DissNitrateNitrite, frequency = 12),
    DissNitrateNitrite = as.numeric(na_kalman(ts_val, model = "StructTS", smooth = TRUE)),
    
    ts_val = ts(DIN, frequency = 12),
    DIN = as.numeric(na_kalman(ts_val, model = "StructTS", smooth = TRUE)),
    
    ts_val = ts(Conductivity, frequency = 12),
    Conductivity = as.numeric(na_kalman(ts_val, model = "StructTS", smooth = TRUE)),
    
    ts_val = ts(DissSilica, frequency = 12),
    DissSilica = as.numeric(na_kalman(ts_val, model = "StructTS", smooth = TRUE)),
    
    ts_val = ts(Chlorophyll, frequency = 12),
    Chlorophyll = as.numeric(na_kalman(ts_val, model = "StructTS", smooth = TRUE)),
    
    ts_val = ts(Temperature, frequency = 12),
    Temperature = as.numeric(na_kalman(ts_val, model = "StructTS", smooth = TRUE))
  ) %>%
  ungroup() %>%
  select(-ts_val)
  
# set negative interpolated values to minimum positive value 
df<- df %>%
  mutate(DissAmmonia = if_else(DissAmmonia < 0, 0.00006256305, DissAmmonia) )%>%
  mutate(Chlorophyll = if_else(Chlorophyll < 0, 0.03827769, Chlorophyll) )


ggplot(df, aes(x = Date, y = DissOrthophos,color = Regions)) +
  geom_line()+
  theme_classic(base_size = 24)

ggplot(df, aes(x = Date, y = Chlorophyll,color = Regions)) +
  geom_line()+
  theme_classic(base_size = 24)

### Get discharge value
unique(df$Regions)
df<- df %>%
  mutate(Discharge = ifelse(Regions=="North Delta", SAC, OUT))

### Get 3 month lags of data



df<- df %>%
  group_by(Regions) %>%
  arrange(Date) %>%
  mutate(lag3_DissSilica = lag(DissSilica, n =3),
         lag3_Chlorophyll = lag(Chlorophyll, n =3),
         lag3_Temperature = lag(Temperature, n =3),
         lag3_DIN = lag(DIN, n =3),
         lag3_Conductivity = lag(Conductivity, n =3),
         lag3_DissAmmonia = lag(DissAmmonia, n =3),
         lag3_DissNitrateNitrite = lag(DissNitrateNitrite, n =3),
         lag3_DissOrthophos = lag(DissOrthophos, n =3),
         lag3_OUT = lag(OUT, n =3),
         lag3_Discharge = lag(Discharge, n =3),
         lag3_TurbidityNTU = lag(TurbidityNTU, n =3)
  ) %>%
  mutate(year_month = as.yearmon(Date)) %>%
   mutate(log_chl_plus1 = log(Chlorophyll+1))  %>%
  ungroup() %>%
  group_by(Date)%>%
  fill(clam_step, .direction = "up") %>%
  fill(season, .direction = "up")%>%
  fill(Month, .direction = "up")#%>%
  # mutate(lagDIN=ifelse(!is.na(lagDissNitrateNitrite) & !is.na(lagDissAmmonia), lagDissNitrateNitrite+lagDissAmmonia, NA)) %>%
  # group_by(Regions) %>%
  # arrange(Date) %>%
  # mutate(DissSilica_lag_1mo = lag(DissSilica),
  #        DissSilica_lag_5mo = lag(DissSilica, n = 5), # This might be the way to go
  #        DissSilica_lag_3mo_av = slide_index_mean(DissSilica, i = Date, before = ~ .x %m-% months(3), after = days(-1), complete = FALSE, na_rm = TRUE)) %>%
  # mutate(year_month = as.yearmon(Date))
  
  
### Download precipitation data
remove_key()
cimir::set_key("791fb4ea-2cac-47f3-8a06-582ab58592c7") ### My key, change this for publication
is_key_set()

station_metadata <- cimis_station()

# 2. Extract and format the active periods 
station_active_ranges <- station_metadata %>%
  select(StationNbr, Name, County, IsActive, ConnectDate, DisconnectDate) %>%
  mutate(
    ConnectDate = as.Date(ConnectDate, format ="%m/%d/%Y"),
    # If it's active, it's still running "Today" rather than the placeholder 2100 date
    EndDate = if_else(IsActive == "True", Sys.Date(), as.Date(DisconnectDate, format ="%m/%d/%Y"))
  ) %>% unique()

# 3. View the date ranges
delta_stations <- c(47) 

# 3. Specify data items to download (Daily parameters)
# Learn more via cimis_items("Daily")
data_items <- c(
  "day-precip"
)

# 4. Generate annual date windows (CIMIS limits queries to 1-year max)
# Note: Data begins mid-1982, but loop runs through requested period safely
years <- 1985:2022 #1985
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
    df <- cimir::cimis_data(
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
  mutate(lag3_precip_month = lag(precip_month_sum, n =3)) 
  



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
ggplot(df, aes(x = log_chl_plus1)) +
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


df_subset <- df  %>% mutate(clam_step= ifelse(clam_step=="pre clam", 0, 1)) %>%
  subset(select = c("Date", "season", "Month", "Regions","Chlorophyll", "log_chl_plus1", "Temperature",
                    "OUT", "Discharge","lag3_DIN", "lag3_DissNitrateNitrite", "lag3_DissAmmonia", 
                    "Conductivity",  "TurbidityNTU", "lag3_DissSilica", "lag3_Chlorophyll", "lag3_OUT", #"precip_month_lag_1mo",
                    "lag3_DissOrthophos", "DissOrthophos", "DIN", "lag3_Discharge", "DissNitrateNitrite", "DissAmmonia", "clam_step", "lag3_precip_month", "precip_month_sum"))

colSums(is.na(df_subset))

df_subset_no_na <- df_subset %>% subset(select = -c(lag3_precip_month, precip_month_sum, TurbidityNTU)) %>% na.omit() #2098 rows remain

### run pSEM ####

#### Suisuin Bay
Region = "Suisun Bay"
Region = "South"
Region = "Confluence"
Region ="North Delta"
Region = "Central"

phytoplankton_psem <- psem(
  lme(Discharge ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset %>% filter(Regions==Region)) ,
  lme(Temperature ~ Discharge+lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset %>% filter(Regions==Region)) ,
  lme(Conductivity ~ Discharge+lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset %>% filter(Regions==Region)) ,
  lme(lag3_DIN ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset %>% filter(Regions==Region)) ,
  lme(lag3_DissSilica ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset %>% filter(Regions==Region)),
  lme(lag3_DissOrthophos ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset %>% filter(Regions==Region)) ,
  
  lme(log_chl_plus1 ~ Discharge+lag3_Discharge+Temperature+lag3_DIN+ Conductivity+lag3_DissSilica+lag3_DissOrthophos,
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



## test model with no region
phytoplankton_psem <- psem(
  lme(Discharge ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  lme(Temperature ~ Discharge+lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  lme(Conductivity ~ Discharge+lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  lme(lag3_DIN ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  lme(lag3_DissSilica ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ),
  lme(lag3_DissOrthophos ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  
  lme(log_chl_plus1 ~ Discharge+lag3_Discharge+Temperature+lag3_DIN+ Conductivity+lag3_DissSilica+lag3_DissOrthophos+lag3_Chlorophyll+clam_step,
      random = ~ 1 | Month, na.action = na.omit,  data = df_subset  )
  
)

summary(phytoplankton_psem)

### Test models with NH4 and NO23 separate

phytoplankton_psem <- psem(
  lme(Discharge ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  lme(Temperature ~ Discharge+lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  lme(Conductivity ~ Discharge+lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  lme(lag3_DissNitrateNitrite ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ),
  lme(lag3_DissAmmonia ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  lme(lag3_DissSilica ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ),
  lme(lag3_DissOrthophos ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  
  lme(log_chl_plus1 ~ Discharge+lag3_Discharge+Temperature+lag3_DissNitrateNitrite+lag3_DissAmmonia+ Conductivity+lag3_DissSilica+lag3_DissOrthophos+lag3_Chlorophyll+clam_step,
      random = ~ 1 | Month, na.action = na.omit,  data = df_subset  )
  
)

summary(phytoplankton_psem)

## test model with precipitation
phytoplankton_psem <- psem(
  lme(Discharge ~ lag3_Discharge+lag3_precip_month, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  lme(Temperature ~ Discharge+lag3_Discharge+lag3_precip_month, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  lme(Conductivity ~ Discharge+lag3_Discharge+lag3_precip_month, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  lme(lag3_DIN ~ lag3_Discharge+lag3_precip_month, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  lme(lag3_DissSilica ~ lag3_Discharge+lag3_precip_month, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ),
  lme(lag3_DissOrthophos ~ lag3_Discharge+lag3_precip_month, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  
  lme(log_chl_plus1 ~ Discharge+lag3_Discharge+Temperature+lag3_DIN+ Conductivity+lag3_DissSilica+lag3_DissOrthophos+lag3_Chlorophyll+clam_step+lag3_precip_month,
      random = ~ 1 | Month, na.action = na.omit,  data = df_subset  )
)


summary(phytoplankton_psem)

## test model with no region and current DIN + PO4
phytoplankton_psem <- psem(
  lme(Discharge ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  lme(Temperature ~ Discharge+lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  lme(Conductivity ~ Discharge+lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  lme(lag3_DIN ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  lme(lag3_DissSilica ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ),
  lme(lag3_DissOrthophos ~ lag3_Discharge, random = ~ 1 | Month, na.action = na.omit,  data = df_subset ) ,
  
  lme(log_chl_plus1 ~ Discharge+lag3_Discharge+Temperature+lag3_DIN+ Conductivity+lag3_DissSilica+lag3_DissOrthophos+lag3_Chlorophyll+clam_step+DissOrthophos+DIN,
      random = ~ 1 | Month, na.action = na.omit,  data = df_subset  )
  
)

summary(phytoplankton_psem)

### Try model with autocorrelation

df_subset_no_na <- df_subset_no_na %>%
  mutate(
    Regions = as.factor(Regions),
    # Extract month as an unordered factor (1 to 12)
    Month = factor(format(Date, "%m"))
  )%>%
  arrange(Regions, Date) %>%
  group_by(Regions) %>%
  mutate(time_index = row_number()) %>%
  ungroup()


phytoplankton_psem <- psem(
  lme(Discharge ~ lag3_Discharge, random = ~ 1 | Regions/ Month,   correlation = corAR1(form = ~ time_index | Regions),  data = df_subset_no_na ) ,
  lme(Temperature ~ Discharge+lag3_Discharge, random = ~ 1 | Regions/ Month,   correlation = corAR1(form = ~ time_index | Regions),  data = df_subset_no_na ) ,
  lme(Conductivity ~ Discharge+lag3_Discharge, random = ~ 1 | Regions/ Month,   correlation = corAR1(form = ~ time_index | Regions),  data = df_subset_no_na ) ,
  lme(lag3_DIN ~ lag3_Discharge, random = ~ 1 | Regions/ Month,   correlation = corAR1(form = ~ time_index | Regions),  data = df_subset_no_na ) ,
  lme(lag3_DissSilica ~ lag3_Discharge, random = ~ 1 | Regions/ Month,   correlation = corAR1(form = ~ time_index | Regions),  data = df_subset_no_na ),
  lme(lag3_DissOrthophos ~ lag3_Discharge, random = ~ 1 | Regions/ Month,   correlation = corAR1(form = ~ time_index | Regions),  data = df_subset_no_na ),
  lme(DissOrthophos ~ lag3_Discharge+Discharge, random = ~ 1 | Regions/ Month,   correlation = corAR1(form = ~ time_index | Regions),  data = df_subset_no_na ),
  lme(DIN ~ lag3_Discharge+Discharge, random = ~ 1 | Regions/ Month,   correlation = corAR1(form = ~ time_index | Regions),  data = df_subset_no_na ) ,
  
  lme(log_chl_plus1 ~ Discharge+lag3_Discharge+Temperature+lag3_DIN+ Conductivity+lag3_DissSilica+lag3_DissOrthophos+lag3_Chlorophyll+clam_step+DissOrthophos+DIN,
      random = ~ 1 | Regions/ Month,   correlation = corAR1(form = ~ time_index | Regions),  data = df_subset_no_na  )
  
)

summary(phytoplankton_psem)




