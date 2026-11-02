### pSEM analysis of chlorophyll data

library(piecewiseSEM)
library(tidyverse)

#### Load data ####
# variables to include in the analysis:
# c = c("Chlorophyll","logchla", "Regions", "Date","Year", "Month",  "season","SAC",
#       "EXPORT","logEXPORT","OUT", "DissAmmonia",
#       "TotPhos","DissNitrateNitrite", 
#       "Temperature","lagTemperature","Secchi", 
#       "Conductivity","season_num", "DissOrthophos",  "clam_step","logDissNitrateNitrite","logDissAmmonia","logDissOrthophos","logConductivity","logSAC","logOUT")#, "Biomass","Grazing_rate","Filtration_rate", "SJR","lagSJR", "Index","lagIndex")

df <- read.csv("Data/regional_integrated_dataset3.csv" )
df$Date = as.Date(df$Date)

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

df<- df %>%
  mutate(log_chl_plus1 = log(Chlorophyll+1))

### visualize data ####

ggplot(df,aes(x =Date, y = Chlorophyll, color = Regions ))+
  geom_line()+
  theme_classic(base_size = 24)

ggplot(df,aes(x =Date, y = Secchi, color = Regions ))+
  geom_line()+
  theme_classic(base_size = 24)


ggplot(df, aes(x = log_chl_plu1)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
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


df_subset <- df %>%
  mutate(lagDIN=ifelse(!is.na(lagDissNitrateNitrite) & !is.na(lagDissAmmonia), lagDissNitrateNitrite+lagDissAmmonia, NA)) %>%
  subset(select = c("Date", "Regions","Chlorophyll", "log_chl_plus1", "Temperature", "OUT","lagDIN", "lagDissNitrateNitrite", "lagDissAmmonia","Secchi"))

### run pSEM ####

#### Suisuin Bay
Region = "Suisin Bay"
Region = "South"
phytoplankton_psem <- psem(
  glm(Temperature ~ OUT,  data = df_subset %>% filter(Regions==Region)) ,
  
  glm(log_chl_plus1 ~ OUT+Temperature+lagDIN+ Secchi,  data = df_subset %>% filter(Regions==Region),family= gaussian() )
  
)

DF_phytoplankton_psem<-coefs(phytoplankton_psem, standardize = "scale")
summary(phytoplankton_psem)
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
