#Alex attempt at setting up bivariate GAMs for determining threshold parameters

library(tidyverse)
library(mgcv)
library(MuMIn)
library(gratia)

#Setting up GAMs for threshold

wq <- read.csv("Data/edi_df_integrate_monthly.csv")

#1. Assign region to each station
# do we already have a regional assignment somewhere?

#2. Calculate monthly average values for each region

#3. Assign a season to each month 
wq$season <- replicate(nrow(wq),NA)
wq$season[wq$Month %in% c(12,1,2)] <- "winter"
wq$season[wq$Month %in% c(3,4,5)] <- "spring"
wq$season[wq$Month %in% c(6,7,8)] <- "summer"
wq$season[wq$Month %in% c(9,10,11)] <- "fall"
