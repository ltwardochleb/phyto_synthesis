# R Script for Global GAM
# Author: Ryan Hankins

#Resources
#https://gsp.humboldt.edu/olm/R/05_03_GAM.html
#https://www.geeksforgeeks.org/r-language/generalized-additive-models-using-r/

library(mgcv)
library(tidyverse)
library(marginaleffects)
library(MuMIn)
library(car)
library(corrplot)
library(stats)

#Chla~ NH4 + PO4 + (NO2+NO3) + temperature + secchi + 
#  conductivity + Sac Valley index + NDOI + Region + Season + 
#  Region*Season + Clam biomass + previous year(Sac Valley index) + 
#  seasonal lag(NDOI) + seasonal lag(NH4) + seasonal lag(PO4) + 
#  seasonal lag (NO2+NO3) + (1|Month) 


c = c( "logchla", "Date","Month",  "season","Regions","SAC","lagSAC",
       "SJR","EXPORT","OUT","lagOUT", "Index","lagIndex","DissAmmonia",
       "lagDissAmmonia", "TotPhos","lagTotPhos","DissNitrateNitrite", 
       "lagDissNitrateNitrite","Temperature","lagTemperature","Secchi", 
       "lagSecchi","Conductivity",  "lagConductivity","Biomass","Grazing_rate",
      "Filtration_rate")

df <- read.csv("Data/regional_integrated_dataset3.csv" )

df$Date = as.Date(df$Date)

df$logchla <- log(df$Chlorophyll)

df <- df %>%
  select(all_of(c)) %>%
  drop_na(all_of(c))



#######################################################
# CHOOSE MODELS WITH THE BEST COMBO OF FLOW VARIABLES #
# Compare with AIC
#######################################################
options(na.action = "na.fail")
m_Index <- gam((logchla) ~ DissAmmonia +
               TotPhos +
               DissNitrateNitrite +
               Temperature +
               Secchi +
               Conductivity +
               Index +
               OUT + 
               season +
               Regions +
               (Regions*season) +
               (OUT*season) +
               Clam_biomass +
               lagIndex +
               lagOUT +
               lagDissAmmonia +
               lagTotPhos +
               lagDissNitrateNitrite+
               # Random effects cannot be set in gam() with the notation 
                # 1|Month
               s(Month,bs="re" ),
             data = df,
             method = 'ML'
             )# changed to ML based on Perry's comment 

m_IndexEXPORT <- gam((logchla) ~ DissAmmonia +
                 TotPhos +
                 DissNitrateNitrite +
                 Temperature +
                 Secchi +
                 Conductivity +
                 Index +
                 OUT + 
                 EXPORT +
                 season +
                 Regions +
                 (Regions*season) +
                 (OUT*season) +
                 Clam_biomass +
                 lagIndex +
                 lagOUT +
                 lagDissAmmonia +
                 lagTotPhos +
                 lagDissNitrateNitrite +
                 s(Month,bs="re" ),
               data = df,
               method = 'ML')# changed to ML based on Perry's comment 

m_SAC <- gam((logchla) ~ DissAmmonia +
                 TotPhos +
                 DissNitrateNitrite +
                 Temperature +
                 Secchi +
                 Conductivity +
                 SAC +
               #removing OUT because it is highly correlated with SAC
                 #OUT + 
                 season +
                 Regions +
               #replace interactions with correct flow variable
                (SAC*season) +
                 (Regions*season) +
                 Clam_biomass +
                 lagIndex +
                 lagSAC +
                 lagDissAmmonia +
                 lagTotPhos +
                 lagDissNitrateNitrite+
                 s(Month,bs="re" ),
               data = df,
               method = 'ML')
# highest performing combination of flow variables:
# using bs=ts for nutrient variables for comparision
m_SACEXPORT <- gam((logchla) ~ DissAmmonia +
               TotPhos +
               DissNitrateNitrite +
               Temperature +
               Secchi +
               Conductivity +
               SAC +
              # OUT + 
               EXPORT +
               season +
               Regions +
               (SAC*season) +
               (Regions*season) +
               Clam_biomass +
               lagIndex +
               lagSAC +
               lagDissAmmonia +
               lagTotPhos +
               lagDissNitrateNitrite +
               s(Month,bs="re" ),
             data = df,
             method = 'ML')
summary(m_SACEXPORT)

m_SJR <- gam((logchla) ~ DissAmmonia +
               TotPhos +
               DissNitrateNitrite +
               Temperature +
               Secchi +
               Conductivity +
               SJR +
               #OUT + 
               season +
               Regions +
               (Regions*season) +
               (SJR*season) +
               Clam_biomass +
               lagIndex +
               lagSJR +
               lagDissAmmonia +
               lagTotPhos +
               lagDissNitrateNitrite+
               s(Month,bs="re" ),
             data = df,
             method = 'ML')


m_SJREXPORT <- gam((logchla) ~ DissAmmonia +
               TotPhos +
               DissNitrateNitrite +
               Temperature +
               Secchi +
               Conductivity +
               SJR +
               EXPORT +
               #OUT + 
               season +
               Regions +
               (Regions*season) +
               (SJR*season) +
               Clam_biomass +
               lagIndex +
               lagSJR +
               lagDissAmmonia +
               lagTotPhos +
               lagDissNitrateNitrite+
               s(Month,bs="re" ),
             data = df,
             method = 'ML')

m_EXPORT<- gam((logchla) ~ DissAmmonia +
                 TotPhos +
                 DissNitrateNitrite +
                 Temperature +
                 Secchi +
                 Conductivity +
                 EXPORT +
                 OUT + 
                 season +
                 Regions +
                 (Regions*season) +
                 (OUT*season) +
                 Clam_biomass +
                 lagIndex +
                 lagOUT +
                 lagDissAmmonia +
                 lagTotPhos +
                 lagDissNitrateNitrite+
                 s(Month,bs="re" ),
               data = df,
               method = 'ML')

ms.1 <- model.sel(m_Index, m_IndexEXPORT, m_SAC, m_SACEXPORT, m_SJR, m_SJREXPORT, m_EXPORT)
ms.1


############################################################################
#identify strongest performing linear models with top flow variable combo  #
############################################################################
#asking dredge to keep flow variables and others that do not have high colinearity due to LONG runtime
m.1.dredge <- dredge(m_SACEXPORT, fixed=c("Temperature","Secchi","Conductivity","season", "Regions",
                                           "Clam_biomass", "EXPORT", "OUT"))
m.1.dredge

m.1.dredge.full <- dredge(m_SACEXPORT,fixed=c("SAC","EXPORT"))

m_SACEXPORT
#best performing model is the one with all of the covariates in m_SACEXPORT
#AICc = 8605.4, equal to the performance of the model sans monthly random effect

#########################################################
# Add shrinkage terms to models with delta AICc of <2  #
#   (#top 4 models)                                     #
#########################################################

# adding smooth terms, shrinking variables we are less certain about including
# having an unknown difficulty with select=TRUE. another option is setting bs="ts"- ts is a thin plate regression
# spline with a modification to the smoothing penalty, so the null space is also penalized slightly and the 
# term can be shrunk to zero, similar to setting select=TRUE except for differences shown in this powerpoint:
# https://eric-pedersen.github.io/mgcv-esa-workshop/slides/03-model-selection.html#/9

m_SACEXPORT_s <- gam((logchla) ~ s(DissAmmonia, bs="ts") +
                     s(TotPhos, bs="ts") +
                     s(DissNitrateNitrite, bs ="ts") +
                     s(Temperature, bs="ts") +
                     s(Secchi, bs="ts")+
                     s(Conductivity, bs="ts") +
                     s(SAC) +
                     s(EXPORT) +
                    # cannot smooth categorical variables
                     season +
                     Regions +
                     (OUT*season) +
                     (Regions*season) +
                     s(Clam_biomass, bs="ts") +
                     s(lagIndex, bs="ts") +
                     s(lagOUT, bs="ts") +
                     s(lagDissAmmonia ,bs="ts") +
                     s(lagTotPhos, bs="ts") +
                     s(lagDissNitrateNitrite, bs="ts") + 
                     s(Month, bs="re"),
                   data = df,
                   method = 'ML')


summary(m_SACEXPORT_s)
help(summary.gam)

m_SACEXPORT_select <- gam((logchla) ~ s(DissAmmonia, select=TRUE) +
                       s(TotPhos, select=TRUE) +
                       s(DissNitrateNitrite,select=TRUE) +
                       s(Temperature,select=TRUE) +
                       s(Secchi,select=TRUE)+
                       s(Conductivity,select=TRUE) +
                       s(SAC) +
                       s(EXPORT) +
                       # cannot smooth categorical variables
                       season +
                       Regions +
                       (OUT*season) +
                       (Regions*season) +
                       s(Clam_biomass,select=TRUE) +
                       s(lagIndex,select=TRUE) +
                       s(lagOUT, select=TRUE) +
                       s(lagDissAmmonia ,select=TRUE) +
                       s(lagTotPhos, select=TRUE) +
                       s(lagDissNitrateNitrite, select=TRUE) +
                       s(Month, bs="re"),
                     data = df,
                     method = 'ML')

m_SACEXPORT_s_no_re <- gam((logchla) ~ s(DissAmmonia, bs="ts") +
                       s(TotPhos, bs="ts") +
                       s(DissNitrateNitrite, bs ="ts") +
                       s(Temperature, bs="ts") +
                       s(Secchi, bs="ts")+
                       s(Conductivity, bs="ts") +
                       s(SAC) +
                       s(EXPORT) +
                       # cannot smooth categorical variables
                       season +
                       Regions +
                       (OUT*season) +
                       (Regions*season) +
                       s(Clam_biomass, bs="ts") +
                       s(lagIndex, bs="ts") +
                       s(lagOUT, bs="ts") +
                       s(lagDissAmmonia ,bs="ts") +
                       s(lagTotPhos, bs="ts") +
                       s(lagDissNitrateNitrite, bs="ts"),
                     data = df,
                     method = 'ML')

m_2_s <- gam((logchla) ~ s(DissAmmonia, bs="ts") +
                   s(TotPhos, bs="ts") +
                   s(DissNitrateNitrite, bs ="ts") +
                   s(Temperature, bs="ts") +
                   s(Secchi, bs="ts")+
                   s(Conductivity, bs="ts") +
                   s(SAC) +
                   s(EXPORT) +
                   # cannot smooth categorical variables
                   season +
                   Regions +
                   (OUT*season) +
                   (Regions*season) +
                   s(Clam_biomass, bs="ts") +
                   s(lagIndex, bs="ts") +
                   s(lagOUT, bs="ts") +
                   s(lagDissAmmonia ,bs="ts") +
                   s(lagTotPhos, bs="ts") +
                   s(Month, bs="re"),
                 data = df,
                 method = 'ML')      


m_3_s <- gam((logchla) ~ s(DissAmmonia, bs="ts") +
               s(TotPhos, bs="ts") +
               s(DissNitrateNitrite, bs ="ts") +
               s(Temperature, bs="ts") +
               s(Secchi, bs="ts")+
               s(Conductivity, bs="ts") +
               s(SAC) +
               s(EXPORT) +
               # cannot smooth categorical variables
               season +
               Regions +
               (OUT*season) +
               (Regions*season) +
               s(Clam_biomass, bs="ts") +
               s(lagIndex, bs="ts") +
               s(lagOUT, bs="ts") +
               s(lagDissAmmonia ,bs="ts") +
               s(lagTotPhos, bs="ts"),
             data = df,
             method = 'ML')  


AIC(m_SACEXPORT_s, m_2_s, m_3_s, m_SACEXPORT_s_no_re)

#Check for concurvity/ variance inflation factors
concurvity(m_SACEXPORT_s)
concurvity(m_SACEXPORT_s, full=FALSE)

# smooth terms with high concurvity (>0.6):
# s(DissNitrateNitrite) =0.8
        # DissAmmonia =0.31
# s(Temperature) = 0.98
        # DissAmmonia= 0.4
        # Month = 0.95
# s(Secchi) = 0.84
        #
# s(Conductivity) = 0.91
        # lagOUT = 0.34
# s(SAC) = 0.97
        # EXPORT = 0.45
# s(lagOUT) = 0.86
        # Conductivity = 0.34
        # Month = 0.45

# s(lagNitrateNitrite) = 0.85
       # lag(DissAmmonia) = 0.37
# s(Month)

m_1_l<- lm(formula = logchla ~  Regions + season + (Regions*season) + (OUT*season), data=df)
vif(m_1_l, full=FALSE)
plot(m_1_s2, pages=1, scale=F, shade=T)


#################################################
# test removing variables with high concurvity  #
#################################################

# removing temperature
m_1 <- gam((logchla) ~ s(DissAmmonia, bs="ts") +
                       s(TotPhos, bs="ts") +
                       s(DissNitrateNitrite, bs ="ts") +
                       s(Secchi, bs="ts")+
                       s(Conductivity, bs="ts") +
                       s(SAC) +
                       s(EXPORT) +
                       season +
                       Regions +
                       (OUT*season) +
                       (Regions*season) +
                       s(Clam_biomass, bs="ts") +
                       s(lagIndex, bs="ts") +
                       s(lagOUT, bs="ts") +
                       s(lagDissAmmonia ,bs="ts") +
                       s(lagTotPhos, bs="ts") +
                       s(lagDissNitrateNitrite, bs="ts") +
                       s(Month, bs="re"),
                     data = df,
                     method = 'ML')
# removing DissNitrateNitrite
m_2<- gam((logchla) ~ s(DissAmmonia, bs="ts") +
                       s(TotPhos, bs="ts") +
                       s(DissNitrateNitrite, bs ="ts") +
                       s(Secchi, bs="ts")+
                       s(Conductivity, bs="ts") +
                       s(SAC) +
                       s(EXPORT) +
                       season +
                       Regions +
                       (OUT*season) +
                       (Regions*season) +
                       s(Clam_biomass, bs="ts") +
                       s(lagIndex, bs="ts") +
                       s(lagOUT, bs="ts") +
                       s(lagDissAmmonia ,bs="ts") +
                       s(lagTotPhos, bs="ts") +
                       s(Month, bs="re"),
                     data = df,
                     method = 'ML')
# removing lagOUT
m_3<- gam((logchla) ~ s(DissAmmonia, bs="ts") +
            s(TotPhos, bs="ts") +
            s(DissNitrateNitrite, bs ="ts") +
            s(Secchi, bs="ts")+
            s(Conductivity, bs="ts") +
            s(SAC) +
            s(EXPORT) +
            season +
            Regions +
            (OUT*season) +
            (Regions*season) +
            s(Clam_biomass, bs="ts") +
            s(lagIndex, bs="ts") +
            s(lagDissAmmonia ,bs="ts") +
            s(lagTotPhos, bs="ts") +
            s(Month, bs="re"),
          data = df,
          method = 'ML')

# removing Conductivity
m_4<- gam((logchla) ~ s(DissAmmonia, bs="ts") +
            s(TotPhos, bs="ts") +
            s(DissNitrateNitrite, bs ="ts") +
            s(Secchi, bs="ts") +
            s(SAC) +
            s(EXPORT) +
            season +
            Regions +
            (OUT*season) +
            (Regions*season) +
            s(Clam_biomass, bs="ts") +
            s(lagIndex, bs="ts") +
            s(lagDissAmmonia ,bs="ts") +
            s(lagTotPhos, bs="ts") +
            s(Month, bs="re"),
          data = df,
          method = 'ML')

concurvity(m_4)

# removing Month
m_5<- gam((logchla) ~ s(DissAmmonia, bs="ts") +
            s(TotPhos, bs="ts") +
            s(DissNitrateNitrite, bs ="ts") +
            s(Secchi, bs="ts") +
            s(SAC) +
            s(EXPORT) +
            season +
            Regions +
            (OUT*season) +
            (Regions*season) +
            s(Clam_biomass, bs="ts") +
            s(lagIndex, bs="ts") +
            s(lagDissAmmonia ,bs="ts") +
            s(lagTotPhos, bs="ts"),
          data = df,
          method = 'ML')

concurvity(m_5)

# removing SAC
m_6<- gam((logchla) ~ s(DissAmmonia, bs="ts") +
            s(TotPhos, bs="ts") +
            s(DissNitrateNitrite, bs ="ts") +
            s(Secchi, bs="ts") +
            s(EXPORT) +
            season +
            Regions +
            (OUT*season) +
            (Regions*season) +
            s(Clam_biomass, bs="ts") +
            s(lagIndex, bs="ts") +
            s(lagDissAmmonia ,bs="ts") +
            s(lagTotPhos, bs="ts"),
          data = df,
          method = 'ML')

concurvity(m_6)

gam.check(m_6)

# removing only monthly random effect
m_7 <- gam((logchla) ~ s(DissAmmonia, bs="ts") +
             s(TotPhos, bs="ts") +
             s(DissNitrateNitrite, bs ="ts") +
             s(Secchi, bs="ts")+
             s(Conductivity, bs="ts") +
             s(Temperature, bs="ts") +
             s(SAC) +
             s(EXPORT) +
             season +
             Regions +
             (OUT*season) +
             (Regions*season) +
             s(Clam_biomass, bs="ts") +
             s(lagIndex, bs="ts") +
             s(lagOUT, bs="ts") +
             s(lagDissAmmonia ,bs="ts") +
             s(lagTotPhos, bs="ts") +
             s(lagDissNitrateNitrite, bs="ts"),
           data = df,
           method = 'ML')

concurvity(m_7)

#m_1 without lagtotphos because of ~0 edf
m_8 <- gam((logchla)~ s(DissAmmonia, bs="ts") +
    s(TotPhos, bs="ts") +
    s(DissNitrateNitrite, bs ="ts") +
    s(Secchi, bs="ts")+
    s(Conductivity, bs="ts") +
    s(SAC) +
    s(EXPORT) +
    season +
    Regions +
    (OUT*season) +
    (Regions*season) +
    s(Clam_biomass, bs="ts") +
    s(lagIndex, bs="ts") +
    s(lagOUT, bs="ts") +
    s(lagDissAmmonia ,bs="ts") +
    s(lagDissNitrateNitrite, bs="ts") +
    s(Month, bs="re"),
  data = df,
  method = 'ML')


AIC(m_1,m_2,m_3,m_4,m_5,m_6,m_7, m_8,m_SACEXPORT_s)



# Plot data with model fit
c <- c("DissAmmonia","TotPhos",
          "DissNitrateNitrite",
          "Temperature",
          "Secchi",
          "Conductivity",
          "SAC",
          "EXPORT",
          "season",
          "Regions", 
          "Clam_biomass",
          "lagSAC",
          "lagDissAmmonia",
          "lagTotPhos",
          "lagDissNitrateNitrite")

lapply(c, function(x) {
  plot_predictions(m_SACEXPORT_s, x , points = .5) +
    theme_classic(base_size = 12)
})

gam.check(m_SACEXPORT_s)

gam.check(m_1)

c <- c("DissAmmonia","TotPhos",
       "DissNitrateNitrite",
       "Secchi",
       "Conductivity",
       "SAC",
       "EXPORT",
       "season",
       "Regions", 
       "Clam_biomass",
       "lagSAC",
       "lagDissAmmonia",
       "lagTotPhos",
       "lagDissNitrateNitrite")

lapply(c, function(x) {
  plot_predictions(m_1, x , points = .5) +
    theme_classic(base_size = 12)
})

##########
# NOTES: #
##########
# Questions:
# 1.) Should the determination of which flow variables to keep be done at the same time as 
# determining the other variables to keep? 
# 
# TO-DO:

#DONE:

# look into basis functions
# try different flow variables & lag terms
#look at histogram of th chla data - might need to be transformed to achieve normal dist.
#log, sqrt
  # make a histogram of the chla values and log and sqrt transformation
  #send to laura, see if one of them looks better than the other.
  #probably have to look at biplots again if we choose the log transformation
#start with linear model
# create bioplots to look at relationship between region and season and chlorophyll (log & untransformed)
# (1|Month) random term for month
# look at the other predictor variables


#####################
# Data exploration  #
#####################
#outputs can be found in presentations folder, logchla exploration
# Determine distribution of Chla-a


hist(df$Chlorophyll)
sqrt_chla <- sqrt(df$Chlorophyll)
hist(sqrt_chla)
df$log_chla <- log(df$Chlorophyll)
hist(df$logchla)
log10_chla <- log10(df$Chlorophyll)
hist(log10_chla)

# log seems to have the best fit.

# biplots

#scatter
lapply(c, function(x) {
  ggplot(df, mapping =aes(x = .data[[x]], y = logchla, color= Regions)) +
    ylab("log(Chlorphyll a)")+
    geom_point()+
    facet_wrap(~season, ncol=1)+
    theme_classic()})

# linear regression
lapply(c, function(x) {
  ggplot(df, mapping =aes(x = .data[[x]], y = logchla, color= Regions)) +
    ylab("log(Chlorphyll a)")+
    geom_smooth(method="lm", se=FALSE)+
    facet_wrap(~season, ncol=1)+
    theme_classic()})


ggplot(df, mapping =aes(x = Date, y=log_chla, color=Regions))+
  ylab("log(Chlorphyll a)")+
  geom_line()+
  scale_x_date(date_breaks= "10 years")+
  facet_wrap(~season, ncol=1)+
  theme_classic()

ggplot(df, mapping =aes(x = Date, y=log_chla))+
  ylab("log(Chlorphyll a)")+
  geom_line()+
  scale_x_date(date_breaks= "10 years")+
  facet_wrap(~season, ncol=1)+
  theme_classic()

# linear regression
lapply(c, function(x) {
  ggplot(df, mapping =aes(x = .data[[x]], y = logchla, color=season)) +
    ylab("log(Chlorphyll a)")+
    geom_smooth(method="lm", se=FALSE)+
    geom_point()+
    theme_classic()})

#Corr plot
df_numeric = df %>% select(where(is.numeric))
m = cor(df_numeric)

corrplot(m,
         method = "color",         
         type = "upper",           
         addCoef.col = "black",     
         number.cex = 0.7,          
         tl.cex = 0.8,              
         tl.col = "black",          
         col = colorRampPalette(c("blue", "white", "red"))(200)
)

#######################################
# Min Max normalized data exploration #
#######################################


df_minmax <- df %>% mutate(across(!c(Regions,season), ~as.vector(scale(.,center=min(.), scale = max(.)-min(.)))))


# linear regression
lapply(c, function(x) {
  ggplot(df_minmax, mapping =aes(x = .data[[x]], y = logchla, color=season)) +
    ylab("log(Chlorphyll a)")+
    geom_smooth(method="lm", se=FALSE)+
    geom_point()+
    theme_classic()})

#######################################
# z-score standardized data exploration #
#######################################


df_std <- df %>% mutate(across(!c(Regions,season), ~as.vector(scale(.))))


# linear regression
lapply(c, function(x) {
  ggplot(df_std, mapping =aes(x = .data[[x]], y = logchla, color=season)) +
    ylab("log(Chlorphyll a)")+
    geom_smooth(method="lm", se=FALSE)+
    geom_point()+
    theme_classic()})
lapply(c, function(x) {
  ggplot(df_std, mapping =aes(x = .data[[x]], y = logchla)) +
    ylab("log(Chlorphyll a)")+
    geom_smooth(method="lm", se=TRUE)+
    geom_point()+
    theme_classic()})


