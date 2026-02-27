# CM bloom thresholds - Discrete Water Quality Dataset
# Ryan Hankins adapating Schuyler Nardelli's code
# July 2025

# Set up environment ------------------------------------------------------

# Clear environment, close any open plots
rm(list = ls()); graphics.off()

# Load in libraries
library(tidyverse)
library(ggpubr)
library(mgcv)
library(spacetools) #use function GGdist to calculate distance from Golden Gate Bridge
library(deltamapr)
lapply(c("deltamapr", "tidyverse", "lubridate", "sf"), require, character.only = TRUE)
library(car)
library(emmeans)
library(multcomp)
library(dplyr)
library(tidyr)
library(ggrepel)
library(ggspatial)
library(sf)


# Import integrated dataset

CM <- read.csv("Data/edi_df_integrate_daily.csv")
#CM<- rename(CM, Date=year_month)
CM<- rename(CM,Chl=Chlorophyll)

CM <- CM %>%
  dplyr::select(Date, Chl, Latitude, Longitude) %>%
  filter(!is.na(Latitude) & !is.na(Longitude))
  
CM$Date <-as.Date(CM$Date, format = "%Y-%m-%d")

#read in region data
regions<-st_read("Regions_shp/Rosies_regions_edited.shp")
regions <- st_transform(regions, 26910)
st_crs(regions)

#add the subregions to the edi dataset
#convert stations file to simple features so we can map index number to subregions
CM = CM %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  #convert to UTMs so it's in the same coordinate reference system as the Delta shapefile
  st_transform(crs = 26910)
st_crs(CM)

# Make an EDI dataset with the delta regions
CM <- st_join(CM, regions["Regions"], join = st_intersects, left= TRUE) %>%  
  filter(!is.na(Regions)) %>%
  st_drop_geometry()%>%
  filter(!is.na(Chl))

# Threshold >5 ug/L (not used)  --------------------------------------------

# 3 consecutive values greater than 5 ug/L
greater_than_5 <- function(x) {
  r <- rle(x > 5)
  consecutive_greater_than_5 <- r$lengths >= 3 & r$values
  return(inverse.rle(list(lengths = r$lengths, values = consecutive_greater_than_5)))
}


# Suisun Bay
test <- CM %>% filter(Regions=="Suisun Bay") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
result <- greater_than_5(test$Chl)
I <- which(result=="TRUE")
p2<-ggplot()+geom_path(data=test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=test[I,],aes(x=Date,y=Chl),color="blue")+ggtitle("Suisun Bay")

# Confluence
test <- CM %>% filter(Regions=="Confluence") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
result <- greater_than_5(test$Chl)
I <- which(result=="TRUE")
p3<-ggplot()+geom_path(data=test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=test[I,],aes(x=Date,y=Chl),color="blue")+ggtitle("Confluence")

# North Delta
test <- CM %>% filter(Regions=="North Delta") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
result <- greater_than_5(test$Chl)
I <- which(result=="TRUE")
p4<-ggplot()+geom_path(data=test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=test[I,],aes(x=Date,y=Chl),color="blue")+ggtitle("North Delta")

# Central Delta
test <- CM %>% filter(Regions=="Central") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
result <- greater_than_5(test$Chl)
I <- which(result=="TRUE")
p5<-ggplot()+geom_path(data=test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=test[I,],aes(x=Date,y=Chl),color="blue")+ggtitle("Central Delta")

# South Delta
test <- CM %>% filter(Regions=="South") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
result <- greater_than_5(test$Chl)
I <- which(result=="TRUE")
p6<-ggplot()+geom_path(data=test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=test[I,],aes(x=Date,y=Chl),color="blue")+ggtitle("South Delta")

tiff("Figures/Bloom detection discrete/Threshold_5ugL.tiff", units="in", width=16, height=9, res=300)
ggarrange(p2,p3,p4,p5,p6,ncol=2,nrow=3)
dev.off()


# Threshold > 10 ug/L -----------------------------------------------------

# 3 consecutive values greater than 10 ug/L
greater_than_10 <- function(x) {
  r <- rle(x > 10)
  consecutive_greater_than_10 <- r$lengths >= 3 & r$values
  return(inverse.rle(list(lengths = r$lengths, values = consecutive_greater_than_10)))
}


# Suisun Bay
test <- CM %>% filter(Regions=="Suisun Bay") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
result <- greater_than_10(test$Chl)
I <- which(result=="TRUE")
p2<-ggplot()+geom_path(data=test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=test[I,],aes(x=Date,y=Chl),color="blue")+ggtitle("Suisun Bay")

# Confluence
test <- CM %>% filter(Regions=="Confluence") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
result <- greater_than_10(test$Chl)
I <- which(result=="TRUE")
p3<-ggplot()+geom_path(data=test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=test[I,],aes(x=Date,y=Chl),color="blue")+ggtitle("Confluence")

# North Delta
test <- CM %>% filter(Regions=="North Delta") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
result <- greater_than_10(test$Chl)
I <- which(result=="TRUE")
p4<-ggplot()+geom_path(data=test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=test[I,],aes(x=Date,y=Chl),color="blue")+ggtitle("North Delta")

# Central Delta
test <- CM %>% filter(Regions=="Central") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
result <- greater_than_10(test$Chl)
I <- which(result=="TRUE")
p5<-ggplot()+geom_path(data=test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=test[I,],aes(x=Date,y=Chl),color="blue")+ggtitle("Central Delta")

# South Delta
test <- CM %>% filter(Regions=="South") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
result <- greater_than_10(test$Chl)
I <- which(result=="TRUE")
p6<-ggplot()+geom_path(data=test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=test[I,],aes(x=Date,y=Chl),color="blue")+ggtitle("South Delta")

tiff("Figures/Bloom detection discrete/Threshold_10ugL.tiff", units="in", width=16, height=9, res=300)
ggarrange(p2,p3,p4,p5,p6,ncol=2,nrow=3)
dev.off()


# Threshold - quantile ----------------------------------------------------

# Change depending on which quantile you'd like to examine
x = 0.75


# Suisun Bay
test <- CM %>% filter(Regions=="Suisun Bay") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > (quantile(test$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
p8<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Suisun Bay")

# Confluence
test <- CM %>% filter(Regions=="Confluence") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > (quantile(test$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
p9<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Confluence")

# North Delta
test <- CM %>% filter(Regions=="North Delta") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > (quantile(test$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
p10<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("North Delta")

# Central Delta
test <- CM %>% filter(Regions=="Central") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > (quantile(test$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
p11<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Central Delta")

# South Delta
test <- CM %>% filter(Regions=="South") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > (quantile(test$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
p12<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("South Delta")

tiff("Figures/Bloom detection discrete/Quantile_75.tiff", units="in", width=16, height=9, res=300)
ggarrange(p8,p9,p10,p11,p12,ncol=2,nrow=3,common.legend=TRUE)
dev.off()


# Threshold - percent above median -------------------------------------------

# Change depending on what percent you'd like to examine
x = 1.75

# Suisun Bay
test <- CM %>% filter(Regions=="Suisun Bay") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > (median(test$Chl,na.rm=TRUE)*x), "Yes", "No")
p14<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Suisun Bay")

# Confluence
test <- CM %>% filter(Regions=="Confluence") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > (median(test$Chl,na.rm=TRUE)*x), "Yes", "No")
p15<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Confluence")

# North Delta
test <- CM %>% filter(Regions=="North Delta") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > (median(test$Chl,na.rm=TRUE)*x), "Yes", "No")
p16<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("North Delta")

# Central Delta
test <- CM %>% filter(Regions=="Central") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > (median(test$Chl,na.rm=TRUE)*x), "Yes", "No")
p17<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Central Delta")

# South Delta
test <- CM %>% filter(Regions=="South") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > (median(test$Chl,na.rm=TRUE)*x), "Yes", "No")
p18<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("South Delta")

tiff("Figures/Bloom detection discrete/Perc_Median_90.tiff", units="in", width=16, height=9, res=300)
ggarrange(p14,p15,p16,p17,p18,ncol=2,nrow=3,common.legend=TRUE)
dev.off()


# Percent above annual median  -------------------------------------------------

# Change depending on which percent you'd like to examine
perc = 1.75



# Suisun Bay
test <- CM %>% filter(Regions=="Suisun Bay") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))

x15 <- test %>% filter(year(Date) == "2015")
x16 <- test %>% filter(year(Date) == "2016")
x17 <- test %>% filter(year(Date) == "2017")
x18 <- test %>% filter(year(Date) == "2018")
x19 <- test %>% filter(year(Date) == "2019")
x20 <- test %>% filter(year(Date) == "2020")
x21 <- test %>% filter(year(Date) == "2021")
x22 <- test %>% filter(year(Date) == "2022")
x23 <- test %>% filter(year(Date) == "2023")
x24 <- test %>% filter(year(Date) == "2024")
x25 <- test %>% filter(year(Date) == "2025")

m15 <- median(x15$Chl,na.rm=TRUE)
m16 <- median(x16$Chl,na.rm=TRUE)
m17 <- median(x17$Chl,na.rm=TRUE)
m18 <- median(x18$Chl,na.rm=TRUE)
m19 <- median(x19$Chl,na.rm=TRUE)
m20 <- median(x20$Chl,na.rm=TRUE)
m21 <- median(x21$Chl,na.rm=TRUE)
m22 <- median(x22$Chl,na.rm=TRUE)
m23 <- median(x23$Chl,na.rm=TRUE)
m24 <- median(x24$Chl,na.rm=TRUE)
m25 <- median(x25$Chl,na.rm=TRUE)

h <- which(x15$Chl > (perc*m15))
i <- which(x16$Chl > (perc*m16))
j <- which(x17$Chl > (perc*m17))
k <- which(x18$Chl > (perc*m18))
l <- which(x19$Chl > (perc*m19))
m <- which(x20$Chl > (perc*m20))
n <- which(x21$Chl > (perc*m21))
o <- which(x22$Chl > (perc*m22))
p <- which(x23$Chl > (perc*m23))
q <- which(x24$Chl > (perc*m24))
r <- which(x25$Chl > (perc*m25))

p20<- ggplot()+
  geom_path(data=x16,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x16[i,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x17,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x17[j,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x18,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x18[k,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x19,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x19[l,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x20,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x20[m,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x21,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x21[n,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x22,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x22[o,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x23,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x23[p,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x24,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x24[q,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x25,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x25[r,],aes(x=Date,y=Chl, group=1),color="blue")+ ggtitle("Suisun Bay")

# Confluence
test <- CM %>% filter(Regions=="Confluence") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))

x08 <- test %>% filter(year(Date) == "2008")
x09 <- test %>% filter(year(Date) == "2009")
x10 <- test %>% filter(year(Date) == "2010")
x11 <- test %>% filter(year(Date) == "2011")
x12 <- test %>% filter(year(Date) == "2012")
x13 <- test %>% filter(year(Date) == "2013")
x14 <- test %>% filter(year(Date) == "2014")
x15 <- test %>% filter(year(Date) == "2015")
x16 <- test %>% filter(year(Date) == "2016")
x17 <- test %>% filter(year(Date) == "2017")
x18 <- test %>% filter(year(Date) == "2018")
x19 <- test %>% filter(year(Date) == "2019")
x20 <- test %>% filter(year(Date) == "2020")
x21 <- test %>% filter(year(Date) == "2021")
x22 <- test %>% filter(year(Date) == "2022")
x23 <- test %>% filter(year(Date) == "2023")
x24 <- test %>% filter(year(Date) == "2024")
x25 <- test %>% filter(year(Date) == "2025")

m08 <- median(x08$Chl,na.rm=TRUE)
m09 <- median(x09$Chl,na.rm=TRUE)
m10 <- median(x10$Chl,na.rm=TRUE)
m11 <- median(x11$Chl,na.rm=TRUE)
m12 <- median(x12$Chl,na.rm=TRUE)
m13 <- median(x13$Chl,na.rm=TRUE)
m14 <- median(x14$Chl,na.rm=TRUE)
m15 <- median(x15$Chl,na.rm=TRUE)
m16 <- median(x16$Chl,na.rm=TRUE)
m17 <- median(x17$Chl,na.rm=TRUE)
m18 <- median(x18$Chl,na.rm=TRUE)
m19 <- median(x19$Chl,na.rm=TRUE)
m20 <- median(x20$Chl,na.rm=TRUE)
m21 <- median(x21$Chl,na.rm=TRUE)
m22 <- median(x22$Chl,na.rm=TRUE)
m23 <- median(x23$Chl,na.rm=TRUE)
m24 <- median(x24$Chl,na.rm=TRUE)
m25 <- median(x25$Chl,na.rm=TRUE)

a <- which(x08$Chl > (perc*m08))
b <- which(x09$Chl > (perc*m09))
c <- which(x10$Chl > (perc*m10))
d <- which(x11$Chl > (perc*m11))
e <- which(x12$Chl > (perc*m12))
f <- which(x13$Chl > (perc*m13))
g <- which(x14$Chl > (perc*m14))
h <- which(x15$Chl > (perc*m15))
i <- which(x16$Chl > (perc*m16))
j <- which(x17$Chl > (perc*m17))
k <- which(x18$Chl > (perc*m18))
l <- which(x19$Chl > (perc*m19))
m <- which(x20$Chl > (perc*m20))
n <- which(x21$Chl > (perc*m21))
o <- which(x22$Chl > (perc*m22))
p <- which(x23$Chl > (perc*m23))
q <- which(x24$Chl > (perc*m24))
r <- which(x25$Chl > (perc*m25))

p21<- ggplot()+geom_path(data=x08,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x08[a,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x09,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x09[b,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x10,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x10[c,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x11,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x11[d,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x12,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x12[e,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x13,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x13[f,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x14,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x14[g,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x15,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x15[h,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x16,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x16[i,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x17,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x17[j,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x18,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x18[k,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x19,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x19[l,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x20,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x20[m,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x21,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x21[n,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x22,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x22[o,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x23,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x23[p,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x24,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x24[q,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x25,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x25[r,],aes(x=Date,y=Chl, group=1),color="blue")+ggtitle("Confluence")

# North Delta
test <- CM %>% filter(Regions=="North Delta") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))

x08 <- test %>% filter(year(Date) == "2008")
x09 <- test %>% filter(year(Date) == "2009")
x10 <- test %>% filter(year(Date) == "2010")
x11 <- test %>% filter(year(Date) == "2011")
x12 <- test %>% filter(year(Date) == "2012")
x13 <- test %>% filter(year(Date) == "2013")
x14 <- test %>% filter(year(Date) == "2014")
x15 <- test %>% filter(year(Date) == "2015")
x16 <- test %>% filter(year(Date) == "2016")
x17 <- test %>% filter(year(Date) == "2017")
x18 <- test %>% filter(year(Date) == "2018")
x19 <- test %>% filter(year(Date) == "2019")
x20 <- test %>% filter(year(Date) == "2020")
x21 <- test %>% filter(year(Date) == "2021")
x22 <- test %>% filter(year(Date) == "2022")
x23 <- test %>% filter(year(Date) == "2023")
x24 <- test %>% filter(year(Date) == "2024")
x25 <- test %>% filter(year(Date) == "2025")

m08 <- median(x08$Chl,na.rm=TRUE)
m09 <- median(x09$Chl,na.rm=TRUE)
m10 <- median(x10$Chl,na.rm=TRUE)
m11 <- median(x11$Chl,na.rm=TRUE)
m12 <- median(x12$Chl,na.rm=TRUE)
m13 <- median(x13$Chl,na.rm=TRUE)
m14 <- median(x14$Chl,na.rm=TRUE)
m15 <- median(x15$Chl,na.rm=TRUE)
m16 <- median(x16$Chl,na.rm=TRUE)
m17 <- median(x17$Chl,na.rm=TRUE)
m18 <- median(x18$Chl,na.rm=TRUE)
m19 <- median(x19$Chl,na.rm=TRUE)
m20 <- median(x20$Chl,na.rm=TRUE)
m21 <- median(x21$Chl,na.rm=TRUE)
m22 <- median(x22$Chl,na.rm=TRUE)
m23 <- median(x23$Chl,na.rm=TRUE)
m24 <- median(x24$Chl,na.rm=TRUE)
m25 <- median(x25$Chl,na.rm=TRUE)

a <- which(x08$Chl > (perc*m08))
b <- which(x09$Chl > (perc*m09))
c <- which(x10$Chl > (perc*m10))
d <- which(x11$Chl > (perc*m11))
e <- which(x12$Chl > (perc*m12))
f <- which(x13$Chl > (perc*m13))
g <- which(x14$Chl > (perc*m14))
h <- which(x15$Chl > (perc*m15))
i <- which(x16$Chl > (perc*m16))
j <- which(x17$Chl > (perc*m17))
k <- which(x18$Chl > (perc*m18))
l <- which(x19$Chl > (perc*m19))
m <- which(x20$Chl > (perc*m20))
n <- which(x21$Chl > (perc*m21))
o <- which(x22$Chl > (perc*m22))
p <- which(x23$Chl > (perc*m23))
q <- which(x24$Chl > (perc*m24))
r <- which(x25$Chl > (perc*m25))

p22<- ggplot()+geom_path(data=x08,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x08[a,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x09,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x09[b,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x10,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x10[c,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x11,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x11[d,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x12,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x12[e,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x13,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x13[f,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x14,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x14[g,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x15,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x15[h,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x16,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x16[i,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x17,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x17[j,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x18,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x18[k,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x19,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x19[l,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x20,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x20[m,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x21,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x21[n,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x22,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x22[o,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x23,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x23[p,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x24,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x24[q,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x25,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x25[r,],aes(x=Date,y=Chl, group=1),color="blue")+ggtitle("North Delta")

# Central
test <- CM %>% filter(Regions=="Central") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))

x08 <- test %>% filter(year(Date) == "2008")
x09 <- test %>% filter(year(Date) == "2009")
x10 <- test %>% filter(year(Date) == "2010")
x11 <- test %>% filter(year(Date) == "2011")
x12 <- test %>% filter(year(Date) == "2012")
x13 <- test %>% filter(year(Date) == "2013")
x14 <- test %>% filter(year(Date) == "2014")
x15 <- test %>% filter(year(Date) == "2015")
x16 <- test %>% filter(year(Date) == "2016")
x17 <- test %>% filter(year(Date) == "2017")
x18 <- test %>% filter(year(Date) == "2018")
x19 <- test %>% filter(year(Date) == "2019")
x20 <- test %>% filter(year(Date) == "2020")
x21 <- test %>% filter(year(Date) == "2021")
x22 <- test %>% filter(year(Date) == "2022")
x23 <- test %>% filter(year(Date) == "2023")
x24 <- test %>% filter(year(Date) == "2024")
x25 <- test %>% filter(year(Date) == "2025")

m08 <- median(x08$Chl,na.rm=TRUE)
m09 <- median(x09$Chl,na.rm=TRUE)
m10 <- median(x10$Chl,na.rm=TRUE)
m11 <- median(x11$Chl,na.rm=TRUE)
m12 <- median(x12$Chl,na.rm=TRUE)
m13 <- median(x13$Chl,na.rm=TRUE)
m14 <- median(x14$Chl,na.rm=TRUE)
m15 <- median(x15$Chl,na.rm=TRUE)
m16 <- median(x16$Chl,na.rm=TRUE)
m17 <- median(x17$Chl,na.rm=TRUE)
m18 <- median(x18$Chl,na.rm=TRUE)
m19 <- median(x19$Chl,na.rm=TRUE)
m20 <- median(x20$Chl,na.rm=TRUE)
m21 <- median(x21$Chl,na.rm=TRUE)
m22 <- median(x22$Chl,na.rm=TRUE)
m23 <- median(x23$Chl,na.rm=TRUE)
m24 <- median(x24$Chl,na.rm=TRUE)
m25 <- median(x25$Chl,na.rm=TRUE)

a <- which(x08$Chl > (perc*m08))
b <- which(x09$Chl > (perc*m09))
c <- which(x10$Chl > (perc*m10))
d <- which(x11$Chl > (perc*m11))
e <- which(x12$Chl > (perc*m12))
f <- which(x13$Chl > (perc*m13))
g <- which(x14$Chl > (perc*m14))
h <- which(x15$Chl > (perc*m15))
i <- which(x16$Chl > (perc*m16))
j <- which(x17$Chl > (perc*m17))
k <- which(x18$Chl > (perc*m18))
l <- which(x19$Chl > (perc*m19))
m <- which(x20$Chl > (perc*m20))
n <- which(x21$Chl > (perc*m21))
o <- which(x22$Chl > (perc*m22))
p <- which(x23$Chl > (perc*m23))
q <- which(x24$Chl > (perc*m24))
r <- which(x25$Chl > (perc*m25))

p23<- ggplot()+geom_path(data=x08,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x08[a,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x09,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x09[b,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x10,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x10[c,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x11,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x11[d,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x12,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x12[e,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x13,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x13[f,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x14,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x14[g,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x15,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x15[h,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x16,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x16[i,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x17,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x17[j,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x18,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x18[k,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x19,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x19[l,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x20,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x20[m,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x21,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x21[n,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x22,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x22[o,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x23,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x23[p,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x24,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x24[q,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x25,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x25[r,],aes(x=Date,y=Chl, group=1),color="blue") + ggtitle("Central Delta")

# South
test <- CM %>% filter(Regions=="South") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))

x08 <- test %>% filter(year(Date) == "2008")
x09 <- test %>% filter(year(Date) == "2009")
x10 <- test %>% filter(year(Date) == "2010")
x11 <- test %>% filter(year(Date) == "2011")
x12 <- test %>% filter(year(Date) == "2012")
x13 <- test %>% filter(year(Date) == "2013")
x14 <- test %>% filter(year(Date) == "2014")
x15 <- test %>% filter(year(Date) == "2015")
x16 <- test %>% filter(year(Date) == "2016")
x17 <- test %>% filter(year(Date) == "2017")
x18 <- test %>% filter(year(Date) == "2018")
x19 <- test %>% filter(year(Date) == "2019")
x20 <- test %>% filter(year(Date) == "2020")
x21 <- test %>% filter(year(Date) == "2021")
x22 <- test %>% filter(year(Date) == "2022")
x23 <- test %>% filter(year(Date) == "2023")
x24 <- test %>% filter(year(Date) == "2024")
x25 <- test %>% filter(year(Date) == "2025")

m08 <- median(x08$Chl,na.rm=TRUE)
m09 <- median(x09$Chl,na.rm=TRUE)
m10 <- median(x10$Chl,na.rm=TRUE)
m11 <- median(x11$Chl,na.rm=TRUE)
m12 <- median(x12$Chl,na.rm=TRUE)
m13 <- median(x13$Chl,na.rm=TRUE)
m14 <- median(x14$Chl,na.rm=TRUE)
m15 <- median(x15$Chl,na.rm=TRUE)
m16 <- median(x16$Chl,na.rm=TRUE)
m17 <- median(x17$Chl,na.rm=TRUE)
m18 <- median(x18$Chl,na.rm=TRUE)
m19 <- median(x19$Chl,na.rm=TRUE)
m20 <- median(x20$Chl,na.rm=TRUE)
m21 <- median(x21$Chl,na.rm=TRUE)
m22 <- median(x22$Chl,na.rm=TRUE)
m23 <- median(x23$Chl,na.rm=TRUE)
m24 <- median(x24$Chl,na.rm=TRUE)
m25 <- median(x25$Chl,na.rm=TRUE)

a <- which(x08$Chl > (perc*m08))
b <- which(x09$Chl > (perc*m09))
c <- which(x10$Chl > (perc*m10))
d <- which(x11$Chl > (perc*m11))
e <- which(x12$Chl > (perc*m12))
f <- which(x13$Chl > (perc*m13))
g <- which(x14$Chl > (perc*m14))
h <- which(x15$Chl > (perc*m15))
i <- which(x16$Chl > (perc*m16))
j <- which(x17$Chl > (perc*m17))
k <- which(x18$Chl > (perc*m18))
l <- which(x19$Chl > (perc*m19))
m <- which(x20$Chl > (perc*m20))
n <- which(x21$Chl > (perc*m21))
o <- which(x22$Chl > (perc*m22))
p <- which(x23$Chl > (perc*m23))
q <- which(x24$Chl > (perc*m24))
r <- which(x25$Chl > (perc*m25))

p24<- ggplot()+geom_path(data=x08,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x08[a,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x09,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x09[b,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x10,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x10[c,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x11,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x11[d,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x12,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x12[e,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x13,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x13[f,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x14,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x14[g,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x15,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x15[h,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x16,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x16[i,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x17,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x17[j,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x18,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x18[k,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x19,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x19[l,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x20,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x20[m,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x21,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x21[n,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x22,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x22[o,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x23,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x23[p,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x24,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x24[q,],aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x25,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x25[r,],aes(x=Date,y=Chl, group=1),color="blue")+ggtitle("South Delta")

tiff("Figures/Bloom detection discrete/PerYear_90PercentMedian.tiff", units="in", width=16, height=9, res=300)
ggarrange(p20,p21,p22,p23,p24,ncol=2,nrow=3,common.legend=TRUE)
dev.off()


# Quantile per year -------------------------------------------------------

# Change depending on which quantile you'd like to examine
x=0.75


# Suisun Bay
test <- CM %>% filter(Regions=="Suisun Bay") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))

x15 <- test %>% filter(year(Date) == "2015")
x16 <- test %>% filter(year(Date) == "2016")
x17 <- test %>% filter(year(Date) == "2017")
x18 <- test %>% filter(year(Date) == "2018")
x19 <- test %>% filter(year(Date) == "2019")
x20 <- test %>% filter(year(Date) == "2020")
x21 <- test %>% filter(year(Date) == "2021")
x22 <- test %>% filter(year(Date) == "2022")
x23 <- test %>% filter(year(Date) == "2023")
x24 <- test %>% filter(year(Date) == "2024")
x25 <- test %>% filter(year(Date) == "2025")

x15$Bloom <- ifelse(x15$Chl > (quantile(x15$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x16$Bloom <- ifelse(x16$Chl > (quantile(x16$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x17$Bloom <- ifelse(x17$Chl > (quantile(x17$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x18$Bloom <- ifelse(x18$Chl > (quantile(x18$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x19$Bloom <- ifelse(x19$Chl > (quantile(x19$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x20$Bloom <- ifelse(x20$Chl > (quantile(x20$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x21$Bloom <- ifelse(x21$Chl > (quantile(x21$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x22$Bloom <- ifelse(x22$Chl > (quantile(x22$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x23$Bloom <- ifelse(x23$Chl > (quantile(x23$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x24$Bloom <- ifelse(x24$Chl > (quantile(x24$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x25$Bloom <- ifelse(x25$Chl > (quantile(x25$Chl, probs = x,na.rm=TRUE)), "Yes", "No")

p26<- ggplot()+
  geom_path(data=x15,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x15%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x16,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x16%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x17,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x17%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x18,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x18%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x19,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x19%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x20,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x20%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x21,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x21%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x22,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x22%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x23,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x23%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x24,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x24%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x25,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x25%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+ggtitle("Suisun Bay")

# Confluence
test <- CM %>% filter(Regions=="Confluence") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))

x08 <- test %>% filter(year(Date) == "2008")
x09 <- test %>% filter(year(Date) == "2009")
x10 <- test %>% filter(year(Date) == "2010")
x11 <- test %>% filter(year(Date) == "2011")
x12 <- test %>% filter(year(Date) == "2012")
x13 <- test %>% filter(year(Date) == "2013")
x14 <- test %>% filter(year(Date) == "2014")
x15 <- test %>% filter(year(Date) == "2015")
x16 <- test %>% filter(year(Date) == "2016")
x17 <- test %>% filter(year(Date) == "2017")
x18 <- test %>% filter(year(Date) == "2018")
x19 <- test %>% filter(year(Date) == "2019")
x20 <- test %>% filter(year(Date) == "2020")
x21 <- test %>% filter(year(Date) == "2021")
x22 <- test %>% filter(year(Date) == "2022")
x23 <- test %>% filter(year(Date) == "2023")
x24 <- test %>% filter(year(Date) == "2024")
x25 <- test %>% filter(year(Date) == "2025")

x08$Bloom <- ifelse(x08$Chl > (quantile(x08$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x09$Bloom <- ifelse(x09$Chl > (quantile(x09$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x10$Bloom <- ifelse(x10$Chl > (quantile(x10$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x11$Bloom <- ifelse(x11$Chl > (quantile(x11$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x12$Bloom <- ifelse(x12$Chl > (quantile(x12$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x13$Bloom <- ifelse(x13$Chl > (quantile(x13$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x14$Bloom <- ifelse(x14$Chl > (quantile(x14$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x15$Bloom <- ifelse(x15$Chl > (quantile(x15$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x16$Bloom <- ifelse(x16$Chl > (quantile(x16$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x17$Bloom <- ifelse(x17$Chl > (quantile(x17$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x18$Bloom <- ifelse(x18$Chl > (quantile(x18$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x19$Bloom <- ifelse(x19$Chl > (quantile(x19$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x20$Bloom <- ifelse(x20$Chl > (quantile(x20$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x21$Bloom <- ifelse(x21$Chl > (quantile(x21$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x22$Bloom <- ifelse(x22$Chl > (quantile(x22$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x23$Bloom <- ifelse(x23$Chl > (quantile(x23$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x24$Bloom <- ifelse(x24$Chl > (quantile(x24$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x25$Bloom <- ifelse(x25$Chl > (quantile(x25$Chl, probs = x,na.rm=TRUE)), "Yes", "No")

p27<- ggplot()+
  geom_path(data=x08 ,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x08 %>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x09,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x09%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x10,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x10%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x11,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x11%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x12,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x12%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x13,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x13%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x14,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x14%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x15,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x15%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x16,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x16%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x17,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x17%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x18,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x18%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x19,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x19%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x20,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x20%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x21,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x21%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x22,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x22%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x23,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x23%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x24,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x24%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x25,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x25%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+ggtitle("Confluence")


# North Delta
test <- CM %>% filter(Regions=="North Delta") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))

x08 <- test %>% filter(year(Date) == "2008")
x09 <- test %>% filter(year(Date) == "2009")
x10 <- test %>% filter(year(Date) == "2010")
x11 <- test %>% filter(year(Date) == "2011")
x12 <- test %>% filter(year(Date) == "2012")
x13 <- test %>% filter(year(Date) == "2013")
x14 <- test %>% filter(year(Date) == "2014")
x15 <- test %>% filter(year(Date) == "2015")
x16 <- test %>% filter(year(Date) == "2016")
x17 <- test %>% filter(year(Date) == "2017")
x18 <- test %>% filter(year(Date) == "2018")
x19 <- test %>% filter(year(Date) == "2019")
x20 <- test %>% filter(year(Date) == "2020")
x21 <- test %>% filter(year(Date) == "2021")
x22 <- test %>% filter(year(Date) == "2022")
x23 <- test %>% filter(year(Date) == "2023")
x24 <- test %>% filter(year(Date) == "2024")
x25 <- test %>% filter(year(Date) == "2025")

x08$Bloom <- ifelse(x08$Chl > (quantile(x08$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x09$Bloom <- ifelse(x09$Chl > (quantile(x09$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x10$Bloom <- ifelse(x10$Chl > (quantile(x10$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x11$Bloom <- ifelse(x11$Chl > (quantile(x11$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x12$Bloom <- ifelse(x12$Chl > (quantile(x12$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x13$Bloom <- ifelse(x13$Chl > (quantile(x13$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x14$Bloom <- ifelse(x14$Chl > (quantile(x14$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x15$Bloom <- ifelse(x15$Chl > (quantile(x15$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x16$Bloom <- ifelse(x16$Chl > (quantile(x16$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x17$Bloom <- ifelse(x17$Chl > (quantile(x17$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x18$Bloom <- ifelse(x18$Chl > (quantile(x18$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x19$Bloom <- ifelse(x19$Chl > (quantile(x19$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x20$Bloom <- ifelse(x20$Chl > (quantile(x20$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x21$Bloom <- ifelse(x21$Chl > (quantile(x21$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x22$Bloom <- ifelse(x22$Chl > (quantile(x22$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x23$Bloom <- ifelse(x23$Chl > (quantile(x23$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x24$Bloom <- ifelse(x24$Chl > (quantile(x24$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x25$Bloom <- ifelse(x25$Chl > (quantile(x25$Chl, probs = x,na.rm=TRUE)), "Yes", "No")

p28<- ggplot()+
  geom_path(data=x08 ,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x08 %>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x09,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x09%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x10,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x10%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x11,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x11%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x12,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x12%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x13,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x13%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x14,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x14%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x15,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x15%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x16,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x16%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x17,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x17%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x18,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x18%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x19,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x19%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x20,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x20%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x21,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x21%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x22,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x22%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x23,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x23%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x24,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x24%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x25,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x25%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+ggtitle("North Delta")

# Central
test <- CM %>% filter(Regions=="Central") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))

x08 <- test %>% filter(year(Date) == "2008")
x09 <- test %>% filter(year(Date) == "2009")
x10 <- test %>% filter(year(Date) == "2010")
x11 <- test %>% filter(year(Date) == "2011")
x12 <- test %>% filter(year(Date) == "2012")
x13 <- test %>% filter(year(Date) == "2013")
x14 <- test %>% filter(year(Date) == "2014")
x15 <- test %>% filter(year(Date) == "2015")
x16 <- test %>% filter(year(Date) == "2016")
x17 <- test %>% filter(year(Date) == "2017")
x18 <- test %>% filter(year(Date) == "2018")
x19 <- test %>% filter(year(Date) == "2019")
x20 <- test %>% filter(year(Date) == "2020")
x21 <- test %>% filter(year(Date) == "2021")
x22 <- test %>% filter(year(Date) == "2022")
x23 <- test %>% filter(year(Date) == "2023")
x24 <- test %>% filter(year(Date) == "2024")
x25 <- test %>% filter(year(Date) == "2025")

x08$Bloom <- ifelse(x08$Chl > (quantile(x08$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x09$Bloom <- ifelse(x09$Chl > (quantile(x09$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x10$Bloom <- ifelse(x10$Chl > (quantile(x10$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x11$Bloom <- ifelse(x11$Chl > (quantile(x11$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x12$Bloom <- ifelse(x12$Chl > (quantile(x12$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x13$Bloom <- ifelse(x13$Chl > (quantile(x13$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x14$Bloom <- ifelse(x14$Chl > (quantile(x14$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x15$Bloom <- ifelse(x15$Chl > (quantile(x15$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x16$Bloom <- ifelse(x16$Chl > (quantile(x16$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x17$Bloom <- ifelse(x17$Chl > (quantile(x17$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x18$Bloom <- ifelse(x18$Chl > (quantile(x18$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x19$Bloom <- ifelse(x19$Chl > (quantile(x19$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x20$Bloom <- ifelse(x20$Chl > (quantile(x20$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x21$Bloom <- ifelse(x21$Chl > (quantile(x21$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x22$Bloom <- ifelse(x22$Chl > (quantile(x22$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x23$Bloom <- ifelse(x23$Chl > (quantile(x23$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x24$Bloom <- ifelse(x24$Chl > (quantile(x24$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x25$Bloom <- ifelse(x25$Chl > (quantile(x25$Chl, probs = x,na.rm=TRUE)), "Yes", "No")

p29<- ggplot()+
  geom_path(data=x08 ,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x08 %>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x09,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x09%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x10,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x10%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x11,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x11%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x12,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x12%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x13,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x13%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x14,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x14%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x15,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x15%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x16,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x16%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x17,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x17%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x18,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x18%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x19,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x19%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x20,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x20%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x21,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x21%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x22,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x22%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x23,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x23%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x24,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x24%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x25,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x25%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+ggtitle("Central Delta")

# South
test <- CM %>% filter(Regions=="South") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))

x08 <- test %>% filter(year(Date) == "2008")
x09 <- test %>% filter(year(Date) == "2009")
x10 <- test %>% filter(year(Date) == "2010")
x11 <- test %>% filter(year(Date) == "2011")
x12 <- test %>% filter(year(Date) == "2012")
x13 <- test %>% filter(year(Date) == "2013")
x14 <- test %>% filter(year(Date) == "2014")
x15 <- test %>% filter(year(Date) == "2015")
x16 <- test %>% filter(year(Date) == "2016")
x17 <- test %>% filter(year(Date) == "2017")
x18 <- test %>% filter(year(Date) == "2018")
x19 <- test %>% filter(year(Date) == "2019")
x20 <- test %>% filter(year(Date) == "2020")
x21 <- test %>% filter(year(Date) == "2021")
x22 <- test %>% filter(year(Date) == "2022")
x23 <- test %>% filter(year(Date) == "2023")
x24 <- test %>% filter(year(Date) == "2024")
x25 <- test %>% filter(year(Date) == "2025")

x08$Bloom <- ifelse(x08$Chl > (quantile(x08$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x09$Bloom <- ifelse(x09$Chl > (quantile(x09$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x10$Bloom <- ifelse(x10$Chl > (quantile(x10$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x11$Bloom <- ifelse(x11$Chl > (quantile(x11$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x12$Bloom <- ifelse(x12$Chl > (quantile(x12$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x13$Bloom <- ifelse(x13$Chl > (quantile(x13$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x14$Bloom <- ifelse(x14$Chl > (quantile(x14$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x15$Bloom <- ifelse(x15$Chl > (quantile(x15$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x16$Bloom <- ifelse(x16$Chl > (quantile(x16$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x17$Bloom <- ifelse(x17$Chl > (quantile(x17$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x18$Bloom <- ifelse(x18$Chl > (quantile(x18$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x19$Bloom <- ifelse(x19$Chl > (quantile(x19$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x20$Bloom <- ifelse(x20$Chl > (quantile(x20$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x21$Bloom <- ifelse(x21$Chl > (quantile(x21$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x22$Bloom <- ifelse(x22$Chl > (quantile(x22$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x23$Bloom <- ifelse(x23$Chl > (quantile(x23$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x24$Bloom <- ifelse(x24$Chl > (quantile(x24$Chl, probs = x,na.rm=TRUE)), "Yes", "No")
x25$Bloom <- ifelse(x25$Chl > (quantile(x25$Chl, probs = x,na.rm=TRUE)), "Yes", "No")

p30<- ggplot()+
  geom_path(data=x08 ,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x08 %>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x09,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x09%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x10,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x10%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x11,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x11%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x12,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x12%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x13,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x13%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x14,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x14%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x15,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x15%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x16,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x16%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x17,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x17%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x18,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x18%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x19,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x19%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x20,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x20%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x21,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x21%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x22,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x22%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x23,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x23%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x24,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x24%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+
  geom_path(data=x25,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=x25%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+ggtitle("South Delta")

tiff("Figures/Bloom detection discrete/PerYear_75quantile.tiff", units="in", width=16, height=9, res=300)
ggarrange(p26,p27,p28,p29,p30,ncol=2,nrow=3,common.legend=TRUE)
dev.off()


# 99% PI on spline fit - 1 iteration ---------------------------------------



# Suisun Bay
test <- CM %>% filter(Regions=="Suisun Bay") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

p32 <- ggplot()+
  geom_path(data=test_pi,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=test_pi%>% filter(bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+ggtitle("Suisun Bay")
rm(test,b,dat,test_pi)


# Confluence
test <- CM %>% filter(Regions=="Confluence") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

p33 <- ggplot()+
  geom_path(data=test_pi,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=test_pi%>% filter(bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+ggtitle("Confluence")
rm(test,b,dat,test_pi)


# North Delta
test <- CM %>% filter(Regions=="North Delta") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

p34 <- ggplot()+
  geom_path(data=test_pi,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=test_pi%>% filter(bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+ggtitle("North Delta")
rm(test,b,dat,test_pi)


# Central Delta
test <- CM %>% filter(Regions=="Central") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

p35 <- ggplot()+
  geom_path(data=test_pi,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=test_pi%>% filter(bloom=="Yes"),aes(x=Date,y=Chl, group=1),color="blue")+ggtitle("Central Delta")
rm(test,b,dat,test_pi)


# South Delta
test <- CM %>% filter(Regions=="South") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

p36 <- ggplot()+
  geom_path(data=test_pi,aes(x=Date,y=Chl, group=1),color="black")+
  geom_point(data=test_pi%>% filter(bloom=="Yes"),aes(x=Date,y=Chl),color="blue")+ggtitle("South Delta")
rm(test,b,dat,test_pi)

tiff("Figures/Bloom detection discrete/Spline_99PI_1iter.tiff", units="in", width=16, height=9, res=300)
ggarrange(p32,p33,p34,p35,p36,ncol=2,nrow=3,common.legend=TRUE)
dev.off()

# 99% PI on spline fit - 1 iteration ---------------------------------------


# Suisun Bay
test <- CM %>% filter(Regions=="Suisun Bay") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

p32 <- ggplot()+
  geom_path(data=test_pi,aes(x=Date,y=Chl),color="black")+
  geom_point(data=test_pi%>% filter(bloom=="Yes"),aes(x=Date,y=Chl),color="blue")+ggtitle("Suisun Bay")
rm(test,b,dat,test_pi)


# Confluence
test <- CM %>% filter(Regions=="Confluence") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

p33 <- ggplot()+
  geom_path(data=test_pi,aes(x=Date,y=Chl),color="black")+
  geom_point(data=test_pi%>% filter(bloom=="Yes"),aes(x=Date,y=Chl),color="blue")+ggtitle("Confluence")
rm(test,b,dat,test_pi)


# North Delta
test <- CM %>% filter(Regions=="North Delta") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

p34 <- ggplot()+
  geom_path(data=test_pi,aes(x=Date,y=Chl),color="black")+
  geom_point(data=test_pi%>% filter(bloom=="Yes"),aes(x=Date,y=Chl),color="blue")+ggtitle("North Delta")
rm(test,b,dat,test_pi)


# Central Delta
test <- CM %>% filter(Regions=="Central") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

p35 <- ggplot()+
  geom_path(data=test_pi,aes(x=Date,y=Chl),color="black")+
  geom_point(data=test_pi%>% filter(bloom=="Yes"),aes(x=Date,y=Chl),color="blue")+ggtitle("Central Delta")
rm(test,b,dat,test_pi)


# South Delta
test <- CM %>% filter(Regions=="South") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

p36 <- ggplot()+
  geom_path(data=test_pi,aes(x=Date,y=Chl),color="black")+
  geom_point(data=test_pi%>% filter(bloom=="Yes"),aes(x=Date,y=Chl),color="blue")+ggtitle("South Delta")
rm(test,b,dat,test_pi)

tiff("Figures/Bloom detection discrete/Spline_99PI_1iter.tiff", units="in", width=16, height=9, res=300)
ggarrange(p32,p33,p34,p35,p36,ncol=2,nrow=3,common.legend=TRUE)
dev.off()



# 99% PI on spline fit - 2 iterations ---------------------------------------

# Suisun Bay
test <- CM %>% filter(Regions=="Suisun Bay") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

# Iteration 1
b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

# Iteration 2
d <- test_pi %>% filter(bloom=="No") %>% dplyr::select("Date","Chl","doy")
b <- gam(Chl~s(doy,bs="cp"),data= d)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi1 <- cbind(d, dat)
test_pi1$bloom <- ifelse(test_pi1$Chl > test_pi1$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi1, aes(x = doy, y = Chl,color=bloom))+
  geom_line(data = test_pi1, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi1, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

dat <- left_join(test_pi,test_pi1,by=c("Date","Chl","doy"))
dat <- dat %>% dplyr::select("Date","Chl","doy","bloom.y") %>% rename(Bloom=bloom.y)
dat$Bloom[is.na(dat$Bloom)] <- "Yes"

p32 <- ggplot()+
  geom_path(data=dat,aes(x=Date,y=Chl),color="black")+
  geom_point(data=dat%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl),color="blue")+ggtitle("Suisun Bay")
rm(test,b,dat,test_pi,test_pi1)


# Confluence
test <- CM %>% filter(Regions=="Confluence") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

# Iteration 1
b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

# Iteration 2
d <- test_pi %>% filter(bloom=="No") %>% dplyr::select("Date","Chl","doy")
b <- gam(Chl~s(doy,bs="cp"),data= d)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi1 <- cbind(d, dat)
test_pi1$bloom <- ifelse(test_pi1$Chl > test_pi1$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi1, aes(x = doy, y = Chl,color=bloom))+
  geom_line(data = test_pi1, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi1, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

dat <- left_join(test_pi,test_pi1,by=c("Date","Chl","doy"))
dat <- dat %>% dplyr::select("Date","Chl","doy","bloom.y") %>% rename(Bloom=bloom.y)
dat$Bloom[is.na(dat$Bloom)] <- "Yes"

p33 <- ggplot()+
  geom_path(data=dat,aes(x=Date,y=Chl),color="black")+
  geom_point(data=dat%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl),color="blue")+ggtitle("Confluence")
rm(test,b,dat,test_pi,test_pi1)


# North Delta
test <- CM %>% filter(Regions=="North Delta") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

# Iteration 1
b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

# Iteration 2
d <- test_pi %>% filter(bloom=="No") %>% dplyr::select("Date","Chl","doy")
b <- gam(Chl~s(doy,bs="cp"),data= d)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi1 <- cbind(d, dat)
test_pi1$bloom <- ifelse(test_pi1$Chl > test_pi1$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi1, aes(x = doy, y = Chl,color=bloom))+
  geom_line(data = test_pi1, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi1, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

dat <- left_join(test_pi,test_pi1,by=c("Date","Chl","doy"))
dat <- dat %>% dplyr::select("Date","Chl","doy","bloom.y") %>% rename(Bloom=bloom.y)
dat$Bloom[is.na(dat$Bloom)] <- "Yes"

p34 <- ggplot()+
  geom_path(data=dat,aes(x=Date,y=Chl),color="black")+
  geom_point(data=dat%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl),color="blue")+ggtitle("North Delta")
rm(test,b,dat,test_pi,test_pi1)


# Central Delta
test <- CM %>% filter(Regions=="Central") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

# Iteration 1
b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

# Iteration 2
d <- test_pi %>% filter(bloom=="No") %>% dplyr::select("Date","Chl","doy")
b <- gam(Chl~s(doy,bs="cp"),data= d)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi1 <- cbind(d, dat)
test_pi1$bloom <- ifelse(test_pi1$Chl > test_pi1$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi1, aes(x = doy, y = Chl,color=bloom))+
  geom_line(data = test_pi1, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi1, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

dat <- left_join(test_pi,test_pi1,by=c("Date","Chl","doy"))
dat <- dat %>% dplyr::select("Date","Chl","doy","bloom.y") %>% rename(Bloom=bloom.y)
dat$Bloom[is.na(dat$Bloom)] <- "Yes"

p35 <- ggplot()+
  geom_path(data=dat,aes(x=Date,y=Chl),color="black")+
  geom_point(data=dat%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl),color="blue")+ggtitle("Central Delta")
rm(test,b,dat,test_pi,test_pi1)


# South Delta
test <- CM %>% filter(Regions=="South") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

# Iteration 1
b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

# Iteration 2
d <- test_pi %>% filter(bloom=="No") %>% dplyr::select("Date","Chl","doy")
b <- gam(Chl~s(doy,bs="cp"),data= d)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi1 <- cbind(d, dat)
test_pi1$bloom <- ifelse(test_pi1$Chl > test_pi1$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi1, aes(x = doy, y = Chl,color=bloom))+
  geom_line(data = test_pi1, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi1, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

dat <- left_join(test_pi,test_pi1,by=c("Date","Chl","doy"))
dat <- dat %>% dplyr::select("Date","Chl","doy","bloom.y") %>% rename(Bloom=bloom.y)
dat$Bloom[is.na(dat$Bloom)] <- "Yes"

p36 <- ggplot()+
  geom_path(data=dat,aes(x=Date,y=Chl),color="black")+
  geom_point(data=dat%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl),color="blue")+ggtitle("South Delta")
rm(test,b,dat,test_pi,test_pi1)

tiff("Figures/Bloom detection discrete/Spline_99PI_2iter.tiff", units="in", width=16, height=9, res=300)
ggarrange(p32,p33,p34,p35,p36,ncol=2,nrow=3,common.legend=TRUE)
dev.off()



# 99% PI on spline fit - 3 iterations ---------------------------------------


# Suisun Bay
test <- CM %>% filter(Regions=="Suisun Bay") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

# Iteration 1
b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

# Iteration 2
d <- test_pi %>% filter(bloom=="No") %>% dplyr::select("Date","Chl","doy")
b <- gam(Chl~s(doy,bs="cp"),data= d)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi1 <- cbind(d, dat)
test_pi1$bloom <- ifelse(test_pi1$Chl > test_pi1$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi1, aes(x = doy, y = Chl,color=bloom))+
  geom_line(data = test_pi1, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi1, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

# Iteration 3
d <- test_pi1 %>% filter(bloom=="No") %>% dplyr::select("Date","Chl","doy")
b <- gam(Chl~s(doy,bs="cp"),data= d)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi2 <- cbind(d, dat)
test_pi2$bloom <- ifelse(test_pi2$Chl > test_pi2$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi1 %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi2, aes(x = doy, y = Chl,color=bloom))+
  geom_line(data = test_pi2, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi2, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

dat <- left_join(test_pi,test_pi2,by=c("Date","Chl","doy"))
dat <- dat %>% dplyr::select("Date","Chl","doy","bloom.y") %>% rename(Bloom=bloom.y)
dat$Bloom[is.na(dat$Bloom)] <- "Yes"

p32 <- ggplot()+
  geom_path(data=dat,aes(x=Date,y=Chl),color="black")+
  geom_point(data=dat%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl),color="blue")+ggtitle("Suisun Bay")
rm(test,b,dat,test_pi,test_pi1,test_pi2)



# Confluence
test <- CM %>% filter(Regions=="Confluence") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

# Iteration 1
b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

# Iteration 2
d <- test_pi %>% filter(bloom=="No") %>% dplyr::select("Date","Chl","doy")
b <- gam(Chl~s(doy,bs="cp"),data= d)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi1 <- cbind(d, dat)
test_pi1$bloom <- ifelse(test_pi1$Chl > test_pi1$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi1, aes(x = doy, y = Chl,color=bloom))+
  geom_line(data = test_pi1, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi1, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

# Iteration 3
d <- test_pi1 %>% filter(bloom=="No") %>% dplyr::select("Date","Chl","doy")
b <- gam(Chl~s(doy,bs="cp"),data= d)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi2 <- cbind(d, dat)
test_pi2$bloom <- ifelse(test_pi2$Chl > test_pi2$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi1 %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi2, aes(x = doy, y = Chl,color=bloom))+
  geom_line(data = test_pi2, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi2, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

dat <- left_join(test_pi,test_pi2,by=c("Date","Chl","doy"))
dat <- dat %>% dplyr::select("Date","Chl","doy","bloom.y") %>% rename(Bloom=bloom.y)
dat$Bloom[is.na(dat$Bloom)] <- "Yes"

p33 <- ggplot()+
  geom_path(data=dat,aes(x=Date,y=Chl),color="black")+
  geom_point(data=dat%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl),color="blue")+ggtitle("Confluence")
rm(test,b,dat,test_pi,test_pi1,test_pi2)


# North Delta
test <- CM %>% filter(Regions=="North Delta") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

# Iteration 1
b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

# Iteration 2
d <- test_pi %>% filter(bloom=="No") %>% dplyr::select("Date","Chl","doy")
b <- gam(Chl~s(doy,bs="cp"),data= d)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi1 <- cbind(d, dat)
test_pi1$bloom <- ifelse(test_pi1$Chl > test_pi1$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi1, aes(x = doy, y = Chl,color=bloom))+
  geom_line(data = test_pi1, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi1, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

# Iteration 3
d <- test_pi1 %>% filter(bloom=="No") %>% dplyr::select("Date","Chl","doy")
b <- gam(Chl~s(doy,bs="cp"),data= d)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi2 <- cbind(d, dat)
test_pi2$bloom <- ifelse(test_pi2$Chl > test_pi2$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi1 %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi2, aes(x = doy, y = Chl,color=bloom))+
  geom_line(data = test_pi2, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi2, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

dat <- left_join(test_pi,test_pi2,by=c("Date","Chl","doy"))
dat <- dat %>% dplyr::select("Date","Chl","doy","bloom.y") %>% rename(Bloom=bloom.y)
dat$Bloom[is.na(dat$Bloom)] <- "Yes"

p34 <- ggplot()+
  geom_path(data=dat,aes(x=Date,y=Chl),color="black")+
  geom_point(data=dat%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl),color="blue")+ggtitle("North Delta")
rm(test,b,dat,test_pi,test_pi1,test_pi2)


# Central Delta
test <- CM %>% filter(Regions=="Central") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

# Iteration 1
b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

# Iteration 2
d <- test_pi %>% filter(bloom=="No") %>% dplyr::select("Date","Chl","doy")
b <- gam(Chl~s(doy,bs="cp"),data= d)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi1 <- cbind(d, dat)
test_pi1$bloom <- ifelse(test_pi1$Chl > test_pi1$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi1, aes(x = doy, y = Chl,color=bloom))+
  geom_line(data = test_pi1, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi1, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

# Iteration 3
d <- test_pi1 %>% filter(bloom=="No") %>% dplyr::select("Date","Chl","doy")
b <- gam(Chl~s(doy,bs="cp"),data= d)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi2 <- cbind(d, dat)
test_pi2$bloom <- ifelse(test_pi2$Chl > test_pi2$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi1 %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi2, aes(x = doy, y = Chl,color=bloom))+
  geom_line(data = test_pi2, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi2, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

dat <- left_join(test_pi,test_pi2,by=c("Date","Chl","doy"))
dat <- dat %>% dplyr::select("Date","Chl","doy","bloom.y") %>% rename(Bloom=bloom.y)
dat$Bloom[is.na(dat$Bloom)] <- "Yes"

p35 <- ggplot()+
  geom_path(data=dat,aes(x=Date,y=Chl),color="black")+
  geom_point(data=dat%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl),color="blue")+ggtitle("Central Delta")
rm(test,b,dat,test_pi,test_pi1,test_pi2)


# South Delta
test <- CM %>% filter(Regions=="South") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$doy <- yday(test$Date)

# Iteration 1
b <- gam(Chl~s(doy,bs="cp"),data=test)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi <- cbind(test, dat)
test_pi$bloom <- ifelse(test_pi$Chl > test_pi$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi, aes(x = doy, y = Chl,color=bloom)) + 
  geom_line(data = test_pi, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

# Iteration 2
d <- test_pi %>% filter(bloom=="No") %>% dplyr::select("Date","Chl","doy")
b <- gam(Chl~s(doy,bs="cp"),data= d)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi1 <- cbind(d, dat)
test_pi1$bloom <- ifelse(test_pi1$Chl > test_pi1$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi1, aes(x = doy, y = Chl,color=bloom))+
  geom_line(data = test_pi1, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi1, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

# Iteration 3
d <- test_pi1 %>% filter(bloom=="No") %>% dplyr::select("Date","Chl","doy")
b <- gam(Chl~s(doy,bs="cp"),data= d)
dat <- nlraa::predict_gam(b,interval="pred",level=0.99)
test_pi2 <- cbind(d, dat)
test_pi2$bloom <- ifelse(test_pi2$Chl > test_pi2$Q99.5, "Yes", "No")

ggplot() + 
  geom_point(data = test_pi %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi1 %>% filter(bloom=="Yes"), aes(x = doy, y = Chl),color="black") + 
  geom_point(data = test_pi2, aes(x = doy, y = Chl,color=bloom))+
  geom_line(data = test_pi2, aes(x = doy, y = Estimate)) + 
  geom_ribbon(data = test_pi2, aes(x = doy, ymin = Q0.5, ymax = Q99.5), 
              color = "red", alpha = 0.3) + ggtitle("99% prediction bands")

dat <- left_join(test_pi,test_pi2,by=c("Date","Chl","doy"))
dat <- dat %>% dplyr::select("Date","Chl","doy","bloom.y") %>% rename(Bloom=bloom.y)
dat$Bloom[is.na(dat$Bloom)] <- "Yes"

p36 <- ggplot()+
  geom_path(data=dat,aes(x=Date,y=Chl),color="black")+
  geom_point(data=dat%>% filter(Bloom=="Yes"),aes(x=Date,y=Chl),color="blue")+ggtitle("South Delta")
rm(test,b,dat,test_pi,test_pi1,test_pi2)

tiff("Figures/Bloom detection discrete/Spline_99PI_3iter.tiff", units="in", width=16, height=9, res=300)
ggarrange(p32,p33,p34,p35,p36,ncol=2,nrow=3,common.legend=TRUE)
dev.off()


library(scales)


# Regional plots, method subplots -----------------------------------------

p2<- p2+ggtitle("Chl > 10 ug/L")
p32<- p32+ggtitle("Chl > 99% PI of periodic spline - 3 iter")
p8<- p8+ggtitle("Chl > 75th quantile")
p14<-p14+ggtitle("Chl > 75% of median")
p26<- p26+ggtitle("Chl > 75th annual quantile")
p20<- p20+ggtitle("Chl > 75% of annual median")
tiff("Figures/Bloom detection discrete/MethodComp_SuisunBay.tiff", units="in", width=16, height=9, res=300)
ggarrange(p2,p32,p8,p14,p26,p20,ncol=2,nrow=3,common.legend=TRUE)
dev.off()

p3<- p3+ggtitle("Chl > 10 ug/L")
p33<- p33+ggtitle("Chl > 99% PI of periodic spline - 3 iter")
p9<- p9+ggtitle("Chl > 75th quantile")
p15<-p15+ggtitle("Chl > 75% of median")
p27<- p27+ggtitle("Chl > 75th annual quantile")
p21<- p21+ggtitle("Chl > 75% of annual median")
tiff("Figures/Bloom detection discrete/MethodComp_Confluence.tiff", units="in", width=16, height=9, res=300)
ggarrange(p3,p33,p9,p15,p27,p21,ncol=2,nrow=3,common.legend=TRUE)
dev.off()

p4<- p4+ggtitle("Chl > 10 ug/L")
p34<- p34+ggtitle("Chl > 99% PI of periodic spline - 3 iter")
p10<- p10+ggtitle("Chl > 75th quantile")
p16<-p16+ggtitle("Chl > 75% of median")
p28<- p28+ggtitle("Chl > 75th annual quantile")
p22<- p22+ggtitle("Chl > 75% of annual median")
tiff("Figures/Bloom detection discrete/MethodComp_NorthDelta.tiff", units="in", width=16, height=9, res=300)
ggarrange(p4,p34,p10,p16,p28,p22,ncol=2,nrow=3,common.legend=TRUE)
dev.off()

p5<- p5+ggtitle("Chl > 10 ug/L")
p35<- p35+ggtitle("Chl > 99% PI of periodic spline - 3 iter")
p11<- p11+ggtitle("Chl > 75th quantile")
p17<-p17+ggtitle("Chl > 75% of median")
p29<- p29+ggtitle("Chl > 75th annual quantile")
p23<- p23+ggtitle("Chl > 75% of annual median")
tiff("Figures/Bloom detection discrete/MethodComp_CentralDelta.tiff", units="in", width=16, height=9, res=300)
ggarrange(p5,p35,p11,p17,p29,p23,ncol=2,nrow=3,common.legend=TRUE)
dev.off()

p6<- p6+ggtitle("Chl > 10 ug/L")
p36<- p36+ggtitle("Chl > 99% PI of periodic spline - 3 iter")
p12<- p12+ggtitle("Chl > 75th quantile")
p18<-p18+ggtitle("Chl > 75% of median")
p30<- p30+ggtitle("Chl > 75th annual quantile")
p24<- p24+ggtitle("Chl > 75% of annual median")
tiff("Figures/Bloom detection discrete/MethodComp_SouthDelta.tiff", units="in", width=16, height=9, res=300)
ggarrange(p6,p36,p12,p18,p30,p24,ncol=2,nrow=3,common.legend=TRUE)
dev.off()

# Std deviation -----------------------------------------------------------
# 1 standard deviation above the mean

# Suisun Bay
test <- CM %>% filter(Regions=="Suisun Bay") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
x = mean(test$Chl,na.rm=TRUE) + sd(test$Chl,na.rm=TRUE)
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p38 <- ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Suisun Bay")

# Confluence
test <- CM %>% filter(Regions=="Confluence") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
x = mean(test$Chl,na.rm=TRUE) + sd(test$Chl,na.rm=TRUE)
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p39 <- ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Confluence")

# North Delta
test <- CM %>% filter(Regions=="North Delta") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
x = mean(test$Chl,na.rm=TRUE) + sd(test$Chl,na.rm=TRUE)
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p40 <- ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("North Delta")

# Central Delta
test <- CM %>% filter(Regions=="Central") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
x = mean(test$Chl,na.rm=TRUE) + sd(test$Chl,na.rm=TRUE)
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p41 <- ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Central Delta")

# South Delta
test <- CM %>% filter(Regions=="South") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
x = mean(test$Chl,na.rm=TRUE) + sd(test$Chl,na.rm=TRUE)
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p42 <- ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("South Delta")

tiff("Figures/Bloom detection discrete/stdev.tiff", units="in", width=16, height=9, res=300)
ggarrange(p38,p39,p40,p41,p42,ncol=2,nrow=3,common.legend=TRUE)
dev.off()


# Delta-wide thresholds ---------------------------------------------------

# >1 standard deviation above the mean
test <- CM %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
x = mean(test$Chl,na.rm=TRUE) + sd(test$Chl,na.rm=TRUE)


test <- CM %>% filter(Regions == "Suisun Bay") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p44 <- ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Suisun Bay")

test <- CM %>% filter(Regions == "Confluence") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p45 <- ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Confluence")

test <- CM %>% filter(Regions == "North Delta") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p46 <- ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("North Delta")

test <- CM %>% filter(Regions == "Central") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p47 <- ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Central Delta")

test <- CM %>% filter(Regions == "South") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p48 <- ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("South Delta")

tiff("Figures/Bloom detection discrete/deltawide_stdev.tiff", units="in", width=16, height=9, res=300)
ggarrange(p44,p45,p46,p47,p48,ncol=2,nrow=3,common.legend=TRUE)
dev.off()


# 75th quantile
test <- CM %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
x = quantile(test$Chl, probs = 0.75,na.rm=TRUE)

test <- CM %>% filter(Regions=="Suisun Bay") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p8<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Suisun Bay")

test <- CM %>% filter(Regions=="Confluence") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p9<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Confluence")

test <- CM %>% filter(Regions=="North Delta") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p10<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("North Delta")

test <- CM %>% filter(Regions=="Central") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p11<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Central Delta")

test <- CM %>% filter(Regions=="South") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p12<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("South Delta")

tiff("Figures/Bloom detection discrete/deltawide_Quantile_75.tiff", units="in", width=16, height=9, res=300)
ggarrange(p8,p9,p10,p11,p12,ncol=2,nrow=3,common.legend=TRUE)
dev.off()

# 75th percentile
test <- CM %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
x = median(test$Chl,na.rm=TRUE)*1.75


test <- CM %>% filter(Regions=="Suisun Bay") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p8<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Suisun Bay")

test <- CM %>% filter(Regions=="Confluence") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p9<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Confluence")

test <- CM %>% filter(Regions=="North Delta") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p10<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("North Delta")

test <- CM %>% filter(Regions=="Central") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p11<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("Central Delta")

test <- CM %>% filter(Regions=="South") %>% group_by(Date) %>% summarize(Chl=mean(Chl,na.rm=TRUE))
test$bloom <- ifelse(test$Chl > x, "Yes", "No")
p12<-ggplot()+geom_path(data = test,aes(x=Date,y=Chl),color="black")+
  geom_point(data = test %>% filter(bloom=="Yes"), aes(x = Date, y = Chl),color="blue")+ggtitle("South Delta")

tiff("Figures/Bloom detection discrete/deltawide_percentile_75.tiff", units="in", width=16, height=9, res=300)
ggarrange(p8,p9,p10,p11,p12,ncol=2,nrow=3,common.legend=TRUE)
dev.off()

# Consolidate Outputs
library(magick)

# list all tif/tiff files in the folder (adjust path)
tiff_paths <- list.files("Figures/Bloom detection discrete", pattern = "\\.tif(f)?$", full.names = TRUE)

# optionally sort them naturally if filenames have numbers
tiff_paths <- sort(tiff_paths)

# read all images into a magick image list
#imgs <- image_read(tiff_paths)
imgs_annotated <- lapply(tiff_paths, function(path) {
  img <- image_read(path)
  fname <- basename(path)  # just the file name, not full path
  # Add filename annotation at bottom center
  image_annotate(img, fname, gravity = "south", size = 45, color = "black", boxcolor = "white")
})
# append them into a single multi-frame object
imgs_joined <- image_join(imgs_annotated)

image_write(imgs_joined, path = "Figures/Bloom detection discrete/all_figures.pdf", format = "pdf")

