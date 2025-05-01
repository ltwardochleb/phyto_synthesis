# author: Ryan Hankins
# Goal: plot the collection frequency of certain EDI data in different parts of the Bay Delta since 2008
# EDI metrics: temperature, conductivity,salinity, water clarity, tidal condition


## packages ############################################
#install.packages("reshape2")
library(reshape2)
library(readxl)
library(plotly)
#devtools::install_github("sbashevkin/spacetools")
#devtools::install_github("InteragencyEcologicalProgram/deltamapr")
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
## Read in the EDI dataset by running edi.731.7.r##########

source("edi.731.7.r")

dt1 <- as_tibble(dt1)



# load the delta shapefile
Delta<-deltamapr::R_EDSM_Subregions_Mahardja_FLOAT

### create tibbles for each metric #########################
edi_data <- dt1 %>%
  dplyr::select(Temperature, Conductivity, Salinity, Secchi, Tide, Date, Datetime, Latitude, Longitude) %>%
  filter(!is.na(Latitude) & !is.na(Longitude))%>%
  filter(Datetime > as.Date('2008-01-01'))

#read in region data
regions<-st_read("Rosies_regions_edited_shp/Rosies_regions_edited.shp")
regions <- st_transform(regions, 26910)
st_crs(regions)
#add the subregions to the edi dataset
#convert stations file to simple features so we can map index number to subregions
geo_edi_data = edi_data %>% st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  #convert to UTMs so it's in the same coordinate reference system as the Delta shapefile
  st_transform(crs = 26910)
st_crs(geo_edi_data)
edi_data_with_regions <- st_join(geo_edi_data, regions["Regions"], join = st_intersects, left= TRUE) %>%  
  filter(!is.na(Regions)) %>%
  st_drop_geometry() %>%
  distinct(Datetime, .keep_all = TRUE)

############### Summarize long-term sample coverage by season, year, region ###################################################################

#convert dates to months and seasons
edi_data_with_regions_monthly <-edi_data_with_regions%>%mutate(Month = month(Date), #create a month and year variable
                                    Year = year(Date), 
                                    Year=if_else(Month==12, Year+1, Year), # Move Decembers to the following year
                                    Season=case_when(Month%in%3:5 ~ "Spring", # Create seasonal variables
                                                     Month%in%6:8 ~ "Summer",
                                                     Month%in%9:11 ~ "Fall",
                                                     Month%in%c(12, 1, 2) ~ "Winter",
                                                     TRUE ~ NA_character_))

edi_data_with_regions_monthly2 <-edi_data%>%mutate(Month = month(Date), #create a month and year variable
                                                               Year = year(Date), 
                                                               Year=if_else(Month==12, Year+1, Year), # Move Decembers to the following year
                                                               Season=case_when(Month%in%3:5 ~ "Spring", # Create seasonal variables
                                                                                Month%in%6:8 ~ "Summer",
                                                                                Month%in%9:11 ~ "Fall",
                                                                                Month%in%c(12, 1, 2) ~ "Winter",
                                                                                TRUE ~ NA_character_))

############## Temperature Summary plots #########################################################

temperature_summary <- edi_data_with_regions_monthly %>%
  dplyr::select(-Conductivity, -Salinity, -Secchi, -Tide)

temperature_summary2 <- edi_data_with_regions_monthly2 %>%
  dplyr::select(-Conductivity, -Salinity, -Secchi, -Tide)

temperature_count <-temperature_summary%>%group_by(Regions, Year, Season)%>%summarize(n=n())

temperature_count2 <-temperature_summary2%>%group_by(Year, Season, Latitude, Longitude)%>%summarize(n=n())

#look at seasonal sample coverage for temperature
temp_sites_plot<-temperature_count%>%ggplot(aes(x=Year,y=Regions))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=4)+
  ggtitle("temperature sampling by season")+
  facet_grid(Season~.)+
  theme_classic()+
  theme(legend.position = "none")
temp_sites_plot
ggsave("temp_sampling_coverage.png", temp_sites_plot,  width=14, height=8)


#look at monthly sample coverage
temp_site_counts2<-temperature_summary%>%group_by(Regions, Year, Month)%>%summarize(n=n())
temp_sites_plot2<-temp_site_counts2%>%ggplot(aes(x=Year,y=Regions))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=1)+
  ggtitle("Temp Sampling By Month")+
  facet_grid(Month~.)+
  theme_classic()+
  theme(legend.position = "none")
temp_sites_plot2
ggsave("temp_monthly_sampling_coverage.png", temp_sites_plot2,  width=14, height=8)

#look at yearly sample coverage
temp_site_counts3<-temperature_summary%>%group_by(Regions, Year)%>%summarize(n=n())
temp_sites_plot3<-temp_site_counts3%>%ggplot(aes(x=Year,y=Regions))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=2)+
  ggtitle("Temperature Sampling By Year")+
  theme_classic()+
  theme(legend.position = "none")
temp_sites_plot3
ggsave("temp_yearly_sampling_coverage.png", temp_sites_plot3,  width=14, height=8)


Map_temp_n<-ggplot(data = deltamapr::WW_Delta) +
  geom_sf(aes(),fill="#3fd9db", color= "#3fd9db") +
  geom_point(data = temperature_count2,
             aes(x = Longitude, y = Latitude, fill= Year),color= "black",pch=21, size = 3) +
  coord_sf(xlim = c(-121.2, -122.15), ylim = c(37.66, 38.48), expand = FALSE)+
  theme_void(base_size = 20)+
  
  #guides(fill=guide_legend(title="yearly temp sampling"))+
  theme(legend.position = c(.1, .8))+
  annotation_scale(location = "bl",text_cex =1, pad_x = unit(0, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x= unit(2.2, "in"),
                         style = ggspatial::north_arrow_orienteering(fill = c("black", "black"),
                                                                     text_size = 10))+
  scale_fill_distiller(palette = "Spectral")

Map_temp_n

#### Conductivity ###################################################################################

con_summary <- edi_data_with_regions_monthly %>%
  dplyr::select(-Temperature, -Salinity, -Secchi, -Tide)

con_count <- con_summary%>%group_by(Regions, Year, Season)%>%summarize(n=n())

con_summary2 <- edi_data_with_regions_monthly2 %>%
  dplyr::select(-Temperature, -Salinity, -Secchi, -Tide)

con_count2 <- con_summary2%>%group_by(Year, Season, Latitude, Longitude)%>%summarize(n=n())


#look at seasonal sample coverage for temperature
con_sites_plot<-con_count%>%ggplot(aes(x=Year,y=Regions))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=4)+
  ggtitle("conductivity sampling by season")+
  facet_grid(Season~.)+
  theme_classic()+
  theme(legend.position = "none")
con_sites_plot
ggsave("con_sampling_coverage.png", con_sites_plot,  width=14, height=8)


#look at monthly sample coverage
con_site_counts2<-con_summary%>%group_by(Regions, Year, Month)%>%summarize(n=n())
con_sites_plot2<-con_site_counts2%>%ggplot(aes(x=Year,y=Regions))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=1)+
  ggtitle("Conductivity Sampling By Month")+
  facet_grid(Month~.)+
  theme_classic()+
  theme(legend.position = "none")
con_sites_plot2
ggsave("temp_monthly_sampling_coverage.png", con_sites_plot2,  width=14, height=8)

#look at yearly sample coverage
con_site_counts3<-con_summary%>%group_by(Regions, Year)%>%summarize(n=n())
con_sites_plot3<-con_site_counts3%>%ggplot(aes(x=Year,y=Regions))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=2)+
  ggtitle("Conductivity Sampling By Year")+
  theme_classic()+
  theme(legend.position = "none")
con_sites_plot3
ggsave("con_yearly_sampling_coverage.png", con_sites_plot3,  width=14, height=8)

Map_con_n<-ggplot(data = deltamapr::WW_Delta) +
  geom_sf(aes(),fill="#3fd9db", color= "#3fd9db") +
  geom_point(data = con_count2,
             aes(x = Longitude, y = Latitude, fill= Year),color= "black",pch=21, size = 3) +
  coord_sf(xlim = c(-121.2, -122.15), ylim = c(37.66, 38.48), expand = FALSE)+
  theme_void(base_size = 20)+

  guides(fill=guide_legend(title="yearly conductivity sampling"))+
  theme(legend.position = c(.1, .8))+
  annotation_scale(location = "bl",text_cex =1, pad_x = unit(0, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x= unit(2.2, "in"),
                         style = ggspatial::north_arrow_orienteering(fill = c("black", "black"),
                                                                     text_size = 10))+
  scale_fill_distiller(palette = "Spectral")

Map_con_n
#### Salinity ###################################################################################

salt_summary <- edi_data_with_regions_monthly %>%
  dplyr::select(-Temperature, -Conductivity, -Secchi, -Tide)

salt_count <- salt_summary%>%group_by(Regions, Year, Season)%>%summarize(n=n())

#look at seasonal sample coverage for temperature
salt_sites_plot<-salt_count%>%ggplot(aes(x=Year,y=Regions))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=4)+
  ggtitle("salinity sampling by season")+
  facet_grid(Season~.)+
  theme_classic()+
  theme(legend.position = "none")
salt_sites_plot
ggsave("salt_sampling_coverage.png", salt_sites_plot,  width=14, height=8)


#look at monthly sample coverage
salt_site_counts2<-salt_summary%>%group_by(Regions, Year, Month)%>%summarize(n=n())
salt_sites_plot2<-salt_site_counts2%>%ggplot(aes(x=Year,y=Regions))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=1)+
  ggtitle("Conductivity Sampling By Month")+
  facet_grid(Month~.)+
  theme_classic()+
  theme(legend.position = "none")
salt_sites_plot2
ggsave("salt_monthly_sampling_coverage.png", salt_sites_plot2,  width=14, height=8)

#look at yearly sample coverage
salt_site_counts3<-salt_summary%>%group_by(Regions, Year)%>%summarize(n=n())
salt_sites_plot3<-salt_site_counts3%>%ggplot(aes(x=Year,y=Regions))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=2)+
  ggtitle("Salinity Sampling By Year")+
  theme_classic()+
  theme(legend.position = "none")
salt_sites_plot3
ggsave("salt_yearly_sampling_coverage.png", salt_sites_plot3,  width=14, height=8)


#### Secchi ###################################################################################

sec_summary <- edi_data_with_regions_monthly %>%
  dplyr::select(-Temperature, -Conductivity, -Salinity, -Tide)

sec_count <- sec_summary%>%group_by(Regions, Year, Season)%>%summarize(n=n())

#look at seasonal sample coverage for temperature
sec_sites_plot<-sec_count%>%ggplot(aes(x=Year,y=Regions))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=4)+
  ggtitle("secchi sampling by season")+
  facet_grid(Season~.)+
  theme_classic()+
  theme(legend.position = "none")
sec_sites_plot
ggsave("sec_sampling_coverage.png", sec_sites_plot,  width=14, height=8)


#look at monthly sample coverage
sec_site_counts2<-sec_summary%>%group_by(Regions, Year, Month)%>%summarize(n=n())
sec_sites_plot2<-sec_site_counts2%>%ggplot(aes(x=Year,y=Regions))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=1)+
  ggtitle("Secchi Sampling By Month")+
  facet_grid(Month~.)+
  theme_classic()+
  theme(legend.position = "none")
sec_sites_plot2
ggsave("sec_monthly_sampling_coverage.png", sec_sites_plot2,  width=14, height=8)

#look at yearly sample coverage
sec_site_counts3<-salt_summary%>%group_by(Regions, Year)%>%summarize(n=n())
sec_sites_plot3<-sec_site_counts3%>%ggplot(aes(x=Year,y=Regions))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=2)+
  ggtitle("Secchi Sampling By Year")+
  theme_classic()+
  theme(legend.position = "none")
sec_sites_plot3
ggsave("sec_yearly_sampling_coverage.png", sec_sites_plot3,  width=14, height=8)


#### Tide ###################################################################################

tide_summary <- edi_data_with_regions_monthly %>%
  dplyr::select(-Temperature, -Conductivity, -Salinity, -Secchi)

tide_count <- sec_summary%>%group_by(Regions, Year, Season)%>%summarize(n=n())

#look at seasonal sample coverage for temperature
tide_sites_plot<-tide_count%>%ggplot(aes(x=Year,y=Regions))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=4)+
  ggtitle("tidal condition sampling by season")+
  facet_grid(Season~.)+
  theme_classic()+
  theme(legend.position = "none")
tide_sites_plot
ggsave("tide_sampling_coverage.png", tide_sites_plot,  width=14, height=8)


#look at monthly sample coverage
tide_site_counts2<-tide_summary%>%group_by(Regions, Year, Month)%>%summarize(n=n())
tide_sites_plot2<-sec_site_counts2%>%ggplot(aes(x=Year,y=Regions))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=1)+
  ggtitle("tidal condition Sampling By Month")+
  facet_grid(Month~.)+
  theme_classic()+
  theme(legend.position = "none")
tide_sites_plot2
ggsave("tide_monthly_sampling_coverage.png", tide_sites_plot2,  width=14, height=8)

#look at yearly sample coverage
tide_site_counts3<-tide_summary%>%group_by(Regions, Year)%>%summarize(n=n())
tide_sites_plot3<-tide_site_counts3%>%ggplot(aes(x=Year,y=Regions))+geom_tile(aes(fill=n))+
  scale_fill_gradient(low="white",high="red")+
  geom_text(aes(label=round(n,1)),size=2)+
  ggtitle("tidal condition Sampling By Year")+
  theme_classic()+
  theme(legend.position = "none")
tide_sites_plot3
ggsave("tide_yearly_sampling_coverage.png", tide_sites_plot3,  width=14, height=8)
