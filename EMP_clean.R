############## Examine EMP data completeness
##### Kyle Leathers, 2025


### Load packages
#install.packages("")
library(dplyr)
library(zoo)
library(tidyverse)
library(deltamapr)
library(sf)
library(ggrepel)
library(ggspatial)


### Load EMP data
EMP_df<-read.csv("EMP_DWQ_1975_2023.csv", header=TRUE)


#### Clean data ####
EMP_df <- EMP_df %>%
  mutate(DissAmmonia = ifelse(DissAmmonia_Sign== "<" &DissAmmonia>0.05,NA, DissAmmonia))%>%
  mutate(DissAmmonia = ifelse(DissAmmonia<0.05, 0.05, DissAmmonia)) %>%
  mutate(DissOrthophos = ifelse(DissOrthophos_Sign== "<" &DissOrthophos>0.3,NA, DissOrthophos))%>%
  mutate(DissOrthophos = ifelse(DissOrthophos<0.05, 0.05, DissOrthophos))%>%
  mutate(DissNitrateNitrite = ifelse(DissNitrateNitrite_Sign== "<" &DissNitrateNitrite>1,NA, DissNitrateNitrite))%>%
  mutate(DissNitrateNitrite = ifelse(DissNitrateNitrite<0.05, 0.05, DissNitrateNitrite))

#Reformat
colnames(EMP_df)

EMP_df <- EMP_df %>%
  subset(select = c(Station, Date, Time, Chla, DissAmmonia,DissNitrateNitrite,DissOrthophos,WaterTempSurface, TurbiditySurface,
                    TSS,WaterDepth ,DOSurface,SpCndSurface, pHSurface))%>%
  mutate(DIN_mg_l_mean= ifelse(is.na(DissAmmonia)|is.na(DissNitrateNitrite), NA, DissNitrateNitrite+DissAmmonia)) %>%
  rename(chla_ug_l= Chla,
         no23_mg_l=DissNitrateNitrite,
         nh4_mg_l= DissAmmonia,
         po4_mg_l=DissOrthophos,
         Temp_C =WaterTempSurface,
         Turbidity_FNU =TurbiditySurface ,
         TSS_mg_l= TSS,
         WaterDepth_ft =WaterDepth,
         DO_mg_l = DOSurface,
         SpCnd_uS_cm = SpCndSurface,
         pH = pHSurface,
         Site_Abbrev = Station) %>%
  mutate(Year_Month = as.yearmon(Date, "%Y-%m-%d"))%>%
  mutate(month = substr(Year_Month,1,3)) %>%
  mutate(Winter = ifelse(month=="Dec"|month=="Jan"|month=="Feb", 1, 0)) %>%
  mutate(spring = ifelse(month=="Mar"|month=="Apr"|month=="May", 1, 0)) %>%
  mutate(summer = ifelse(month=="Jun"|month=="Jul"|month=="Aug", 1, 0)) %>%
  mutate(fall = ifelse(month=="Sep"|month=="Oct"|month=="Nov", 1, 0)) %>%
  group_by(Site_Abbrev, Year_Month) %>%
  mutate(n_rows = n()) %>% # Prior to 1998, especially 1992, sites can have 2 measurements per month
  unique() %>%
  mutate(year_month = as.Date(Year_Month))%>%
  mutate(Month_day = format(as.Date(year_month), "%m-%d"))%>%
  mutate(Season= ifelse(Month_day <format(as.Date("03-01",format="%m-%d"), "%m-%d")|Month_day >=format(as.Date("12-01","%m-%d"), "%m-%d"), "Winter", NA)) %>%
  mutate(Season= ifelse(Month_day <format(as.Date("06-01","%m-%d"), "%m-%d")&Month_day >=format(as.Date("03-01","%m-%d"), "%m-%d"), "Spring", Season))%>%
  mutate(Season= ifelse(Month_day <format(as.Date("09-01","%m-%d"), "%m-%d")&Month_day >=format(as.Date("06-01","%m-%d"), "%m-%d"), "Summer", Season))%>%
  mutate(Season= ifelse(Month_day <format(as.Date("12-01","%m-%d"), "%m-%d")&Month_day >=format(as.Date("09-01","%m-%d"), "%m-%d"), "Fall", Season)) %>%
 # subset(year_month >= as.Date("2014-01-01",format = "%Y-%m-%d")) %>%
  mutate(Year_Month=as.character(Year_Month),
         year_month= as.character(year_month))%>%
  subset(select = -c(Month_day))

#Remove sites we do not consider to be in the Delta
#Combine sites that had slight location change
unique(EMP_df$Site_Abbrev)
EMP_df <- EMP_df %>%
  subset(Site_Abbrev != "D41"&Site_Abbrev != "D41A"&Site_Abbrev != "D42"&Site_Abbrev != "D6"&Site_Abbrev != "NZ002"&Site_Abbrev != "NZ004"&
           Site_Abbrev != "NZ325") %>%
  mutate(Site_Abbrev = ifelse(Site_Abbrev =="P12", "P12A", Site_Abbrev))%>%
  mutate(Site_Abbrev = ifelse(Site_Abbrev =="P10", "P10A", Site_Abbrev))%>%
  mutate(Site_Abbrev = ifelse(Site_Abbrev =="MD7", "MD7A", Site_Abbrev))%>%
  mutate(Site_Abbrev = ifelse(Site_Abbrev =="MD10", "MD10A", Site_Abbrev))%>%
  mutate(Site_Abbrev = ifelse(Site_Abbrev =="C3", "C3A", Site_Abbrev))%>%
  mutate(Site_Abbrev = ifelse(Site_Abbrev =="C10", "C10A", Site_Abbrev))%>%
  mutate(Site_Abbrev = ifelse(Site_Abbrev =="S42", "NZS42", Site_Abbrev))


#### Examine data completeness ####

# For each site, examine the number of rows where every metric is recorded
EMP_df_na_omit <- EMP_df %>%
  na.omit() %>%
  mutate(year = as.numeric(substr(Date,1,4))) %>%
  group_by(Site_Abbrev, year) %>%
  mutate(complete_n= n())
# 2,660 rows remaining from 15,309 to start Data goes from 2009 to 2023.

plot_all_param_EMP<-ggplot(EMP_df_na_omit, aes(x = year, y = Site_Abbrev, fill = complete_n)) +
  geom_tile()+
  theme_classic(base_size = 20)+
  scale_fill_distiller(palette = "Spectral")
plot_all_param_EMP
#looks like data completeness improves in 2017 for a lot of sites. 
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/plot_all_param_EMP.pdf",plot_all_param_EMP, width = 9, height = 8 )

# examine counts of data completeness for all other parameters
EMP_df <- EMP_df  %>%
  mutate(year = as.numeric(substr(Date,1,4))) %>%
  group_by(Site_Abbrev, year) %>%
  mutate(chla_n= sum(!is.na(chla_ug_l)))%>%
  mutate(nh4_n= sum(!is.na(nh4_mg_l)))%>%
  mutate(no23_n= sum(!is.na(no23_mg_l)))%>%
  mutate(po4_n= sum(!is.na(po4_mg_l)))%>%
  mutate(Temp_n= sum(!is.na(Temp_C)))%>%
  mutate(Turbidity_n= sum(!is.na(Turbidity_FNU)))%>%
  mutate(TSS_n= sum(!is.na(TSS_mg_l)))%>%
  mutate(Depth_n= sum(!is.na(WaterDepth_ft)))%>%
  mutate(DO_n= sum(!is.na(DO_mg_l)))%>%
  mutate(SpCnd_n= sum(!is.na(SpCnd_uS_cm)))%>%
  mutate(pH_n= sum(!is.na(pH)))%>%
  mutate(DIN_n= sum(!is.na(DIN_mg_l_mean))) %>%
  ungroup() %>%
  group_by(Site_Abbrev) %>%
  mutate(chla_n_total= sum(!is.na(chla_ug_l)))%>%
  mutate(nh4_n_total= sum(!is.na(nh4_mg_l)))%>%
  mutate(no23_n_total= sum(!is.na(no23_mg_l)))%>%
  mutate(po4_n_total= sum(!is.na(po4_mg_l)))%>%
  mutate(Temp_n_total= sum(!is.na(Temp_C)))%>%
  mutate(Turbidity_n_total= sum(!is.na(Turbidity_FNU)))%>%
  mutate(TSS_n_total= sum(!is.na(TSS_mg_l)))%>%
  mutate(Depth_n_total= sum(!is.na(WaterDepth_ft)))%>%
  mutate(DO_n_total= sum(!is.na(DO_mg_l)))%>%
  mutate(SpCnd_n_total= sum(!is.na(SpCnd_uS_cm)))%>%
  mutate(pH_n_total= sum(!is.na(pH)))%>%
  mutate(DIN_n_total= sum(!is.na(DIN_mg_l_mean)))
  

plot_chla_EMP<-ggplot(EMP_df, aes(x = year, y = Site_Abbrev, fill = chla_n)) +
  geom_tile()+
  theme_classic(base_size = 20)+
  scale_fill_distiller(palette = "Spectral")
plot_chla_EMP
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/plot_chla_EMP.pdf",plot_chla_EMP, width = 9, height = 8 )

plot_nh4_EMP<-ggplot(EMP_df, aes(x = year, y = Site_Abbrev, fill = nh4_n)) +
  geom_tile()+
  theme_classic(base_size = 20)+
  scale_fill_distiller(palette = "Spectral")
plot_nh4_EMP
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/plot_nh4_EMP.pdf",plot_nh4_EMP, width = 9, height = 8 )

plot_no23_EMP<-ggplot(EMP_df, aes(x = year, y = Site_Abbrev, fill = no23_n)) +
  geom_tile()+
  theme_classic(base_size = 20)+
  scale_fill_distiller(palette = "Spectral")
plot_no23_EMP
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/plot_no23_EMP.pdf",plot_no23_EMP, width = 9, height = 8 )

plot_po4_EMP<-ggplot(EMP_df, aes(x = year, y = Site_Abbrev, fill = po4_n)) +
  geom_tile()+
  theme_classic(base_size = 20)+
  scale_fill_distiller(palette = "Spectral")
plot_po4_EMP
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/plot_po4_EMP.pdf",plot_po4_EMP, width = 9, height = 8 )

plot_Temp_EMP<-ggplot(EMP_df, aes(x = year, y = Site_Abbrev, fill = Temp_n)) +
  geom_tile()+
  theme_classic(base_size = 20)+
  scale_fill_distiller(palette = "Spectral")
plot_Temp_EMP
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/plot_Temp_EMP.pdf",plot_Temp_EMP, width = 9, height = 8 )

plot_Turbidity_EMP<-ggplot(EMP_df, aes(x = year, y = Site_Abbrev, fill = Turbidity_n)) +
  geom_tile()+
  theme_classic(base_size = 20)+
  scale_fill_distiller(palette = "Spectral")
plot_Turbidity_EMP
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/plot_Turbidity_EMP.pdf",plot_Turbidity_EMP, width = 9, height = 8 )

plot_TSS_EMP<-ggplot(EMP_df, aes(x = year, y = Site_Abbrev, fill = TSS_n)) +
  geom_tile()+
  theme_classic(base_size = 20)+
  scale_fill_distiller(palette = "Spectral")
plot_TSS_EMP
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/plot_TSS_EMP.pdf",plot_TSS_EMP, width = 9, height = 8 )

plot_Depth_EMP<-ggplot(EMP_df, aes(x = year, y = Site_Abbrev, fill = Depth_n)) +
  geom_tile()+
  theme_classic(base_size = 20)+
  scale_fill_distiller(palette = "Spectral")
plot_Depth_EMP
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/plot_Depth_EMP.pdf",plot_Depth_EMP, width = 9, height = 8 )

plot_DO_EMP<-ggplot(EMP_df, aes(x = year, y = Site_Abbrev, fill = DO_n)) +
  geom_tile()+
  theme_classic(base_size = 20)+
  scale_fill_distiller(palette = "Spectral")
plot_DO_EMP
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/plot_DO_EMP.pdf",plot_DO_EMP, width = 9, height = 8 )

plot_SpCnd_EMP<-ggplot(EMP_df, aes(x = year, y = Site_Abbrev, fill = SpCnd_n)) +
  geom_tile()+
  theme_classic(base_size = 20)+
  scale_fill_distiller(palette = "Spectral")
plot_SpCnd_EMP
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/plot_SpCnd_EMP.pdf",plot_SpCnd_EMP, width = 9, height = 8 )

plot_pH_EMP<-ggplot(EMP_df, aes(x = year, y = Site_Abbrev, fill = pH_n)) +
  geom_tile()+
  theme_classic(base_size = 20)+
  scale_fill_distiller(palette = "Spectral")
plot_pH_EMP
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/plot_pH_EMP.pdf",plot_pH_EMP, width = 9, height = 8 )

plot_DIN_EMP<-ggplot(EMP_df, aes(x = year, y = Site_Abbrev, fill = DIN_n)) +
  geom_tile()+
  theme_classic(base_size = 20)+
  scale_fill_distiller(palette = "Spectral")
plot_DIN_EMP
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/plot_DIN_EMP.pdf",plot_DIN_EMP, width = 9, height = 8 )

#### spatial examination #####

# Load Lat/Long
site_lat_long_emp<-read.csv('EMP_DWQ_Stations_1975-2023.csv', header= TRUE)
site_lat_long_emp <- site_lat_long_emp %>%
  subset(select = c(Station, Latitude, Longitude) ) %>%
  rename(Site_Abbrev = Station)%>%
  mutate(data_type= "EMP")

EMP_df <- left_join(EMP_df, site_lat_long_emp, by = c('Site_Abbrev'))
colnames(EMP_df)
EMP_df_map <- EMP_df %>%
  subset(select = c(Site_Abbrev, Latitude, Longitude,chla_n_total,nh4_n_total,no23_n_total,po4_n_total,Temp_n_total,Turbidity_n_total, TSS_n_total,
                    Depth_n_total,DO_n_total,SpCnd_n_total,pH_n_total,DIN_n_total)) %>%
  unique() %>%
  mutate()


### Make map
colnames(EMP_df_map)
Map_chla_n<-ggplot(data = deltamapr::WW_Delta) +
  geom_sf(aes(),fill="#7fd6d4", color= "#7fd6d4") +
  geom_point(data = EMP_df_map,
             aes(x = Longitude, y = Latitude, fill= chla_n_total),color= "black",pch=21, size = 3) +
  coord_sf(xlim = c(-121.2, -122.15), ylim = c(37.66, 38.48), expand = FALSE)+
  theme_void(base_size = 20)+
  geom_text_repel(data = EMP_df_map, aes(x = Longitude, y = Latitude, label = Site_Abbrev), 
                  fontface = "bold", nudge_x = c(), nudge_y = c(), color= "#AD2A1A",
                  bg.color = "white",
                  bg.r = 0.1)+ 
 # guides(fill=guide_legend(title="Legend"))+
  theme(legend.position = c(.2, .85))+
  annotation_scale(location = "bl",text_cex =1, pad_x = unit(0, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x= unit(2.2, "in"),
                         style = ggspatial::north_arrow_orienteering(fill = c("black", "black"),
                                                                     text_size = 15))+
  scale_fill_distiller(palette = "Spectral")

Map_chla_n
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/Map_chla_n.pdf",Map_chla_n, width = 10, height = 7 )

Map_nh4_n<-ggplot(data = deltamapr::WW_Delta) +
  geom_sf(aes(),fill="#7fd6d4", color= "#7fd6d4") +
  geom_point(data = EMP_df_map,
             aes(x = Longitude, y = Latitude, fill= nh4_n_total),color= "black",pch=21, size = 3) +
  coord_sf(xlim = c(-121.2, -122.15), ylim = c(37.66, 38.48), expand = FALSE)+
  theme_void(base_size = 20)+
  geom_text_repel(data = EMP_df_map, aes(x = Longitude, y = Latitude, label = Site_Abbrev), 
                  fontface = "bold", nudge_x = c(), nudge_y = c(), color= "#AD2A1A",
                  bg.color = "white",
                  bg.r = 0.1)+ 
  # guides(fill=guide_legend(title="Legend"))+
  theme(legend.position = c(.2, .85))+
  annotation_scale(location = "bl",text_cex =1, pad_x = unit(0, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x= unit(2.2, "in"),
                         style = ggspatial::north_arrow_orienteering(fill = c("black", "black"),
                                                                     text_size = 15))+
  scale_fill_distiller(palette = "Spectral")

Map_nh4_n
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/Map_nh4_n.pdf",Map_nh4_n, width = 10, height = 7 )

Map_no23_n<-ggplot(data = deltamapr::WW_Delta) +
  geom_sf(aes(),fill="#7fd6d4", color= "#7fd6d4") +
  geom_point(data = EMP_df_map,
             aes(x = Longitude, y = Latitude, fill= no23_n_total),color= "black",pch=21, size = 3) +
  coord_sf(xlim = c(-121.2, -122.15), ylim = c(37.66, 38.48), expand = FALSE)+
  theme_void(base_size = 20)+
  geom_text_repel(data = EMP_df_map, aes(x = Longitude, y = Latitude, label = Site_Abbrev), 
                  fontface = "bold", nudge_x = c(), nudge_y = c(), color= "#AD2A1A",
                  bg.color = "white",
                  bg.r = 0.1)+ 
  # guides(fill=guide_legend(title="Legend"))+
  theme(legend.position = c(.2, .85))+
  annotation_scale(location = "bl",text_cex =1, pad_x = unit(0, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x= unit(2.2, "in"),
                         style = ggspatial::north_arrow_orienteering(fill = c("black", "black"),
                                                                     text_size = 15))+
  scale_fill_distiller(palette = "Spectral")

Map_no23_n
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/Map_no23_n.pdf",Map_no23_n, width = 10, height = 7 )


Map_po4_n<-ggplot(data = deltamapr::WW_Delta) +
  geom_sf(aes(),fill="#7fd6d4", color= "#7fd6d4") +
  geom_point(data = EMP_df_map,
             aes(x = Longitude, y = Latitude, fill= po4_n_total),color= "black",pch=21, size = 3) +
  coord_sf(xlim = c(-121.2, -122.15), ylim = c(37.66, 38.48), expand = FALSE)+
  theme_void(base_size = 20)+
  geom_text_repel(data = EMP_df_map, aes(x = Longitude, y = Latitude, label = Site_Abbrev), 
                  fontface = "bold", nudge_x = c(), nudge_y = c(), color= "#AD2A1A",
                  bg.color = "white",
                  bg.r = 0.1)+ 
  # guides(fill=guide_legend(title="Legend"))+
  theme(legend.position = c(.2, .85))+
  annotation_scale(location = "bl",text_cex =1, pad_x = unit(0, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x= unit(2.2, "in"),
                         style = ggspatial::north_arrow_orienteering(fill = c("black", "black"),
                                                                     text_size = 15))+
  scale_fill_distiller(palette = "Spectral")

Map_po4_n
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/Map_po4_n.pdf",Map_po4_n, width = 10, height = 7 )


Map_Temp_n<-ggplot(data = deltamapr::WW_Delta) +
  geom_sf(aes(),fill="#7fd6d4", color= "#7fd6d4") +
  geom_point(data = EMP_df_map,
             aes(x = Longitude, y = Latitude, fill= Temp_n_total),color= "black",pch=21, size = 3) +
  coord_sf(xlim = c(-121.2, -122.15), ylim = c(37.66, 38.48), expand = FALSE)+
  theme_void(base_size = 20)+
  geom_text_repel(data = EMP_df_map, aes(x = Longitude, y = Latitude, label = Site_Abbrev), 
                  fontface = "bold", nudge_x = c(), nudge_y = c(), color= "#AD2A1A",
                  bg.color = "white",
                  bg.r = 0.1)+ 
  # guides(fill=guide_legend(title="Legend"))+
  theme(legend.position = c(.2, .85))+
  annotation_scale(location = "bl",text_cex =1, pad_x = unit(0, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x= unit(2.2, "in"),
                         style = ggspatial::north_arrow_orienteering(fill = c("black", "black"),
                                                                     text_size = 15))+
  scale_fill_distiller(palette = "Spectral")

Map_Temp_n
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/Map_Temp_n.pdf",Map_Temp_n, width = 10, height = 7 )


Map_Turbidity_n<-ggplot(data = deltamapr::WW_Delta) +
  geom_sf(aes(),fill="#7fd6d4", color= "#7fd6d4") +
  geom_point(data = EMP_df_map,
             aes(x = Longitude, y = Latitude, fill= Turbidity_n_total),color= "black",pch=21, size = 3) +
  coord_sf(xlim = c(-121.2, -122.15), ylim = c(37.66, 38.48), expand = FALSE)+
  theme_void(base_size = 20)+
  geom_text_repel(data = EMP_df_map, aes(x = Longitude, y = Latitude, label = Site_Abbrev), 
                  fontface = "bold", nudge_x = c(), nudge_y = c(), color= "#AD2A1A",
                  bg.color = "white",
                  bg.r = 0.1)+ 
  # guides(fill=guide_legend(title="Legend"))+
  theme(legend.position = c(.2, .85))+
  annotation_scale(location = "bl",text_cex =1, pad_x = unit(0, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x= unit(2.2, "in"),
                         style = ggspatial::north_arrow_orienteering(fill = c("black", "black"),
                                                                     text_size = 15))+
  scale_fill_distiller(palette = "Spectral")

Map_Turbidity_n
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/Map_Turbidity_n.pdf",Map_Turbidity_n, width = 10, height = 7 )

Map_TSS_n<-ggplot(data = deltamapr::WW_Delta) +
  geom_sf(aes(),fill="#7fd6d4", color= "#7fd6d4") +
  geom_point(data = EMP_df_map,
             aes(x = Longitude, y = Latitude, fill= TSS_n_total),color= "black",pch=21, size = 3) +
  coord_sf(xlim = c(-121.2, -122.15), ylim = c(37.66, 38.48), expand = FALSE)+
  theme_void(base_size = 20)+
  geom_text_repel(data = EMP_df_map, aes(x = Longitude, y = Latitude, label = Site_Abbrev), 
                  fontface = "bold", nudge_x = c(), nudge_y = c(), color= "#AD2A1A",
                  bg.color = "white",
                  bg.r = 0.1)+ 
  # guides(fill=guide_legend(title="Legend"))+
  theme(legend.position = c(.2, .85))+
  annotation_scale(location = "bl",text_cex =1, pad_x = unit(0, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x= unit(2.2, "in"),
                         style = ggspatial::north_arrow_orienteering(fill = c("black", "black"),
                                                                     text_size = 15))+
  scale_fill_distiller(palette = "Spectral")

Map_TSS_n
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/Map_TSS_n.pdf",Map_TSS_n, width = 10, height = 7 )

Map_Depth_n<-ggplot(data = deltamapr::WW_Delta) +
  geom_sf(aes(),fill="#7fd6d4", color= "#7fd6d4") +
  geom_point(data = EMP_df_map,
             aes(x = Longitude, y = Latitude, fill= Depth_n_total),color= "black",pch=21, size = 3) +
  coord_sf(xlim = c(-121.2, -122.15), ylim = c(37.66, 38.48), expand = FALSE)+
  theme_void(base_size = 20)+
  geom_text_repel(data = EMP_df_map, aes(x = Longitude, y = Latitude, label = Site_Abbrev), 
                  fontface = "bold", nudge_x = c(), nudge_y = c(), color= "#AD2A1A",
                  bg.color = "white",
                  bg.r = 0.1)+ 
  # guides(fill=guide_legend(title="Legend"))+
  theme(legend.position = c(.2, .85))+
  annotation_scale(location = "bl",text_cex =1, pad_x = unit(0, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x= unit(2.2, "in"),
                         style = ggspatial::north_arrow_orienteering(fill = c("black", "black"),
                                                                     text_size = 15))+
  scale_fill_distiller(palette = "Spectral")

Map_Depth_n
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/Map_Depth_n.pdf",Map_Depth_n, width = 10, height = 7 )

Map_DO_n<-ggplot(data = deltamapr::WW_Delta) +
  geom_sf(aes(),fill="#7fd6d4", color= "#7fd6d4") +
  geom_point(data = EMP_df_map,
             aes(x = Longitude, y = Latitude, fill= DO_n_total),color= "black",pch=21, size = 3) +
  coord_sf(xlim = c(-121.2, -122.15), ylim = c(37.66, 38.48), expand = FALSE)+
  theme_void(base_size = 20)+
  geom_text_repel(data = EMP_df_map, aes(x = Longitude, y = Latitude, label = Site_Abbrev), 
                  fontface = "bold", nudge_x = c(), nudge_y = c(), color= "#AD2A1A",
                  bg.color = "white",
                  bg.r = 0.1)+ 
  # guides(fill=guide_legend(title="Legend"))+
  theme(legend.position = c(.2, .85))+
  annotation_scale(location = "bl",text_cex =1, pad_x = unit(0, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x= unit(2.2, "in"),
                         style = ggspatial::north_arrow_orienteering(fill = c("black", "black"),
                                                                     text_size = 15))+
  scale_fill_distiller(palette = "Spectral")

Map_DO_n
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/Map_DO_n.pdf",Map_DO_n, width = 10, height = 7 )

Map_SpCnd_n<-ggplot(data = deltamapr::WW_Delta) +
  geom_sf(aes(),fill="#7fd6d4", color= "#7fd6d4") +
  geom_point(data = EMP_df_map,
             aes(x = Longitude, y = Latitude, fill= SpCnd_n_total),color= "black",pch=21, size = 3) +
  coord_sf(xlim = c(-121.2, -122.15), ylim = c(37.66, 38.48), expand = FALSE)+
  theme_void(base_size = 20)+
  geom_text_repel(data = EMP_df_map, aes(x = Longitude, y = Latitude, label = Site_Abbrev), 
                  fontface = "bold", nudge_x = c(), nudge_y = c(), color= "#AD2A1A",
                  bg.color = "white",
                  bg.r = 0.1)+ 
  # guides(fill=guide_legend(title="Legend"))+
  theme(legend.position = c(.2, .85))+
  annotation_scale(location = "bl",text_cex =1, pad_x = unit(0, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x= unit(2.2, "in"),
                         style = ggspatial::north_arrow_orienteering(fill = c("black", "black"),
                                                                     text_size = 15))+
  scale_fill_distiller(palette = "Spectral")

Map_SpCnd_n
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/Map_SpCnd_n.pdf",Map_SpCnd_n, width = 10, height = 7 )

Map_pH_n<-ggplot(data = deltamapr::WW_Delta) +
  geom_sf(aes(),fill="#7fd6d4", color= "#7fd6d4") +
  geom_point(data = EMP_df_map,
             aes(x = Longitude, y = Latitude, fill= pH_n_total),color= "black",pch=21, size = 3) +
  coord_sf(xlim = c(-121.2, -122.15), ylim = c(37.66, 38.48), expand = FALSE)+
  theme_void(base_size = 20)+
  geom_text_repel(data = EMP_df_map, aes(x = Longitude, y = Latitude, label = Site_Abbrev), 
                  fontface = "bold", nudge_x = c(), nudge_y = c(), color= "#AD2A1A",
                  bg.color = "white",
                  bg.r = 0.1)+ 
  # guides(fill=guide_legend(title="Legend"))+
  theme(legend.position = c(.2, .85))+
  annotation_scale(location = "bl",text_cex =1, pad_x = unit(0, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x= unit(2.2, "in"),
                         style = ggspatial::north_arrow_orienteering(fill = c("black", "black"),
                                                                     text_size = 15))+
  scale_fill_distiller(palette = "Spectral")

Map_pH_n
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/Map_pH_n.pdf",Map_pH_n, width = 10, height = 7 )

Map_DIN_n<-ggplot(data = deltamapr::WW_Delta) +
  geom_sf(aes(),fill="#7fd6d4", color= "#7fd6d4") +
  geom_point(data = EMP_df_map,
             aes(x = Longitude, y = Latitude, fill= DIN_n_total),color= "black",pch=21, size = 3) +
  coord_sf(xlim = c(-121.2, -122.15), ylim = c(37.66, 38.48), expand = FALSE)+
  theme_void(base_size = 20)+
  geom_text_repel(data = EMP_df_map, aes(x = Longitude, y = Latitude, label = Site_Abbrev), 
                  fontface = "bold", nudge_x = c(), nudge_y = c(), color= "#AD2A1A",
                  bg.color = "white",
                  bg.r = 0.1)+ 
  # guides(fill=guide_legend(title="Legend"))+
  theme(legend.position = c(.2, .85))+
  annotation_scale(location = "bl",text_cex =1, pad_x = unit(0, "in")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x= unit(2.2, "in"),
                         style = ggspatial::north_arrow_orienteering(fill = c("black", "black"),
                                                                     text_size = 15))+
  scale_fill_distiller(palette = "Spectral")

Map_DIN_n
ggsave("/Users/kleathers/Library/CloudStorage/OneDrive-DOI/Shared Documents - BGC Projects (v3)/BGC Proposals and Projects/Active Projects/BOR/Nuts TS Analysis/Phytoplankton_synthesis_project/Figures/Map_DIN_n.pdf",Map_DIN_n, width = 10, height = 7 )




