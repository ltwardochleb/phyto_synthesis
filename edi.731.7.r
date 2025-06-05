# Package ID: edi.731.7 Cataloging System:https://pasta.edirepository.org.
# Data set title: Six decades (1959-2022) of water quality in the upper San Francisco Estuary: an integrated database of 16 discrete monitoring surveys in the Sacramento San Joaquin Delta, Suisun Bay, Suisun Marsh, and San Francisco Bay.
# Data set creator:  Samuel Bashevkin - California State Water Resources Control Board 
# Data set creator:  David Bosworth - California Department of Water Resources 
# Data set creator:  Sarah Perry - California Department of Water Resources 
# Data set creator:  Elizabeth Stumpner - California Department of Water Resources 
# Data set creator:  Rosemary Hartman - California Department of Water Resources 
# Contact:  Samuel Bashevkin -  California State Water Resources Control Board  - sam.bashevkin@waterboards.ca.gov
# Contact:  David Bosworth -  California Department of Water Resources  - David.Bosworth@water.ca.gov
# Stylesheet v2.14 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")
	      

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/731/7/6c5f35b1d316e39c8de0bfadfb3c9692" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Source",     
                    "Station",     
                    "Latitude",     
                    "Longitude",     
                    "Field_coords",     
                    "Date",     
                    "Datetime",     
                    "Depth",     
                    "Sample_depth_surface",     
                    "Sample_depth_nutr_surface",     
                    "Sample_depth_bottom",     
                    "Tide",     
                    "Temperature",     
                    "Temperature_bottom",     
                    "Conductivity",     
                    "Conductivity_bottom",     
                    "Salinity",     
                    "Salinity_bottom",     
                    "Secchi",     
                    "Secchi_estimated",     
                    "TurbidityNTU",     
                    "TurbidityNTU_bottom",     
                    "TurbidityFNU",     
                    "TurbidityFNU_bottom",     
                    "DissolvedOxygen",     
                    "DissolvedOxygen_bottom",     
                    "DissolvedOxygenPercent",     
                    "DissolvedOxygenPercent_bottom",     
                    "pH",     
                    "pH_bottom",     
                    "Microcystis",     
                    "Chlorophyll_Sign",     
                    "Chlorophyll",     
                    "Pheophytin_Sign",     
                    "Pheophytin",     
                    "TotAmmonia_Sign",     
                    "TotAmmonia",     
                    "DissAmmonia_Sign",     
                    "DissAmmonia",     
                    "DissNitrateNitrite_Sign",     
                    "DissNitrateNitrite",     
                    "TotPhos_Sign",     
                    "TotPhos",     
                    "DissOrthophos_Sign",     
                    "DissOrthophos",     
                    "TON_Sign",     
                    "TON",     
                    "DON_Sign",     
                    "DON",     
                    "TKN_Sign",     
                    "TKN",     
                    "TotAlkalinity_Sign",     
                    "TotAlkalinity",     
                    "DissBromide_Sign",     
                    "DissBromide",     
                    "DissCalcium_Sign",     
                    "DissCalcium",     
                    "TotChloride_Sign",     
                    "TotChloride",     
                    "DissChloride_Sign",     
                    "DissChloride",     
                    "DissSilica_Sign",     
                    "DissSilica",     
                    "TOC_Sign",     
                    "TOC",     
                    "DOC_Sign",     
                    "DOC",     
                    "TDS_Sign",     
                    "TDS",     
                    "TSS_Sign",     
                    "TSS",     
                    "VSS_Sign",     
                    "VSS",     
                    "Notes"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$Source)!="factor") dt1$Source<- as.factor(dt1$Source)
if (class(dt1$Station)!="factor") dt1$Station<- as.factor(dt1$Station)
if (class(dt1$Latitude)=="factor") dt1$Latitude <-as.numeric(levels(dt1$Latitude))[as.integer(dt1$Latitude) ]               
if (class(dt1$Latitude)=="character") dt1$Latitude <-as.numeric(dt1$Latitude)
if (class(dt1$Longitude)=="factor") dt1$Longitude <-as.numeric(levels(dt1$Longitude))[as.integer(dt1$Longitude) ]               
if (class(dt1$Longitude)=="character") dt1$Longitude <-as.numeric(dt1$Longitude)
if (class(dt1$Field_coords)!="factor") dt1$Field_coords<- as.factor(dt1$Field_coords)                                   
# attempting to convert dt1$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1Date<-as.Date(dt1$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$Date != "",]) == length(tmp1Date[!is.na(tmp1Date)])){dt1$Date <- tmp1Date } else {print("Date conversion failed for dt1$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
                                                                   
# attempting to convert dt1$Datetime dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp1Datetime<-as.POSIXct(dt1$Datetime,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$Datetime != "",]) == length(tmp1Datetime[!is.na(tmp1Datetime)])){dt1$Datetime <- tmp1Datetime } else {print("Date conversion failed for dt1$Datetime. Please inspect the data and do the date conversion yourself.")}                                                                    
                                
if (class(dt1$Depth)=="factor") dt1$Depth <-as.numeric(levels(dt1$Depth))[as.integer(dt1$Depth) ]               
if (class(dt1$Depth)=="character") dt1$Depth <-as.numeric(dt1$Depth)
if (class(dt1$Sample_depth_surface)=="factor") dt1$Sample_depth_surface <-as.numeric(levels(dt1$Sample_depth_surface))[as.integer(dt1$Sample_depth_surface) ]               
if (class(dt1$Sample_depth_surface)=="character") dt1$Sample_depth_surface <-as.numeric(dt1$Sample_depth_surface)
if (class(dt1$Sample_depth_nutr_surface)=="factor") dt1$Sample_depth_nutr_surface <-as.numeric(levels(dt1$Sample_depth_nutr_surface))[as.integer(dt1$Sample_depth_nutr_surface) ]               
if (class(dt1$Sample_depth_nutr_surface)=="character") dt1$Sample_depth_nutr_surface <-as.numeric(dt1$Sample_depth_nutr_surface)
if (class(dt1$Sample_depth_bottom)=="factor") dt1$Sample_depth_bottom <-as.numeric(levels(dt1$Sample_depth_bottom))[as.integer(dt1$Sample_depth_bottom) ]               
if (class(dt1$Sample_depth_bottom)=="character") dt1$Sample_depth_bottom <-as.numeric(dt1$Sample_depth_bottom)
if (class(dt1$Tide)!="factor") dt1$Tide<- as.factor(dt1$Tide)
if (class(dt1$Temperature)=="factor") dt1$Temperature <-as.numeric(levels(dt1$Temperature))[as.integer(dt1$Temperature) ]               
if (class(dt1$Temperature)=="character") dt1$Temperature <-as.numeric(dt1$Temperature)
if (class(dt1$Temperature_bottom)=="factor") dt1$Temperature_bottom <-as.numeric(levels(dt1$Temperature_bottom))[as.integer(dt1$Temperature_bottom) ]               
if (class(dt1$Temperature_bottom)=="character") dt1$Temperature_bottom <-as.numeric(dt1$Temperature_bottom)
if (class(dt1$Conductivity)=="factor") dt1$Conductivity <-as.numeric(levels(dt1$Conductivity))[as.integer(dt1$Conductivity) ]               
if (class(dt1$Conductivity)=="character") dt1$Conductivity <-as.numeric(dt1$Conductivity)
if (class(dt1$Conductivity_bottom)=="factor") dt1$Conductivity_bottom <-as.numeric(levels(dt1$Conductivity_bottom))[as.integer(dt1$Conductivity_bottom) ]               
if (class(dt1$Conductivity_bottom)=="character") dt1$Conductivity_bottom <-as.numeric(dt1$Conductivity_bottom)
if (class(dt1$Salinity)=="factor") dt1$Salinity <-as.numeric(levels(dt1$Salinity))[as.integer(dt1$Salinity) ]               
if (class(dt1$Salinity)=="character") dt1$Salinity <-as.numeric(dt1$Salinity)
if (class(dt1$Salinity_bottom)=="factor") dt1$Salinity_bottom <-as.numeric(levels(dt1$Salinity_bottom))[as.integer(dt1$Salinity_bottom) ]               
if (class(dt1$Salinity_bottom)=="character") dt1$Salinity_bottom <-as.numeric(dt1$Salinity_bottom)
if (class(dt1$Secchi)=="factor") dt1$Secchi <-as.numeric(levels(dt1$Secchi))[as.integer(dt1$Secchi) ]               
if (class(dt1$Secchi)=="character") dt1$Secchi <-as.numeric(dt1$Secchi)
if (class(dt1$Secchi_estimated)!="factor") dt1$Secchi_estimated<- as.factor(dt1$Secchi_estimated)
if (class(dt1$TurbidityNTU)=="factor") dt1$TurbidityNTU <-as.numeric(levels(dt1$TurbidityNTU))[as.integer(dt1$TurbidityNTU) ]               
if (class(dt1$TurbidityNTU)=="character") dt1$TurbidityNTU <-as.numeric(dt1$TurbidityNTU)
if (class(dt1$TurbidityNTU_bottom)=="factor") dt1$TurbidityNTU_bottom <-as.numeric(levels(dt1$TurbidityNTU_bottom))[as.integer(dt1$TurbidityNTU_bottom) ]               
if (class(dt1$TurbidityNTU_bottom)=="character") dt1$TurbidityNTU_bottom <-as.numeric(dt1$TurbidityNTU_bottom)
if (class(dt1$TurbidityFNU)=="factor") dt1$TurbidityFNU <-as.numeric(levels(dt1$TurbidityFNU))[as.integer(dt1$TurbidityFNU) ]               
if (class(dt1$TurbidityFNU)=="character") dt1$TurbidityFNU <-as.numeric(dt1$TurbidityFNU)
if (class(dt1$TurbidityFNU_bottom)=="factor") dt1$TurbidityFNU_bottom <-as.numeric(levels(dt1$TurbidityFNU_bottom))[as.integer(dt1$TurbidityFNU_bottom) ]               
if (class(dt1$TurbidityFNU_bottom)=="character") dt1$TurbidityFNU_bottom <-as.numeric(dt1$TurbidityFNU_bottom)
if (class(dt1$DissolvedOxygen)=="factor") dt1$DissolvedOxygen <-as.numeric(levels(dt1$DissolvedOxygen))[as.integer(dt1$DissolvedOxygen) ]               
if (class(dt1$DissolvedOxygen)=="character") dt1$DissolvedOxygen <-as.numeric(dt1$DissolvedOxygen)
if (class(dt1$DissolvedOxygen_bottom)=="factor") dt1$DissolvedOxygen_bottom <-as.numeric(levels(dt1$DissolvedOxygen_bottom))[as.integer(dt1$DissolvedOxygen_bottom) ]               
if (class(dt1$DissolvedOxygen_bottom)=="character") dt1$DissolvedOxygen_bottom <-as.numeric(dt1$DissolvedOxygen_bottom)
if (class(dt1$DissolvedOxygenPercent)=="factor") dt1$DissolvedOxygenPercent <-as.numeric(levels(dt1$DissolvedOxygenPercent))[as.integer(dt1$DissolvedOxygenPercent) ]               
if (class(dt1$DissolvedOxygenPercent)=="character") dt1$DissolvedOxygenPercent <-as.numeric(dt1$DissolvedOxygenPercent)
if (class(dt1$DissolvedOxygenPercent_bottom)=="factor") dt1$DissolvedOxygenPercent_bottom <-as.numeric(levels(dt1$DissolvedOxygenPercent_bottom))[as.integer(dt1$DissolvedOxygenPercent_bottom) ]               
if (class(dt1$DissolvedOxygenPercent_bottom)=="character") dt1$DissolvedOxygenPercent_bottom <-as.numeric(dt1$DissolvedOxygenPercent_bottom)
if (class(dt1$pH)=="factor") dt1$pH <-as.numeric(levels(dt1$pH))[as.integer(dt1$pH) ]               
if (class(dt1$pH)=="character") dt1$pH <-as.numeric(dt1$pH)
if (class(dt1$pH_bottom)=="factor") dt1$pH_bottom <-as.numeric(levels(dt1$pH_bottom))[as.integer(dt1$pH_bottom) ]               
if (class(dt1$pH_bottom)=="character") dt1$pH_bottom <-as.numeric(dt1$pH_bottom)
if (class(dt1$Microcystis)!="factor") dt1$Microcystis<- as.factor(dt1$Microcystis)
if (class(dt1$Chlorophyll_Sign)!="factor") dt1$Chlorophyll_Sign<- as.factor(dt1$Chlorophyll_Sign)
if (class(dt1$Chlorophyll)=="factor") dt1$Chlorophyll <-as.numeric(levels(dt1$Chlorophyll))[as.integer(dt1$Chlorophyll) ]               
if (class(dt1$Chlorophyll)=="character") dt1$Chlorophyll <-as.numeric(dt1$Chlorophyll)
if (class(dt1$Pheophytin_Sign)!="factor") dt1$Pheophytin_Sign<- as.factor(dt1$Pheophytin_Sign)
if (class(dt1$Pheophytin)=="factor") dt1$Pheophytin <-as.numeric(levels(dt1$Pheophytin))[as.integer(dt1$Pheophytin) ]               
if (class(dt1$Pheophytin)=="character") dt1$Pheophytin <-as.numeric(dt1$Pheophytin)
if (class(dt1$TotAmmonia_Sign)!="factor") dt1$TotAmmonia_Sign<- as.factor(dt1$TotAmmonia_Sign)
if (class(dt1$TotAmmonia)=="factor") dt1$TotAmmonia <-as.numeric(levels(dt1$TotAmmonia))[as.integer(dt1$TotAmmonia) ]               
if (class(dt1$TotAmmonia)=="character") dt1$TotAmmonia <-as.numeric(dt1$TotAmmonia)
if (class(dt1$DissAmmonia_Sign)!="factor") dt1$DissAmmonia_Sign<- as.factor(dt1$DissAmmonia_Sign)
if (class(dt1$DissAmmonia)=="factor") dt1$DissAmmonia <-as.numeric(levels(dt1$DissAmmonia))[as.integer(dt1$DissAmmonia) ]               
if (class(dt1$DissAmmonia)=="character") dt1$DissAmmonia <-as.numeric(dt1$DissAmmonia)
if (class(dt1$DissNitrateNitrite_Sign)!="factor") dt1$DissNitrateNitrite_Sign<- as.factor(dt1$DissNitrateNitrite_Sign)
if (class(dt1$DissNitrateNitrite)=="factor") dt1$DissNitrateNitrite <-as.numeric(levels(dt1$DissNitrateNitrite))[as.integer(dt1$DissNitrateNitrite) ]               
if (class(dt1$DissNitrateNitrite)=="character") dt1$DissNitrateNitrite <-as.numeric(dt1$DissNitrateNitrite)
if (class(dt1$TotPhos_Sign)!="factor") dt1$TotPhos_Sign<- as.factor(dt1$TotPhos_Sign)
if (class(dt1$TotPhos)=="factor") dt1$TotPhos <-as.numeric(levels(dt1$TotPhos))[as.integer(dt1$TotPhos) ]               
if (class(dt1$TotPhos)=="character") dt1$TotPhos <-as.numeric(dt1$TotPhos)
if (class(dt1$DissOrthophos_Sign)!="factor") dt1$DissOrthophos_Sign<- as.factor(dt1$DissOrthophos_Sign)
if (class(dt1$DissOrthophos)=="factor") dt1$DissOrthophos <-as.numeric(levels(dt1$DissOrthophos))[as.integer(dt1$DissOrthophos) ]               
if (class(dt1$DissOrthophos)=="character") dt1$DissOrthophos <-as.numeric(dt1$DissOrthophos)
if (class(dt1$TON_Sign)!="factor") dt1$TON_Sign<- as.factor(dt1$TON_Sign)
if (class(dt1$TON)=="factor") dt1$TON <-as.numeric(levels(dt1$TON))[as.integer(dt1$TON) ]               
if (class(dt1$TON)=="character") dt1$TON <-as.numeric(dt1$TON)
if (class(dt1$DON_Sign)!="factor") dt1$DON_Sign<- as.factor(dt1$DON_Sign)
if (class(dt1$DON)=="factor") dt1$DON <-as.numeric(levels(dt1$DON))[as.integer(dt1$DON) ]               
if (class(dt1$DON)=="character") dt1$DON <-as.numeric(dt1$DON)
if (class(dt1$TKN_Sign)!="factor") dt1$TKN_Sign<- as.factor(dt1$TKN_Sign)
if (class(dt1$TKN)=="factor") dt1$TKN <-as.numeric(levels(dt1$TKN))[as.integer(dt1$TKN) ]               
if (class(dt1$TKN)=="character") dt1$TKN <-as.numeric(dt1$TKN)
if (class(dt1$TotAlkalinity_Sign)!="factor") dt1$TotAlkalinity_Sign<- as.factor(dt1$TotAlkalinity_Sign)
if (class(dt1$TotAlkalinity)=="factor") dt1$TotAlkalinity <-as.numeric(levels(dt1$TotAlkalinity))[as.integer(dt1$TotAlkalinity) ]               
if (class(dt1$TotAlkalinity)=="character") dt1$TotAlkalinity <-as.numeric(dt1$TotAlkalinity)
if (class(dt1$DissBromide_Sign)!="factor") dt1$DissBromide_Sign<- as.factor(dt1$DissBromide_Sign)
if (class(dt1$DissBromide)=="factor") dt1$DissBromide <-as.numeric(levels(dt1$DissBromide))[as.integer(dt1$DissBromide) ]               
if (class(dt1$DissBromide)=="character") dt1$DissBromide <-as.numeric(dt1$DissBromide)
if (class(dt1$DissCalcium_Sign)!="factor") dt1$DissCalcium_Sign<- as.factor(dt1$DissCalcium_Sign)
if (class(dt1$DissCalcium)=="factor") dt1$DissCalcium <-as.numeric(levels(dt1$DissCalcium))[as.integer(dt1$DissCalcium) ]               
if (class(dt1$DissCalcium)=="character") dt1$DissCalcium <-as.numeric(dt1$DissCalcium)
if (class(dt1$TotChloride_Sign)!="factor") dt1$TotChloride_Sign<- as.factor(dt1$TotChloride_Sign)
if (class(dt1$TotChloride)=="factor") dt1$TotChloride <-as.numeric(levels(dt1$TotChloride))[as.integer(dt1$TotChloride) ]               
if (class(dt1$TotChloride)=="character") dt1$TotChloride <-as.numeric(dt1$TotChloride)
if (class(dt1$DissChloride_Sign)!="factor") dt1$DissChloride_Sign<- as.factor(dt1$DissChloride_Sign)
if (class(dt1$DissChloride)=="factor") dt1$DissChloride <-as.numeric(levels(dt1$DissChloride))[as.integer(dt1$DissChloride) ]               
if (class(dt1$DissChloride)=="character") dt1$DissChloride <-as.numeric(dt1$DissChloride)
if (class(dt1$DissSilica_Sign)!="factor") dt1$DissSilica_Sign<- as.factor(dt1$DissSilica_Sign)
if (class(dt1$DissSilica)=="factor") dt1$DissSilica <-as.numeric(levels(dt1$DissSilica))[as.integer(dt1$DissSilica) ]               
if (class(dt1$DissSilica)=="character") dt1$DissSilica <-as.numeric(dt1$DissSilica)
if (class(dt1$TOC_Sign)!="factor") dt1$TOC_Sign<- as.factor(dt1$TOC_Sign)
if (class(dt1$TOC)=="factor") dt1$TOC <-as.numeric(levels(dt1$TOC))[as.integer(dt1$TOC) ]               
if (class(dt1$TOC)=="character") dt1$TOC <-as.numeric(dt1$TOC)
if (class(dt1$DOC_Sign)!="factor") dt1$DOC_Sign<- as.factor(dt1$DOC_Sign)
if (class(dt1$DOC)=="factor") dt1$DOC <-as.numeric(levels(dt1$DOC))[as.integer(dt1$DOC) ]               
if (class(dt1$DOC)=="character") dt1$DOC <-as.numeric(dt1$DOC)
if (class(dt1$TDS_Sign)!="factor") dt1$TDS_Sign<- as.factor(dt1$TDS_Sign)
if (class(dt1$TDS)=="factor") dt1$TDS <-as.numeric(levels(dt1$TDS))[as.integer(dt1$TDS) ]               
if (class(dt1$TDS)=="character") dt1$TDS <-as.numeric(dt1$TDS)
if (class(dt1$TSS_Sign)!="factor") dt1$TSS_Sign<- as.factor(dt1$TSS_Sign)
if (class(dt1$TSS)=="factor") dt1$TSS <-as.numeric(levels(dt1$TSS))[as.integer(dt1$TSS) ]               
if (class(dt1$TSS)=="character") dt1$TSS <-as.numeric(dt1$TSS)
if (class(dt1$VSS_Sign)!="factor") dt1$VSS_Sign<- as.factor(dt1$VSS_Sign)
if (class(dt1$VSS)=="factor") dt1$VSS <-as.numeric(levels(dt1$VSS))[as.integer(dt1$VSS) ]               
if (class(dt1$VSS)=="character") dt1$VSS <-as.numeric(dt1$VSS)
if (class(dt1$Notes)!="factor") dt1$Notes<- as.factor(dt1$Notes)
                
# Convert Missing Values to NA for non-dates
                
dt1$Latitude <- ifelse((trimws(as.character(dt1$Latitude))==trimws("NA")),NA,dt1$Latitude)               
suppressWarnings(dt1$Latitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Latitude))==as.character(as.numeric("NA"))),NA,dt1$Latitude))
dt1$Longitude <- ifelse((trimws(as.character(dt1$Longitude))==trimws("NA")),NA,dt1$Longitude)               
suppressWarnings(dt1$Longitude <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Longitude))==as.character(as.numeric("NA"))),NA,dt1$Longitude))
dt1$Depth <- ifelse((trimws(as.character(dt1$Depth))==trimws("NA")),NA,dt1$Depth)               
suppressWarnings(dt1$Depth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Depth))==as.character(as.numeric("NA"))),NA,dt1$Depth))
dt1$Sample_depth_surface <- ifelse((trimws(as.character(dt1$Sample_depth_surface))==trimws("NA")),NA,dt1$Sample_depth_surface)               
suppressWarnings(dt1$Sample_depth_surface <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Sample_depth_surface))==as.character(as.numeric("NA"))),NA,dt1$Sample_depth_surface))
dt1$Sample_depth_nutr_surface <- ifelse((trimws(as.character(dt1$Sample_depth_nutr_surface))==trimws("NA")),NA,dt1$Sample_depth_nutr_surface)               
suppressWarnings(dt1$Sample_depth_nutr_surface <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Sample_depth_nutr_surface))==as.character(as.numeric("NA"))),NA,dt1$Sample_depth_nutr_surface))
dt1$Sample_depth_bottom <- ifelse((trimws(as.character(dt1$Sample_depth_bottom))==trimws("NA")),NA,dt1$Sample_depth_bottom)               
suppressWarnings(dt1$Sample_depth_bottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Sample_depth_bottom))==as.character(as.numeric("NA"))),NA,dt1$Sample_depth_bottom))
dt1$Tide <- as.factor(ifelse((trimws(as.character(dt1$Tide))==trimws("NA")),NA,as.character(dt1$Tide)))
dt1$Temperature <- ifelse((trimws(as.character(dt1$Temperature))==trimws("NA")),NA,dt1$Temperature)               
suppressWarnings(dt1$Temperature <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Temperature))==as.character(as.numeric("NA"))),NA,dt1$Temperature))
dt1$Temperature_bottom <- ifelse((trimws(as.character(dt1$Temperature_bottom))==trimws("NA")),NA,dt1$Temperature_bottom)               
suppressWarnings(dt1$Temperature_bottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Temperature_bottom))==as.character(as.numeric("NA"))),NA,dt1$Temperature_bottom))
dt1$Conductivity <- ifelse((trimws(as.character(dt1$Conductivity))==trimws("NA")),NA,dt1$Conductivity)               
suppressWarnings(dt1$Conductivity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Conductivity))==as.character(as.numeric("NA"))),NA,dt1$Conductivity))
dt1$Conductivity_bottom <- ifelse((trimws(as.character(dt1$Conductivity_bottom))==trimws("NA")),NA,dt1$Conductivity_bottom)               
suppressWarnings(dt1$Conductivity_bottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Conductivity_bottom))==as.character(as.numeric("NA"))),NA,dt1$Conductivity_bottom))
dt1$Salinity <- ifelse((trimws(as.character(dt1$Salinity))==trimws("NA")),NA,dt1$Salinity)               
suppressWarnings(dt1$Salinity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Salinity))==as.character(as.numeric("NA"))),NA,dt1$Salinity))
dt1$Salinity_bottom <- ifelse((trimws(as.character(dt1$Salinity_bottom))==trimws("NA")),NA,dt1$Salinity_bottom)               
suppressWarnings(dt1$Salinity_bottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Salinity_bottom))==as.character(as.numeric("NA"))),NA,dt1$Salinity_bottom))
dt1$Secchi <- ifelse((trimws(as.character(dt1$Secchi))==trimws("NA")),NA,dt1$Secchi)               
suppressWarnings(dt1$Secchi <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Secchi))==as.character(as.numeric("NA"))),NA,dt1$Secchi))
dt1$Secchi_estimated <- as.factor(ifelse((trimws(as.character(dt1$Secchi_estimated))==trimws("NA")),NA,as.character(dt1$Secchi_estimated)))
dt1$TurbidityNTU <- ifelse((trimws(as.character(dt1$TurbidityNTU))==trimws("NA")),NA,dt1$TurbidityNTU)               
suppressWarnings(dt1$TurbidityNTU <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TurbidityNTU))==as.character(as.numeric("NA"))),NA,dt1$TurbidityNTU))
dt1$TurbidityNTU_bottom <- ifelse((trimws(as.character(dt1$TurbidityNTU_bottom))==trimws("NA")),NA,dt1$TurbidityNTU_bottom)               
suppressWarnings(dt1$TurbidityNTU_bottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TurbidityNTU_bottom))==as.character(as.numeric("NA"))),NA,dt1$TurbidityNTU_bottom))
dt1$TurbidityFNU <- ifelse((trimws(as.character(dt1$TurbidityFNU))==trimws("NA")),NA,dt1$TurbidityFNU)               
suppressWarnings(dt1$TurbidityFNU <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TurbidityFNU))==as.character(as.numeric("NA"))),NA,dt1$TurbidityFNU))
dt1$TurbidityFNU_bottom <- ifelse((trimws(as.character(dt1$TurbidityFNU_bottom))==trimws("NA")),NA,dt1$TurbidityFNU_bottom)               
suppressWarnings(dt1$TurbidityFNU_bottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TurbidityFNU_bottom))==as.character(as.numeric("NA"))),NA,dt1$TurbidityFNU_bottom))
dt1$DissolvedOxygen <- ifelse((trimws(as.character(dt1$DissolvedOxygen))==trimws("NA")),NA,dt1$DissolvedOxygen)               
suppressWarnings(dt1$DissolvedOxygen <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissolvedOxygen))==as.character(as.numeric("NA"))),NA,dt1$DissolvedOxygen))
dt1$DissolvedOxygen_bottom <- ifelse((trimws(as.character(dt1$DissolvedOxygen_bottom))==trimws("NA")),NA,dt1$DissolvedOxygen_bottom)               
suppressWarnings(dt1$DissolvedOxygen_bottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissolvedOxygen_bottom))==as.character(as.numeric("NA"))),NA,dt1$DissolvedOxygen_bottom))
dt1$DissolvedOxygenPercent <- ifelse((trimws(as.character(dt1$DissolvedOxygenPercent))==trimws("NA")),NA,dt1$DissolvedOxygenPercent)               
suppressWarnings(dt1$DissolvedOxygenPercent <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissolvedOxygenPercent))==as.character(as.numeric("NA"))),NA,dt1$DissolvedOxygenPercent))
dt1$DissolvedOxygenPercent_bottom <- ifelse((trimws(as.character(dt1$DissolvedOxygenPercent_bottom))==trimws("NA")),NA,dt1$DissolvedOxygenPercent_bottom)               
suppressWarnings(dt1$DissolvedOxygenPercent_bottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissolvedOxygenPercent_bottom))==as.character(as.numeric("NA"))),NA,dt1$DissolvedOxygenPercent_bottom))
dt1$pH <- ifelse((trimws(as.character(dt1$pH))==trimws("NA")),NA,dt1$pH)               
suppressWarnings(dt1$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$pH))==as.character(as.numeric("NA"))),NA,dt1$pH))
dt1$pH_bottom <- ifelse((trimws(as.character(dt1$pH_bottom))==trimws("NA")),NA,dt1$pH_bottom)               
suppressWarnings(dt1$pH_bottom <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$pH_bottom))==as.character(as.numeric("NA"))),NA,dt1$pH_bottom))
dt1$Microcystis <- as.factor(ifelse((trimws(as.character(dt1$Microcystis))==trimws("NA")),NA,as.character(dt1$Microcystis)))
dt1$Chlorophyll_Sign <- as.factor(ifelse((trimws(as.character(dt1$Chlorophyll_Sign))==trimws("NA")),NA,as.character(dt1$Chlorophyll_Sign)))
dt1$Chlorophyll <- ifelse((trimws(as.character(dt1$Chlorophyll))==trimws("NA")),NA,dt1$Chlorophyll)               
suppressWarnings(dt1$Chlorophyll <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Chlorophyll))==as.character(as.numeric("NA"))),NA,dt1$Chlorophyll))
dt1$Pheophytin_Sign <- as.factor(ifelse((trimws(as.character(dt1$Pheophytin_Sign))==trimws("NA")),NA,as.character(dt1$Pheophytin_Sign)))
dt1$Pheophytin <- ifelse((trimws(as.character(dt1$Pheophytin))==trimws("NA")),NA,dt1$Pheophytin)               
suppressWarnings(dt1$Pheophytin <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Pheophytin))==as.character(as.numeric("NA"))),NA,dt1$Pheophytin))
dt1$TotAmmonia_Sign <- as.factor(ifelse((trimws(as.character(dt1$TotAmmonia_Sign))==trimws("NA")),NA,as.character(dt1$TotAmmonia_Sign)))
dt1$TotAmmonia <- ifelse((trimws(as.character(dt1$TotAmmonia))==trimws("NA")),NA,dt1$TotAmmonia)               
suppressWarnings(dt1$TotAmmonia <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TotAmmonia))==as.character(as.numeric("NA"))),NA,dt1$TotAmmonia))
dt1$DissAmmonia_Sign <- as.factor(ifelse((trimws(as.character(dt1$DissAmmonia_Sign))==trimws("NA")),NA,as.character(dt1$DissAmmonia_Sign)))
dt1$DissAmmonia <- ifelse((trimws(as.character(dt1$DissAmmonia))==trimws("NA")),NA,dt1$DissAmmonia)               
suppressWarnings(dt1$DissAmmonia <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissAmmonia))==as.character(as.numeric("NA"))),NA,dt1$DissAmmonia))
dt1$DissNitrateNitrite_Sign <- as.factor(ifelse((trimws(as.character(dt1$DissNitrateNitrite_Sign))==trimws("NA")),NA,as.character(dt1$DissNitrateNitrite_Sign)))
dt1$DissNitrateNitrite <- ifelse((trimws(as.character(dt1$DissNitrateNitrite))==trimws("NA")),NA,dt1$DissNitrateNitrite)               
suppressWarnings(dt1$DissNitrateNitrite <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissNitrateNitrite))==as.character(as.numeric("NA"))),NA,dt1$DissNitrateNitrite))
dt1$TotPhos_Sign <- as.factor(ifelse((trimws(as.character(dt1$TotPhos_Sign))==trimws("NA")),NA,as.character(dt1$TotPhos_Sign)))
dt1$TotPhos <- ifelse((trimws(as.character(dt1$TotPhos))==trimws("NA")),NA,dt1$TotPhos)               
suppressWarnings(dt1$TotPhos <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TotPhos))==as.character(as.numeric("NA"))),NA,dt1$TotPhos))
dt1$DissOrthophos_Sign <- as.factor(ifelse((trimws(as.character(dt1$DissOrthophos_Sign))==trimws("NA")),NA,as.character(dt1$DissOrthophos_Sign)))
dt1$DissOrthophos <- ifelse((trimws(as.character(dt1$DissOrthophos))==trimws("NA")),NA,dt1$DissOrthophos)               
suppressWarnings(dt1$DissOrthophos <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissOrthophos))==as.character(as.numeric("NA"))),NA,dt1$DissOrthophos))
dt1$TON_Sign <- as.factor(ifelse((trimws(as.character(dt1$TON_Sign))==trimws("NA")),NA,as.character(dt1$TON_Sign)))
dt1$TON <- ifelse((trimws(as.character(dt1$TON))==trimws("NA")),NA,dt1$TON)               
suppressWarnings(dt1$TON <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TON))==as.character(as.numeric("NA"))),NA,dt1$TON))
dt1$DON_Sign <- as.factor(ifelse((trimws(as.character(dt1$DON_Sign))==trimws("NA")),NA,as.character(dt1$DON_Sign)))
dt1$DON <- ifelse((trimws(as.character(dt1$DON))==trimws("NA")),NA,dt1$DON)               
suppressWarnings(dt1$DON <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DON))==as.character(as.numeric("NA"))),NA,dt1$DON))
dt1$TKN_Sign <- as.factor(ifelse((trimws(as.character(dt1$TKN_Sign))==trimws("NA")),NA,as.character(dt1$TKN_Sign)))
dt1$TKN <- ifelse((trimws(as.character(dt1$TKN))==trimws("NA")),NA,dt1$TKN)               
suppressWarnings(dt1$TKN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TKN))==as.character(as.numeric("NA"))),NA,dt1$TKN))
dt1$TotAlkalinity_Sign <- as.factor(ifelse((trimws(as.character(dt1$TotAlkalinity_Sign))==trimws("NA")),NA,as.character(dt1$TotAlkalinity_Sign)))
dt1$TotAlkalinity <- ifelse((trimws(as.character(dt1$TotAlkalinity))==trimws("NA")),NA,dt1$TotAlkalinity)               
suppressWarnings(dt1$TotAlkalinity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TotAlkalinity))==as.character(as.numeric("NA"))),NA,dt1$TotAlkalinity))
dt1$DissBromide_Sign <- as.factor(ifelse((trimws(as.character(dt1$DissBromide_Sign))==trimws("NA")),NA,as.character(dt1$DissBromide_Sign)))
dt1$DissBromide <- ifelse((trimws(as.character(dt1$DissBromide))==trimws("NA")),NA,dt1$DissBromide)               
suppressWarnings(dt1$DissBromide <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissBromide))==as.character(as.numeric("NA"))),NA,dt1$DissBromide))
dt1$DissCalcium_Sign <- as.factor(ifelse((trimws(as.character(dt1$DissCalcium_Sign))==trimws("NA")),NA,as.character(dt1$DissCalcium_Sign)))
dt1$DissCalcium <- ifelse((trimws(as.character(dt1$DissCalcium))==trimws("NA")),NA,dt1$DissCalcium)               
suppressWarnings(dt1$DissCalcium <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissCalcium))==as.character(as.numeric("NA"))),NA,dt1$DissCalcium))
dt1$TotChloride_Sign <- as.factor(ifelse((trimws(as.character(dt1$TotChloride_Sign))==trimws("NA")),NA,as.character(dt1$TotChloride_Sign)))
dt1$TotChloride <- ifelse((trimws(as.character(dt1$TotChloride))==trimws("NA")),NA,dt1$TotChloride)               
suppressWarnings(dt1$TotChloride <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TotChloride))==as.character(as.numeric("NA"))),NA,dt1$TotChloride))
dt1$DissChloride_Sign <- as.factor(ifelse((trimws(as.character(dt1$DissChloride_Sign))==trimws("NA")),NA,as.character(dt1$DissChloride_Sign)))
dt1$DissChloride <- ifelse((trimws(as.character(dt1$DissChloride))==trimws("NA")),NA,dt1$DissChloride)               
suppressWarnings(dt1$DissChloride <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissChloride))==as.character(as.numeric("NA"))),NA,dt1$DissChloride))
dt1$DissSilica_Sign <- as.factor(ifelse((trimws(as.character(dt1$DissSilica_Sign))==trimws("NA")),NA,as.character(dt1$DissSilica_Sign)))
dt1$DissSilica <- ifelse((trimws(as.character(dt1$DissSilica))==trimws("NA")),NA,dt1$DissSilica)               
suppressWarnings(dt1$DissSilica <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DissSilica))==as.character(as.numeric("NA"))),NA,dt1$DissSilica))
dt1$TOC_Sign <- as.factor(ifelse((trimws(as.character(dt1$TOC_Sign))==trimws("NA")),NA,as.character(dt1$TOC_Sign)))
dt1$TOC <- ifelse((trimws(as.character(dt1$TOC))==trimws("NA")),NA,dt1$TOC)               
suppressWarnings(dt1$TOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TOC))==as.character(as.numeric("NA"))),NA,dt1$TOC))
dt1$DOC_Sign <- as.factor(ifelse((trimws(as.character(dt1$DOC_Sign))==trimws("NA")),NA,as.character(dt1$DOC_Sign)))
dt1$DOC <- ifelse((trimws(as.character(dt1$DOC))==trimws("NA")),NA,dt1$DOC)               
suppressWarnings(dt1$DOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$DOC))==as.character(as.numeric("NA"))),NA,dt1$DOC))
dt1$TDS_Sign <- as.factor(ifelse((trimws(as.character(dt1$TDS_Sign))==trimws("NA")),NA,as.character(dt1$TDS_Sign)))
dt1$TDS <- ifelse((trimws(as.character(dt1$TDS))==trimws("NA")),NA,dt1$TDS)               
suppressWarnings(dt1$TDS <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TDS))==as.character(as.numeric("NA"))),NA,dt1$TDS))
dt1$TSS_Sign <- as.factor(ifelse((trimws(as.character(dt1$TSS_Sign))==trimws("NA")),NA,as.character(dt1$TSS_Sign)))
dt1$TSS <- ifelse((trimws(as.character(dt1$TSS))==trimws("NA")),NA,dt1$TSS)               
suppressWarnings(dt1$TSS <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TSS))==as.character(as.numeric("NA"))),NA,dt1$TSS))
dt1$VSS_Sign <- as.factor(ifelse((trimws(as.character(dt1$VSS_Sign))==trimws("NA")),NA,as.character(dt1$VSS_Sign)))
dt1$VSS <- ifelse((trimws(as.character(dt1$VSS))==trimws("NA")),NA,dt1$VSS)               
suppressWarnings(dt1$VSS <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$VSS))==as.character(as.numeric("NA"))),NA,dt1$VSS))
dt1$Notes <- as.factor(ifelse((trimws(as.character(dt1$Notes))==trimws("NA")),NA,as.character(dt1$Notes)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Source)
summary(Station)
summary(Latitude)
summary(Longitude)
summary(Field_coords)
summary(Date)
summary(Datetime)
summary(Depth)
summary(Sample_depth_surface)
summary(Sample_depth_nutr_surface)
summary(Sample_depth_bottom)
summary(Tide)
summary(Temperature)
summary(Temperature_bottom)
summary(Conductivity)
summary(Conductivity_bottom)
summary(Salinity)
summary(Salinity_bottom)
summary(Secchi)
summary(Secchi_estimated)
summary(TurbidityNTU)
summary(TurbidityNTU_bottom)
summary(TurbidityFNU)
summary(TurbidityFNU_bottom)
summary(DissolvedOxygen)
summary(DissolvedOxygen_bottom)
summary(DissolvedOxygenPercent)
summary(DissolvedOxygenPercent_bottom)
summary(pH)
summary(pH_bottom)
summary(Microcystis)
summary(Chlorophyll_Sign)
summary(Chlorophyll)
summary(Pheophytin_Sign)
summary(Pheophytin)
summary(TotAmmonia_Sign)
summary(TotAmmonia)
summary(DissAmmonia_Sign)
summary(DissAmmonia)
summary(DissNitrateNitrite_Sign)
summary(DissNitrateNitrite)
summary(TotPhos_Sign)
summary(TotPhos)
summary(DissOrthophos_Sign)
summary(DissOrthophos)
summary(TON_Sign)
summary(TON)
summary(DON_Sign)
summary(DON)
summary(TKN_Sign)
summary(TKN)
summary(TotAlkalinity_Sign)
summary(TotAlkalinity)
summary(DissBromide_Sign)
summary(DissBromide)
summary(DissCalcium_Sign)
summary(DissCalcium)
summary(TotChloride_Sign)
summary(TotChloride)
summary(DissChloride_Sign)
summary(DissChloride)
summary(DissSilica_Sign)
summary(DissSilica)
summary(TOC_Sign)
summary(TOC)
summary(DOC_Sign)
summary(DOC)
summary(TDS_Sign)
summary(TDS)
summary(TSS_Sign)
summary(TSS)
summary(VSS_Sign)
summary(VSS)
summary(Notes) 
                # Get more details on character variables
                 
summary(as.factor(dt1$Source)) 
summary(as.factor(dt1$Station)) 
summary(as.factor(dt1$Field_coords)) 
summary(as.factor(dt1$Tide)) 
summary(as.factor(dt1$Secchi_estimated)) 
summary(as.factor(dt1$Microcystis)) 
summary(as.factor(dt1$Chlorophyll_Sign)) 
summary(as.factor(dt1$Pheophytin_Sign)) 
summary(as.factor(dt1$TotAmmonia_Sign)) 
summary(as.factor(dt1$DissAmmonia_Sign)) 
summary(as.factor(dt1$DissNitrateNitrite_Sign)) 
summary(as.factor(dt1$TotPhos_Sign)) 
summary(as.factor(dt1$DissOrthophos_Sign)) 
summary(as.factor(dt1$TON_Sign)) 
summary(as.factor(dt1$DON_Sign)) 
summary(as.factor(dt1$TKN_Sign)) 
summary(as.factor(dt1$TotAlkalinity_Sign)) 
summary(as.factor(dt1$DissBromide_Sign)) 
summary(as.factor(dt1$DissCalcium_Sign)) 
summary(as.factor(dt1$TotChloride_Sign)) 
summary(as.factor(dt1$DissChloride_Sign)) 
summary(as.factor(dt1$DissSilica_Sign)) 
summary(as.factor(dt1$TOC_Sign)) 
summary(as.factor(dt1$DOC_Sign)) 
summary(as.factor(dt1$TDS_Sign)) 
summary(as.factor(dt1$TSS_Sign)) 
summary(as.factor(dt1$VSS_Sign)) 
summary(as.factor(dt1$Notes))
detach(dt1)               
        
	      

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/731/7/0f71269d5347e1c4318424f7efda7503" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")

                   
 dt2 <-read.csv(infile2,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "Survey",     
                    "Abbreviation",     
                    "Agency",     
                    "Survey_focus",     
                    "Start_year",     
                    "Season",     
                    "Frequency",     
                    "Data_source",     
                    "Surface_sample_depth",     
                    "Bottom_sample_depth"    ), check.names=TRUE)
               
unlink(infile2)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt2$Survey)!="factor") dt2$Survey<- as.factor(dt2$Survey)
if (class(dt2$Abbreviation)!="factor") dt2$Abbreviation<- as.factor(dt2$Abbreviation)
if (class(dt2$Agency)!="factor") dt2$Agency<- as.factor(dt2$Agency)
if (class(dt2$Survey_focus)!="factor") dt2$Survey_focus<- as.factor(dt2$Survey_focus)
if (class(dt2$Season)!="factor") dt2$Season<- as.factor(dt2$Season)
if (class(dt2$Frequency)!="factor") dt2$Frequency<- as.factor(dt2$Frequency)
if (class(dt2$Data_source)!="factor") dt2$Data_source<- as.factor(dt2$Data_source)
if (class(dt2$Surface_sample_depth)!="factor") dt2$Surface_sample_depth<- as.factor(dt2$Surface_sample_depth)
if (class(dt2$Bottom_sample_depth)!="factor") dt2$Bottom_sample_depth<- as.factor(dt2$Bottom_sample_depth)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Survey)
summary(Abbreviation)
summary(Agency)
summary(Survey_focus)
summary(Start_year)
summary(Season)
summary(Frequency)
summary(Data_source)
summary(Surface_sample_depth)
summary(Bottom_sample_depth) 
                # Get more details on character variables
                 
summary(as.factor(dt2$Survey)) 
summary(as.factor(dt2$Abbreviation)) 
summary(as.factor(dt2$Agency)) 
summary(as.factor(dt2$Survey_focus)) 
summary(as.factor(dt2$Season)) 
summary(as.factor(dt2$Frequency)) 
summary(as.factor(dt2$Data_source)) 
summary(as.factor(dt2$Surface_sample_depth)) 
summary(as.factor(dt2$Bottom_sample_depth))
detach(dt2)               
        




