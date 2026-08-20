con <- DBI::dbConnect(odbc::odbc(),
                      Driver = "MySQL ODBC 9.7 Unicode Driver",
                      Server = "mysql8.natweb.usgs.gov",
                      Database = "sfbay",
                      UID = "sfbay",
                      PWD = "TnMNAXA9AypR", #Ask Schuyler for PW if you need it!
                      Port = 3306,
                      SSLMODE = "REQUIRED")

# Read in cruise database
cruiseall <- DBI::dbReadTable(con, "cruiseall")

# Read in nutrient database
nutrients <- DBI::dbReadTable(con, "nutrients")

# Disconnect database
dbDisconnect(con)
rm(con)

save(cruiseall,nutrients, file = "Data/Peterson_data.Rdata")


#--------------------------------------------------
# Helper function for loading EDI entities
#--------------------------------------------------
read_edi_entity <- function(package_id, entity_name) {
  entity_id <- read_data_entity_names(package_id) %>%
    filter(entityName == entity_name) %>%
    pull(entityId)
  raw_file <- read_data_entity(package_id, entity_id)
  readr::read_csv(raw_file, show_col_types = FALSE)
}

#--------------------------------------------------
# Load PESP data
#--------------------------------------------------

pesp <- read_edi_entity(package_id = "edi.2209.3",entity_name = "PESP_enumeration.csv") 
emp <- read_edi_entity(package_id = "edi.458.14",entity_name = "EMP_DWQ_1975-2024") 

