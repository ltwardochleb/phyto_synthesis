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
