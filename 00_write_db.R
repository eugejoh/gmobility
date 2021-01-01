library(here)
library(readr)
library(dplyr)
library(purrr)
library(RSQLite)
library(DBI)

# create province table
prov_df <- tibble(
  country = c("Canada"),
  prov_names = c("Alberta",
                 "British Columbia",
                 "Manitoba",
                 "New Brunswick",
                 "Newfoundland and Labrador",
                 "Northwest Territories",
                 "Nova Scotia",
                 "Nunavut",
                 "Ontario",
                 "Prince Edward Island",
                 "Quebec",
                 "Saskatchewan",
                 "Yukon"),
  prov_abb = c("AB", "BC", "MB", "NB", "NL", "NT", "NS", "NU", "ON", "PE", "QC", "SK", "YT"))

sapply(list.files("R", full.names = T), source, encoding = "UTF-8")

x0 <- get_CANgmobility("https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip")

# Write to local SQLite database ------------------------------------------
# intialize/connect DB
conn <- dbConnect(RSQLite::SQLite(), "gmobility.db")

# canada
d_reg <- get_regions(x0, 1)

# province/territory
d_sreg1 <- get_regions(x0, 2)

# municipal
d_sreg2 <- get_regions(x0, 3)
# split by province
d_sreg2l <- d_sreg2 %>% split(.$prov_abb)
names(d_sreg2l) <- paste0("sreg2_", tolower(names(d_sreg2l)))

# write region name table
dbWriteTable(conn, "reg_name", prov_df, overwrite = TRUE)
# write region level (Canada)
dbWriteTable(conn, "reg", d_reg, overwrite = TRUE)
# write sub region 1 level (province/territory)
dbWriteTable(conn, "sreg1", d_sreg1, overwrite = TRUE)
# write sub region 2 level (municipality)
purrr::map2(names(d_sreg2l), d_sreg2l, function(x,y) dbWriteTable(conn, x, y, overwrite = TRUE))

# check tables
dbListTables(conn)



# disconnect
dbDisconnect(conn)