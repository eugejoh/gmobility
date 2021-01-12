library(here)
library(readr)
library(dplyr)
library(purrr)
library(RSQLite)
library(DBI)

# load fnc
sapply(list.files("R", full.names = T), source, encoding = "UTF-8")


# Download .csv -----------------------------------------------------------
x0 <- get_CANgmobility("https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip")


# Process -----------------------------------------------------------------
list2db <- c(
  list(reg = get_regions(x0, 1), # canada
       sreg1 = get_regions(x0, 2)), # province/territory
  get_regions(x0, 3) %>% # municipal, split by province
    split(., .$prov_abb) %>%
    set_names(paste0("sreg2_", tolower(names(
      .
    ))))
)
names(list2db)

# Initialize DB -----------------------------------------------------------
if (!file.exists(here("gmobility.db"))) {
  conn <- dbConnect(RSQLite::SQLite(), "gmobility.db")
  
  map2(names(list2db), list2db, function(x, y) dbWriteTable(conn, name = x, value = y, overwrite = TRUE)) %>% 
    set_names(names(list2db))
  dbDisconnect(conn) # disconnect
}

# Compare and Append DB Tables --------------------------------------------
# connect DB
conn <- dbConnect(RSQLite::SQLite(), "gmobility.db")

if (all(names(list2db) %in% dbListTables(conn))) {
  
  # fetch tables in db
  listdb <- map(names(list2db), function(x) read_db(conn, x)) %>% 
    set_names(names(list2db))

  # get max dats and compare
  db_maxdates <- map(names(list2db), function(y) {
    read_db(conn, y, fetch = FALSE) %>% select(date) %>% 
      filter(date == max(date, na.rm = TRUE)) %>% distinct() %>% 
      collect() %>% mutate(date = as.Date(date, origin = "1970-01-01")) %>% pull()
  }) %>% set_names(names(list2db))
  new_maxdates <- map(list2db, ~max(.$date, na.rm = TRUE))
  comp <- map2_lgl(db_maxdates, new_maxdates, ~.x==.y)
  
  if (!all(comp)) {
    update <- names(comp)[!comp] #get names of tables to update
    # get new rows
    append_rows <- map(update, function(i) {
      anti_join(list2db[[i]], read_db(conn, i),
                by = c("country_region_code", "country_region", "sub_region_1", "sub_region_2", "iso_3166_2_code", "date", "prov_abb"))
    }) %>% 
      set_names(update)
    
    map2(names(append_rows), append_rows, function(x,y) dbWriteTable(conn, name = x, value = y, append = TRUE)) %>% 
      set_names(names(append_rows))
    dbDisconnect(conn) # disconnect
    message(paste(update, collapse = ", "), " tables updated")
  } else if (all(comp)) {
    message("no update required for tables")
  }
}

