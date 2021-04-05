# Run All -----------------------------------------------------------------
library(here)
library(readr)
library(dplyr)
library(purrr)
library(RSQLite)
library(DBI)
library(rmarkdown)
library(zoo)
library(extrafont)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(tools)

# update DB
source(here::here("00_update_db.r"))

# create heatmaps
source(here::here("plot_heatmaps.R"))
max_date <- max(prov$date, na.rm = TRUE)

# update .rmd
render_site(here::here("_site"))