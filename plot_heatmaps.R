library(here)
library(dplyr)
library(purrr)
library(RSQLite)
library(tidyr)
library(lubridate)
library(ggplot2)
library(scales)
library(tools)
library(zoo)
library(extrafont)

# prep
loadfonts("win", quiet = TRUE)
invisible(sapply(list.files("R", full.names = T), source, encoding = "UTF-8"))
colfnc <- colorRampPalette(colors = c("#2196f3", "#FBF6C3", "#CF0B0B"))

theme_heatmap <- theme_minimal(base_family = "Calibri") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1), panel.grid = element_blank(), panel.border = element_rect(fill = NA))

# connect to db
conn <- dbConnect(RSQLite::SQLite(), "gmobility.db")

# get province table
prov <- read_db(conn, "sreg1")

# process for plotting
prov_proc4plot <- prov %>% 
  select(-c(country_region_code, country_region, sub_region_2, iso_3166_2_code)) %>% 
  rename_at(vars(matches("change_from_baseline")), ~gsub("_percent_change_from_baseline$", "", .)) %>% 
  pivot_longer(-c(sub_region_1, prov_abb, date), names_to = "location", values_to = "mobility") %>% 
  group_by(sub_region_1, prov_abb, location) %>% 
  mutate(roll7 = zoo::rollapply(mobility, 7, mean, fill = NA)) %>% 
  # filter(!is.na(roll7)) %>%
  # filter(!prov_abb %in% c("NU", "YT", "NT", "PE", "NL")) %>% 
  mutate(location = factor(location))

# create heatmaps for each province
p_heatmaps <- prov_proc4plot %>% 
  split(.$location) %>% 
  map(function(d) {
    ggplot(d, aes(x = date, y = reorder(prov_abb, desc(prov_abb)), fill = roll7)) +
      geom_tile(color = "white") +
      # scale_fill_gradient2(low = "#2196f3", mid = "#FBF6C3", high = "#CF0B0B", name = "Percent\nChange", na.value = "white") +
      scale_fill_gradientn(colours = colfnc(10), values = seq(0, 1, length.out = 10),  name = "Percent Change\n(7 day MA)", na.value = "white") +
      scale_x_date(date_breaks = "2 week", date_labels  = "%b %d %Y", expand = c(0.01,0)) +
      scale_y_discrete(expand = c(0, 0)) +
      theme_heatmap +
      labs(x = "", y = "Province", title = tools::toTitleCase(gsub("_", " ", unique(d$location))))
  }
  )

# save output
walk2(names(p_heatmaps), p_heatmaps, function(nm, p) {
  ggsave(filename = here("output", paste0("heatmap_", nm, ".png")), plot = p, dpi = 300, type = "cairo", width = 8, height = 4)
})

# get Ontario table
on <- read_db(conn, "sreg2_on")

on_proc4plot <- on %>% 
  select(-c(country_region_code, country_region, sub_region_1, iso_3166_2_code)) %>% 
  rename_at(vars(matches("change_from_baseline")), ~gsub("_percent_change_from_baseline$", "", .)) %>% 
  pivot_longer(-c(sub_region_2, prov_abb, date), names_to = "location", values_to = "mobility") %>% 
  mutate(wk = floor_date(date, unit = "week")) %>% 
  group_by(sub_region_2, prov_abb, location) %>% 
  mutate(roll7 = zoo::rollapply(mobility, 7, mean, fill = NA)) %>% 
  
  # summarise(roll7 = mean(mobility, na.rm = TRUE), .groups = "drop") %>% 
  mutate(location = factor(location))

on_heatmaps <- on_proc4plot %>% 
  split(.$location) %>% 
  map(function(d) {
    ggplot(d, aes(x = date, y = reorder(sub_region_2, desc(sub_region_2)), fill = roll7)) +
      geom_tile(color = "white") +
      # scale_fill_gradient2(low = "#2196f3", mid = "#FBF6C3", high = "#CF0B0B", name = "Percent\nChange", na.value = "white") +
      scale_fill_gradientn(colours = colfnc(10), values = seq(0, 1, length.out = 10),  name = "Percent Change\n(7 day MA)", na.value = "white") +
      scale_x_date(date_breaks = "2 week", date_labels  = "%b %d %Y", expand = c(0.01,0)) +
      scale_y_discrete(expand = c(0, 0)) +
      theme_heatmap +
      labs(x = "", y = "", title = tools::toTitleCase(gsub("_", " ", unique(d$location))))
    
  })
  
# save output
walk2(names(on_heatmaps), on_heatmaps, function(nm, p) {
  ggsave(filename = here("output", paste0("heatmapON_", nm, ".png")), plot = p, dpi = 300, type = "cairo", width = 8, height = 10)
})

# disconnect
dbDisconnect(conn)