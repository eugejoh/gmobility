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

theme_trends <- theme_minimal(base_family = "Calibri") +
  theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1), panel.grid.major = element_blank(), panel.border = element_rect(fill = NA))

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
  ungroup() %>% 
  mutate(
    roll7neg = ifelse(roll7 < 0, roll7, 0),
    roll7pos = ifelse(roll7 >=0, roll7, 0)
  ) %>% 
  mutate(rev_baseline = roll7neg == 0) %>% 
  # filter(!is.na(roll7)) %>%
  # filter(!prov_abb %in% c("NU", "YT", "NT", "PE", "NL")) %>% 
  # mutate(location = factor(location)) %>% 
  # filter(location != "parks") %>% 
  mutate(location = tools::toTitleCase(gsub("_", " ", location)))

p_trends <- prov_proc4plot %>% 
  split(.$prov_abb) %>% 
  map(function(z) {
    ggplot(z, aes(x = date)) +
      geom_point(aes(y = mobility), size = 0.2, color = "#3C1E39",alpha = 0.4) +
      geom_area(aes(y = roll7neg), fill = "#2196f3", alpha = 0.25, col = NA) +
      geom_area(aes(y = roll7pos), fill = "#CF0B0B", alpha = 0.25, col = NA) +
      geom_line(aes(y = roll7), color = "#3C1E39", size = 0.8, alpha = 0.9) +

      geom_hline(yintercept = 0, lty = 2) +
      theme_trends +
      facet_wrap(~location, ncol = 2) +
      scale_x_date(date_breaks = "2 week", date_labels  = "%b %d %Y", expand = c(0.01,0)) +
      scale_y_discrete(expand = c(0, 0)) +
      labs(x = "", y = "Percent Change", title = unique(z$sub_region_1))
  })


# save output
walk2(names(p_trends), p_trends, function(nm, p) {
  ggsave(filename = here("output", paste0("trends_", nm, ".png")), plot = p, dpi = 300, type = "cairo", width = 8, height = 4)
})


