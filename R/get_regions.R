#' Subset Regions from Google Mobility Data
#'
#' @param x a dataframe raw csv import from zip
#' @param region 1-Canada, 2-province/territory, 3-municipalities
#'
#' @return dataframe of subset
#' @export
#'
#' @importFrom dplyr filter select left_join
#' @importFrom tibble tibble
#' @importFrom  purrr map_lgl keep
#'
#' @examples
get_regions <- function(x, region = c(1,2,3)) {
  
  stopifnot(region %in% c(1,2,3))
  stopifnot(c("country_region", "sub_region_1", "sub_region_2") %in% names(x))
  
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
  
  rm_cols <- names(keep(map_lgl(x0, ~sum(is.na(.)) == nrow(x0)), isTRUE))
  
  x <- x %>% 
    select(-all_of(rm_cols))
  
  x <- left_join(x, prov_df %>% select(-country), by = c("sub_region_1" = "prov_names"))
  
  if (region == 1) {
    out <- filter(x, country_region == "Canada" & is.na(sub_region_1))
  }
  
  if (region == 2) {
    out <- filter(x, sub_region_1 %in% prov_df$prov_names & is.na(sub_region_2))
  }
  
  if (region == 3) {
    out <- filter(x, sub_region_1 %in% prov_df$prov_names & !is.na(sub_region_2))
  }
  
  return(out)
}
