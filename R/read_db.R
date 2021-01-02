
#' Read tables from SQLite into R
#'
#' Retrieves table from SQLite Google Mobility Canadian regions with consistent type conversion
#'
#' @param conn A DBIConnection object, as returned by dbConnect().
#' @param name A string, the table name
#'
#' @return tibble
#' @export
#' 
#' @importFrom dplyr tbl
#' @importFrom dplyr collect
#' @import readr
#' 
#' 
#' 
#' @examples
#' \donttest{
#' read_db(conn, "reg_name")
#' }
read_db <- function(conn, name, fetch = TRUE) {
  
  x_conn <- tbl(conn, name)
  
  if (fetch) {
    x <- collect(x_conn) %>% 
      mutate(date = as.Date(date, origin = "1970-01-01"))
  } else if (!fetch) {
    x <- x_conn
  }
  
  return(x)
  
  # cols_db <- cols(
  #   country_region_code = col_character(),
  #   country_region = col_character(),
  #   sub_region_1 = col_character(),
  #   sub_region_2 = col_character(),
  #   iso_3166_2_code = col_character(),
  #   date = col_date(format = "%Y-%m-%d"),
  #   retail_and_recreation_percent_change_from_baseline = col_double(),
  #   grocery_and_pharmacy_percent_change_from_baseline = col_double(),
  #   parks_percent_change_from_baseline = col_double(),
  #   transit_stations_percent_change_from_baseline = col_double(),
  #   workplaces_percent_change_from_baseline = col_double(),
  #   residential_percent_change_from_baseline = col_double(),
  #   prov_abb = col_character()
  # )
  # col_check <- names(cols_db$cols)
  # 
  # stopifnot(all(colnames(x_conn) %in% col_check))
  # 
  # x <- collect(x_conn)
  # x <- type_convert(x, cols_db)
  # 
  # 
}


