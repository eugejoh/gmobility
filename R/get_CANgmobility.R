
#' Download Canadian Regions from Google Mobility
#'
#' @param url hyperlink of Region Mobility Report. Default is static URL
#'
#' @return data in tibble, all columns as character
#' 
#' @export
#'
#' @import readr
#'
#' @examples
#' donttest{
#' get_CANgmobility("https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip")
#' }
#' 
get_CANgmobility <- function(url = "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip") {
  
  target_name <- "2020_CA_Region_Mobility_Report.csv"
  
  
  temp <- tempfile()
  download.file(url = url, destfile = temp)
  
  data <- read_csv(unz(temp, target_name), col_types = cols(.default = col_character()))
  unlink(temp)
  
  return(data)
  
  message("max date: ",max(as.Date(data$date), na.rm = TRUE))
   
}
