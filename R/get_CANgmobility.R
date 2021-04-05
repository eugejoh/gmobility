
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
  regex <- "^202[01]_CA.*\\.csv$"
  
  temp <- tempfile()
  download.file(url = url, destfile = temp)
  
  zipfiles <- unzip(temp, list = TRUE)
  CAfiles <- zipfiles[["Name"]][grepl(regex, zipfiles[["Name"]])]
  
  if (length(CAfiles) > 1) {
    out <- do.call("rbind",lapply(CAfiles, function(x) {read_csv(unz(temp, x))}))
  } else if (length(CAfiles) == 1) {
    out <- read_csv(unz(temp, target_name))
  }
  
  unlink(temp)
  
  message("max date: ",max(as.Date(out$date), na.rm = TRUE))
  return(out)
  
}
