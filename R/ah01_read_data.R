


#' Read Fitness Data From Apple Health
#'
#' This function reads XML data dumps from Apple Health.
#'
#' @param export_zip Path to the "Export.zip" file from the Apple Health App.
#'
#' @export
#' @examples
#' zip_file <- system.file("example_data/Export.zip", package = "healthieR")
#' health_data <- read_apple_health(zip_file)
#' print(health_data)
read_apple_health <- function(export_zip) {
  fitness_data <- list(
    raw_data = ah_load_raw_data(export_zip)
  )
  class(fitness_data) <- c("apple_health_data",
                           "health_data",
                           "list")
  fitness_data
}


ah_load_raw_data <- function(export_zip) {
  xml_file <- unz(export_zip, "apple_health_export/Export.xml")
  xml_doc <- xml2::read_xml(xml_file)
  xml_doc
}


#' @export
print.apple_health_data <- function(x, ...) {
  cat("Apple Health Export from", format(extract_export_time(x)), "\n")
  cat("Containing:\n")
  records <- extract_records(x)
  if (nrow(records) > 0) {
    cat("*", nrow(records), "health records\n")
  }
}


ah_parse_time <- function(time) {
  time <- gsub(" +", "+", time, fixed = TRUE)
  lubridate::ymd_hms(time, tz = "UTC")
}
