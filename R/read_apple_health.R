apple_health_class <- R6::R6Class("apple_health_data",
  private = list(
    data_raw = NULL
  ),
  public = list(
    initialize = function(export_zip) {
      private$data_raw <- ah_load_raw_data(export_zip)
    },
    get_raw_data = function() private$data_raw
  )
)

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
  apple_health_class$new(export_zip)
}


ah_load_raw_data <- function(export_zip) {
  xml_file <- unz(export_zip, "apple_health_export/Export.xml")
  xml_doc <- xml2::read_xml(xml_file)
  xml_doc
}
