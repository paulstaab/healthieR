#' Extracts All Records from the Fitness Data
#'
#' @template extractors
#' @export
#' @examples
#' library(dplyr)
#' data_file <- system.file("example_data/Export.zip", package = "healthieR")
#' read_apple_health(data_file) %>% extract_records
extract_records <- function(fitness_data) {
  assertthat::assert_that(inherits(fitness_data, "apple_health_data"))
  ah_get_records(fitness_data$raw_data,
                 NULL,
                 has_duration = TRUE)
}


#' Extracts all Weight Records from the Fitness Data
#'
#' @template extractors
#' @export
#' @examples
#' library(dplyr)
#' data_file <- system.file("example_data/Export.zip", package = "healthieR")
#' read_apple_health(data_file) %>% extract_records
extract_weight <- function(fitness_data) {
  assertthat::assert_that(inherits(fitness_data, "apple_health_data"))
  ah_get_records(fitness_data$raw_data,
                 "HKQuantityTypeIdentifierBodyMass",
                 has_duration = FALSE)
}


#' Extracts all Steps Records from the Fitness Data
#'
#' @template extractors
#' @export
#' @examples
#' library(dplyr)
#' data_file <- system.file("example_data/Export.zip", package = "healthieR")
#' read_apple_health(data_file) %>% extract_steps
extract_steps <- function(fitness_data) {
  assertthat::assert_that(inherits(fitness_data, "apple_health_data"))
  ah_get_records(fitness_data$raw_data,
                 "HKQuantityTypeIdentifierStepCount",
                 has_duration = TRUE)
}


ah_get_xml_records <- function(data_raw, type = NULL) {
  if (is.null(type)) xpath <- ".//Record"
  else xpath <- paste0(".//Record[attribute::type='", type, "']")

  xml2::xml_find_all(data_raw, xpath)
}


ah_parse_record <- function(record) {
  if (grepl("^HKQuantityTypeIdentifier", record["type"])) {
    type <- gsub("^HKQuantityTypeIdentifier", "", record["type"])
    unit <- record["unit"]
    value <- as.numeric(record["value"])
  } else if (grepl("^HKCategoryTypeIdentifier", record["type"])) {
    type <- gsub("^HKCategoryTypeIdentifier", "", record["type"])
    unit <- NA
    value <- gsub("^HKCategoryValue", "", record["value"])
  } else {
    warning("Unknown record type:", record["type"])
    return(NULL)
  }

  data.frame(type = type,
             start_time = ah_parse_time(record["startDate"]),
             end_time = ah_parse_time(record["endDate"]),
             unit = unit,
             value = value,
             row.names = NULL)
}


ah_get_records <- function(data_raw, record_type, has_duration = FALSE) {
  records <- ah_get_xml_records(data_raw, record_type)
  record_attr <- xml2::xml_attrs(records)
  do.call(rbind.data.frame, lapply(record_attr, ah_parse_record))
}
