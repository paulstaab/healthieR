apple_health_class <- R6::R6Class("apple_health_data",
  private = list(
    data_raw = NULL,
    personal_info = list(date_of_birth = NA,
                         sex = NA,
                         blood_type = NA,
                         skin_type = NA)
  ),
  public = list(
    initialize = function(export_zip) {
      private$data_raw <- ah_load_raw_data(export_zip)
      private$personal_info <- ah_parse_personal_info(private$data_raw,
                                                      private$personal_info)
    },
    get_raw_data = function() {
      "returns the raw XML data"
      private$data_raw
    },
    get_personal_info = function() {
      "returns personal information (date of birth, sex, blood & skin type)"
      private$personal_info
    },
    get_all_records = function() {
      "returns all records from as data.frame"
      ah_get_records(private$data_raw,
                     NULL,
                     has_duration = TRUE)
    },
    get_weight = function() {
      "returns weight information as data.frame"
      ah_get_records(private$data_raw,
                    "HKQuantityTypeIdentifierBodyMass",
                    has_duration = FALSE)
    },
    get_steps = function() {
      "returns step entries as data.frame"
      ah_get_records(private$data_raw,
                    "HKQuantityTypeIdentifierStepCount",
                    has_duration = TRUE)
    }
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
  apple_health_class$new(export_zip) #nolint
}


ah_load_raw_data <- function(export_zip) {
  xml_file <- unz(export_zip, "apple_health_export/Export.xml")
  xml_doc <- xml2::read_xml(xml_file)
  xml_doc
}


ah_parse_personal_info <- function(data_raw, personal_info) {
  xml_info <- xml2::xml_find_first(data_raw, ".//Me")
  info <- xml2::xml_attrs(xml_info)

  dob_key <- "HKCharacteristicTypeIdentifierDateOfBirth"
  personal_info$date_of_birth <- lubridate::ymd(info[dob_key])

  sex_key <- "HKCharacteristicTypeIdentifierBiologicalSex"
  personal_info$sex <- ah_translate_sex(info[sex_key])

  blood_key <- "HKCharacteristicTypeIdentifierBloodType"
  personal_info$blood_type <- ah_translate_bloodtype(info[blood_key])

  skin_key <- "HKCharacteristicTypeIdentifierFitzpatrickSkinType"
  personal_info$skin_type <- ah_translate_skintype(info[skin_key])

  personal_info
}


# https://developer.apple.com/reference/healthkit/hkbiologicalsex
ah_translate_sex <- function(x) {
  switch(x,
    "HKBiologicalSexNotSet" = NA,
    "HKBiologicalSexMale" = "male",
    "HKBiologicalSexFemale" = "female",
    "HKBiologicalSexOther" = "other",
    {
      warning("Unknown value for 'sex': ", x)
      NA
    }
  )
}


# https://developer.apple.com/reference/healthkit/hkbloodtype
ah_translate_bloodtype <- function(x) {
  switch(x,
    "HKBloodTypeNotSet"     = NA,
    "HKBloodTypeAPositive"  = "A+",
    "HKBloodTypeANegative"  = "A-",
    "HKBloodTypeBPositive"  = "B+",
    "HKBloodTypeBNegative"  = "B-",
    "HKBloodTypeABPositive" = "AB+",
    "HKBloodTypeABNegative" = "AB-",
    "HKBloodTypeOPositive"  = "0+",
    "HKBloodTypeONegative"  = "0-",
    {
      warning("Unknown value for 'blood_type': ", x)
      NA
    }
  )
}


ah_translate_skintype <- function(x) {
  switch(x,
     "HKFitzpatrickSkinTypeNotSet" = NA,
     "HKFitzpatrickSkinTypeI"      = 1,
     "HKFitzpatrickSkinTypeII"     = 2,
     "HKFitzpatrickSkinTypeIII"    = 3,
     "HKFitzpatrickSkinTypeIV"     = 4,
     "HKFitzpatrickSkinTypeV"      = 5,
     {
       warning("Unknown value for 'skin_type': ", x)
       NA
     }
  )
}


ah_parse_time <- function(time) {
  time <- gsub(" +", "+", time, fixed = TRUE)
  lubridate::ymd_hms(time, tz = "UTC")
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
