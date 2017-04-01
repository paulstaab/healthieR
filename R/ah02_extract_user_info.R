#' Extracts information about the user
#'
#' This function extracts certain chacterisitcs (see *Extracted Information*)
#' about the athlete if they are present in the data.
#'
#' @section Extracted Information:
#' * Date of Birth (Column `date_of_birth`)
#' * Sex (Column `sex`)
#' * Blood Type (Column `blood_type`)
#' * Fitzpatrick Skin Type (Column `skin_type`)
#'
#' @template extractors
#' @export
#' @examples
#' library(dplyr)
#' data_file <- system.file("example_data/Export.zip", package = "healthieR")
#' read_apple_health(data_file) %>% extract_personal_info
extract_personal_info <- function(fitness_data) {
  assertthat::assert_that(inherits(fitness_data, "apple_health_data"))

  xml_info <- xml2::xml_find_first(fitness_data$raw_data, ".//Me")
  info <- xml2::xml_attrs(xml_info)
  personal_info <- list()

  dob_key <- "HKCharacteristicTypeIdentifierDateOfBirth"
  personal_info$date_of_birth <- lubridate::ymd(info[dob_key])

  sex_key <- "HKCharacteristicTypeIdentifierBiologicalSex"
  personal_info$sex <- ah_translate_sex(info[sex_key])

  blood_key <- "HKCharacteristicTypeIdentifierBloodType"
  personal_info$blood_type <- ah_translate_bloodtype(info[blood_key])

  skin_key <- "HKCharacteristicTypeIdentifierFitzpatrickSkinType"
  personal_info$skin_type <- ah_translate_skintype(info[skin_key])

  tibble::as_tibble(personal_info)
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


#' Extracts the Date at which the Fitness Data was Exported
#'
#' @template extractors
#' @export
#' @examples
#' library(dplyr)
#' data_file <- system.file("example_data/Export.zip", package = "healthieR")
#' read_apple_health(data_file) %>% extract_export_time
extract_export_time <- function(fitness_data) {
  assertthat::assert_that(inherits(fitness_data, "apple_health_data"))
  xml_export_date <- xml2::xml_find_first(fitness_data$raw_data,
                                          ".//ExportDate")
  ah_parse_time(xml2::xml_attr(xml_export_date, "value"))
}
