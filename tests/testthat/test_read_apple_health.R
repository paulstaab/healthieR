context("Apple Health")

ah_data_file <- system.file("example_data/Export.zip", package = "healthieR")


test_that("data can be read", {
  data_raw <- ah_load_raw_data(ah_data_file)
  expect_is(data_raw, "xml_document")
})


test_that("class can be initialialized", {
  example_data <- read_apple_health(ah_data_file)
  expect_is(example_data, "apple_health_data")
  expect_is(example_data$get_raw_data(), "xml_document")
})


test_that("personal information can be extracted", {
  ah <- read_apple_health(ah_data_file)
  pi <- ah$get_personal_info()
  expect_is(pi, "list")
  expect_equal(pi$date_of_birth, lubridate::ymd("1980-01-05"))
  expect_equal(pi$sex, "male")
  expect_equal(pi$blood_type, "A-")
  expect_equal(pi$skin_type, 2)
})


test_that("translation of sex works", {
  expect_equal(ah_translate_sex("HKBiologicalSexNotSet"), NA)
  expect_equal(ah_translate_sex("HKBiologicalSexMale"), "male")
  expect_equal(ah_translate_sex("HKBiologicalSexFemale"), "female")
  expect_equal(ah_translate_sex("HKBiologicalSexOther"), "other")
  expect_warning(expect_equal(ah_translate_sex("HKBiologicalSexNew"), NA))
})


test_that("translation of blood_type works", {
  expect_equal(ah_translate_bloodtype("HKBloodTypeNotSet"), NA)
  expect_equal(ah_translate_bloodtype("HKBloodTypeAPositive"), "A+")
  expect_equal(ah_translate_bloodtype("HKBloodTypeANegative"), "A-")
  expect_equal(ah_translate_bloodtype("HKBloodTypeBPositive"), "B+")
  expect_equal(ah_translate_bloodtype("HKBloodTypeBNegative"), "B-")
  expect_equal(ah_translate_bloodtype("HKBloodTypeABPositive"), "AB+")
  expect_equal(ah_translate_bloodtype("HKBloodTypeABNegative"), "AB-")
  expect_equal(ah_translate_bloodtype("HKBloodTypeOPositive"), "0+")
  expect_equal(ah_translate_bloodtype("HKBloodTypeONegative"), "0-")
  expect_warning(expect_equal(ah_translate_bloodtype("HKBloodTypeOther"), NA))
})


test_that("translation of skin_type works", {
  expect_equal(ah_translate_skintype("HKFitzpatrickSkinTypeNotSet"), NA)
  expect_equal(ah_translate_skintype("HKFitzpatrickSkinTypeI"), 1)
  expect_equal(ah_translate_skintype("HKFitzpatrickSkinTypeII"), 2)
  expect_equal(ah_translate_skintype("HKFitzpatrickSkinTypeIII"), 3)
  expect_equal(ah_translate_skintype("HKFitzpatrickSkinTypeIV"), 4)
  expect_equal(ah_translate_skintype("HKFitzpatrickSkinTypeV"), 5)
  expect_warning(
    expect_equal(ah_translate_skintype("HKFitzpatrickSkinTypeOther"), NA)
  )
})


test_that("datetimes are parsed correctly", {
  expect_equal(ah_parse_time("2016-06-10 06:30:00 +0100"),
               lubridate::ymd_hms("2016-06-10 05:30:00", tz = "UTC"))
})


test_that("xml records are extracted", {
  data_raw <- ah_load_raw_data(ah_data_file)
  type <- "HKQuantityTypeIdentifierHeight"
  expect_equal(length(ah_get_xml_records(data_raw, type)), 2)
  type <- "HKQuantityTypeIdentifierDistanceWalkingRunning"
  expect_equal(length(ah_get_xml_records(data_raw, type)), 3)
})


test_that("weight can be extracted from data", {
  ah <- read_apple_health(ah_data_file)
  expect_equal(ah$get_weight(),
               data.frame(time = c(ah_parse_time("2016-06-10 06:30:00 +0100"),
                                   ah_parse_time("2016-06-12 21:07:00 +0100")),
                          unit = c("kg", "kg"),
                          value = c(88.8, 99.9)))
})


test_that("steps can be extracted from data", {
  ah <- read_apple_health(ah_data_file)
  expect_equal(
    ah$get_steps(),
    data.frame(start_time = c(ah_parse_time("2016-06-06 11:55:57 +0100"),
                              ah_parse_time("2016-06-06 12:04:03 +0100")),
               end_time =  c(ah_parse_time("2016-06-06 11:56:12 +0100"),
                             ah_parse_time("2016-06-06 12:09:55 +0100")),
               unit = c("count", "count"),
               value = c(22, 78)))
})
