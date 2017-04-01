context("Apple Health - Extracting Records")

ah_data_file <- system.file("example_data/Export.zip", package = "healthieR")
ah_data <- read_apple_health(ah_data_file)


test_that("xml records are extracted", {
  data_raw <- ah_load_raw_data(ah_data_file)
  type <- "HKQuantityTypeIdentifierHeight"
  expect_equal(length(ah_get_xml_records(data_raw, type)), 2)
  type <- "HKQuantityTypeIdentifierDistanceWalkingRunning"
  expect_equal(length(ah_get_xml_records(data_raw, type)), 3)
})


test_that("weight can be extracted from data", {
  expect_equal(
    extract_weight(ah_data),
    data.frame(type = "BodyMass",
               start_time = c(ah_parse_time("2016-06-10 06:30:00 +0100"),
                              ah_parse_time("2016-06-12 21:07:00 +0100")),
               end_time = c(ah_parse_time("2016-06-10 06:30:00 +0100"),
                            ah_parse_time("2016-06-12 21:07:00 +0100")),
               unit = "kg",
               value = c(88.8, 99.9)))
})


test_that("steps can be extracted from data", {
  expect_equal(
    extract_steps(ah_data),
    data.frame(type = "StepCount",
               start_time = c(ah_parse_time("2016-06-06 11:55:57 +0100"),
                              ah_parse_time("2016-06-06 12:04:03 +0100")),
               end_time =  c(ah_parse_time("2016-06-06 11:56:12 +0100"),
                             ah_parse_time("2016-06-06 12:09:55 +0100")),
               unit = "count",
               value = c(22, 78)))
})


test_that("all records can be extracted", {
  expect_silent(records <- extract_records(ah_data))
  expect_is(records, "data.frame")
  expect_equal(colnames(records), c("type", "start_time",
                                    "end_time", "unit", "value"))
  expect_gt(nrow(records), 70)
  expect_false(any(grepl("HKQuantityTypeIdentifier", records$type)))
  expect_false(any(grepl("HKCategoryTypeIdentifier", records$type)))
})


test_that("parsing unknown record types throws a warnings", {
  record <- c(type = "UnknownRecord",
              startDate = "2017-01-14 02:38:00 +0100",
              endDate = "2017-01-14 02:54:59 +0100",
              value = "17")
  expect_warning(expect_equal(ah_parse_record(record), NULL))
})
