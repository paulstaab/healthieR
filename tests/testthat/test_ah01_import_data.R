context("Apple Health - Reading the data")

ah_data_file <- system.file("example_data/Export.zip", package = "healthieR")


test_that("data can be read", {
  data_raw <- ah_load_raw_data(ah_data_file)
  expect_is(data_raw, "xml_document")
})


test_that("class can be initialialized", {
  example_data <- read_apple_health(ah_data_file)
  expect_is(example_data, "apple_health_data")
  expect_is(example_data, "health_data")
  expect_is(example_data$raw_data, "xml_document")
})


test_that("datetimes are parsed correctly", {
  expect_equal(ah_parse_time("2016-06-10 06:30:00 +0100"),
               lubridate::ymd_hms("2016-06-10 05:30:00", tz = "UTC"))
})


test_that("printing the object is supported", {
  ah <- read_apple_health(ah_data_file)
  expect_output(print(ah))
})
