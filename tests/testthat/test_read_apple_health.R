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
