context("Apple Health - Extracting User Information")

ah_data_file <- system.file("example_data/Export.zip", package = "healthieR")
ah_data <- read_apple_health(ah_data_file)


test_that("personal information can be extracted", {
  pi <- ah_data %>% extract_personal_info()
  expect_is(pi, "tbl_df")
  expect_equal(pi$date_of_birth, lubridate::ymd("1980-01-05"))
  expect_equal(pi$sex, "male")
  expect_equal(pi$blood_type, "A-")
  expect_equal(pi$skin_type, 2)
})


test_that("an error this thrown for other data", {
  expect_error(extract_personal_info(NA))
  expect_error(extract_personal_info(5))
  expect_error(extract_personal_info(list(A = "5")))
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


test_that("the export date can be extracted", {
  expect_equal(extract_export_time(ah_data),
               ah_parse_time("2017-01-25 20:11:49 +0100"))
})
