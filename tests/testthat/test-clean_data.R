# test-clean_data.R
# Tests for R/clean_data.R functions

# Source the refactored functions
source_project_file("clean_data.R")

test_that("clean_airports validates file path exists", {
  expect_error(
    clean_airports("/nonexistent/path/APT_BASE.csv"),
    class = "simpleError"
  )
})

test_that("clean_airports filters out 4-character ARPT_IDs", {
  # Create temp test data
  temp_dir <- withr::local_tempdir()
  temp_file <- file.path(temp_dir, "APT_BASE.csv")

  # Create sample data with mix of 3 and 4 character IDs
  test_data <- tibble::tibble(
    EFF_DATE = "12/19/2024",
    SITE_NO = c("00001", "00002", "00003", "00004"),
    SITE_TYPE_CODE = "A",
    STATE_CODE = c("CA", "TX", "NY", "FL"),
    ARPT_ID = c("LAX", "DFW", "JFK", "KMIA"),  # KMIA is 4 chars
    CITY = c("Los Angeles", "Dallas", "New York", "Miami"),
    COUNTRY_CODE = "US",
    STATE_NAME = c("California", "Texas", "New York", "Florida"),
    COUNTY_NAME = c("Los Angeles", "Tarrant", "Queens", "Miami-Dade"),
    ARPT_NAME = c("Los Angeles Intl", "Dallas Fort Worth", "JFK Intl", "Miami"),
    LAT_DEG = c(33L, 32L, 40L, 25L),
    LAT_MIN = c(56L, 53L, 38L, 47L),
    LAT_SEC = c("33.00", "35.00", "23.00", "36.00"),
    LAT_HEMIS = "N",
    LAT_DECIMAL = c(33.94, 32.89, 40.64, 25.79),
    LONG_DEG = c(118L, 97L, 73L, 80L),
    LONG_MIN = c(24L, 2L, 46L, 17L),
    LONG_SEC = c("29.00", "44.00", "44.00", "25.00"),
    LONG_HEMIS = "W",
    LONG_DECIMAL = c(-118.41, -97.05, -73.78, -80.29),
    ELEV = c(128, 607, 13, 9),
    ELEV_METHOD_CODE = "S",
    MAG_VARN = c(13.0, 4.0, -13.0, -5.0),
    MAG_HEMIS = c("E", "E", "W", "W"),
    MAG_VARN_YEAR = 2020L,
    ICAO_ID = c("KLAX", "KDFW", "KJFK", "KMIA")
  )

  readr::write_csv(test_data, temp_file)

  result <- clean_airports(temp_file)

  # KMIA (4 chars) should be filtered out
  expect_false("KMIA" %in% result$ARPT_ID)

  # LAX, DFW, JFK (3 chars) should remain
  expect_true(all(c("LAX", "DFW", "JFK") %in% result$ARPT_ID))

  expect_equal(nrow(result), 3)
})

test_that("clean_airports returns expected columns", {
  temp_dir <- withr::local_tempdir()
  temp_file <- file.path(temp_dir, "APT_BASE.csv")

  # Create minimal valid test data
  test_data <- tibble::tibble(
    EFF_DATE = "12/19/2024",
    SITE_NO = "00001",
    SITE_TYPE_CODE = "A",
    STATE_CODE = "CA",
    ARPT_ID = "LAX",
    CITY = "Los Angeles",
    COUNTRY_CODE = "US",
    STATE_NAME = "California",
    COUNTY_NAME = "Los Angeles",
    ARPT_NAME = "Los Angeles Intl",
    LAT_DEG = 33L, LAT_MIN = 56L, LAT_SEC = "33.00", LAT_HEMIS = "N",
    LAT_DECIMAL = 33.94,
    LONG_DEG = 118L, LONG_MIN = 24L, LONG_SEC = "29.00", LONG_HEMIS = "W",
    LONG_DECIMAL = -118.41,
    ELEV = 128,
    ELEV_METHOD_CODE = "S",
    MAG_VARN = 13.0, MAG_HEMIS = "E", MAG_VARN_YEAR = 2020L,
    ICAO_ID = "KLAX",
    EXTRA_COL = "should be dropped"  # Extra column not in schema
  )

  readr::write_csv(test_data, temp_file)

  result <- clean_airports(temp_file)

  # Should have exactly the expected columns
  expect_equal(sort(names(result)), sort(airports_columns))

  # Extra column should be dropped
  expect_false("EXTRA_COL" %in% names(result))
})

test_that("clean_airports aborts on missing columns", {
  temp_dir <- withr::local_tempdir()
  temp_file <- file.path(temp_dir, "APT_BASE.csv")

  # Create data missing required columns
  test_data <- tibble::tibble(
    EFF_DATE = "12/19/2024",
    ARPT_ID = "LAX"
    # Missing many required columns
  )

  readr::write_csv(test_data, temp_file)

  expect_error(
    clean_airports(temp_file),
    class = "clean_data_schema_error"
  )
})

test_that("clean_navaids validates file path exists", {
  expect_error(
    clean_navaids("/nonexistent/path/NAV_BASE.csv"),
    class = "simpleError"
  )
})

test_that("clean_navaids returns expected columns", {
  temp_dir <- withr::local_tempdir()
  temp_file <- file.path(temp_dir, "NAV_BASE.csv")

  # Create minimal valid test data
  test_data <- tibble::tibble(
    EFF_DATE = "12/19/2024",
    NAV_ID = "LAX",
    NAV_TYPE = "VOR",
    STATE_CODE = "CA",
    CITY = "Los Angeles",
    COUNTRY_CODE = "US",
    NAME = "Los Angeles VOR",
    STATE_NAME = "California",
    REGION_CODE = "AWP",
    LAT_HEMIS = "N", LAT_DEG = 33L, LAT_MIN = 56L, LAT_SEC = "33.00",
    LAT_DECIMAL = 33.94,
    LONG_HEMIS = "W", LONG_DEG = 118L, LONG_MIN = 24L, LONG_SEC = "29.00",
    LONG_DECIMAL = -118.41,
    ELEV = 128,
    MAG_VARN = 13.0, MAG_VARN_HEMIS = "E", MAG_VARN_YEAR = 2020L,
    ALT_CODE = "LOW",
    EXTRA_COL = "should be dropped"
  )

  readr::write_csv(test_data, temp_file)

  result <- clean_navaids(temp_file)

  expect_equal(sort(names(result)), sort(navaids_columns))
  expect_false("EXTRA_COL" %in% names(result))
})

test_that("validate_cleaned_data requires valid schema_type", {
  data <- sample_airports(3)

  expect_error(
    validate_cleaned_data(data, "invalid_type"),
    "arg"
  )
})

test_that("validate_cleaned_data checks airports have valid ARPT_ID length", {
  # Create data that would fail validation (has 4-char ARPT_ID)
  data <- tibble::tibble(
    EFF_DATE = as.Date("2024-12-19"),
    SITE_NO = "00001",
    SITE_TYPE_CODE = "A",
    STATE_CODE = "CA",
    ARPT_ID = "KMIA",  # 4 characters - should fail
    CITY = "Miami",
    COUNTRY_CODE = "US",
    STATE_NAME = "Florida",
    COUNTY_NAME = "Miami-Dade",
    ARPT_NAME = "Miami Intl",
    LAT_DEG = 25L, LAT_MIN = 47L, LAT_SEC = "36.00", LAT_HEMIS = "N",
    LAT_DECIMAL = 25.79,
    LONG_DEG = 80L, LONG_MIN = 17L, LONG_SEC = "25.00", LONG_HEMIS = "W",
    LONG_DECIMAL = -80.29,
    ELEV = 9,
    ELEV_METHOD_CODE = "S",
    MAG_VARN = -5.0, MAG_HEMIS = "W", MAG_VARN_YEAR = 2020L,
    ICAO_ID = "KMIA"
  )

  expect_error(
    validate_cleaned_data(data, "airports"),
    class = "validation_error"
  )
})

test_that("find_raw_data_dirs validates directory exists", {
  expect_error(
    find_raw_data_dirs("/nonexistent/directory"),
    class = "simpleError"
  )
})

test_that("find_raw_data_dirs aborts when no APT/NAV dirs found", {
  temp_dir <- withr::local_tempdir()

  expect_error(
    find_raw_data_dirs(temp_dir),
    class = "clean_data_missing_dirs"
  )
})
