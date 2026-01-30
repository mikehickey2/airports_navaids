# test-scrape_airports_navaids.R
# Tests for R/scrape_airports_navaids.R functions

# Source the refactored functions
source_project_file("scrape_airports_navaids.R")

test_that("get_local_data_date validates raw_dir argument", {
  expect_error(
    get_local_data_date("/nonexistent/directory"),
    class = "simpleError"
  )
})

test_that("get_local_data_date returns NA when no data exists", {
  temp_dir <- withr::local_tempdir()

  result <- get_local_data_date(temp_dir)

  expect_true(is.na(result))
})

test_that("get_local_data_date extracts date from directory names", {
  temp_dir <- withr::local_tempdir()

  # Create directory with date pattern
  dir.create(file.path(temp_dir, "19_Dec_2024_APT_CSV"))
  dir.create(file.path(temp_dir, "19_Dec_2024_NAV_CSV"))

  result <- get_local_data_date(temp_dir)

  expect_s3_class(result, "Date")
  expect_equal(result, as.Date("2024-12-19"))
})

test_that("get_local_data_date returns latest date when multiple exist", {
  temp_dir <- withr::local_tempdir()

  # Create directories with different dates
  dir.create(file.path(temp_dir, "19_Dec_2024_APT_CSV"))
  dir.create(file.path(temp_dir, "15_Jan_2025_APT_CSV"))

  result <- get_local_data_date(temp_dir)

  expect_equal(result, as.Date("2025-01-15"))
})

test_that("scrape_faa_current_date validates URL argument", {
  expect_error(
    scrape_faa_current_date("not-a-valid-url"),
    class = "simpleError"
  )

  expect_error(
    scrape_faa_current_date("ftp://invalid-protocol.com"),
    class = "simpleError"
  )
})

test_that("scrape_faa_current_date returns a Date", {
  skip_on_cran()
  skip_if_offline()

  # This test actually hits the FAA website
  # Skip in CI environments where network may be restricted
  skip_on_ci()

  result <- scrape_faa_current_date()

  expect_s3_class(result, "Date")
  # Date should be reasonable (within last year)
  expect_true(result > Sys.Date() - 365)
  expect_true(result <= Sys.Date() + 30)  # Allow for future effective dates
})

test_that("download_faa_data validates date argument", {
  expect_error(
    download_faa_data("not-a-date", type = "APT"),
    class = "simpleError"
  )
})
test_that("download_faa_data validates type argument", {
  temp_dir <- withr::local_tempdir()

  expect_error(
    download_faa_data(Sys.Date(), type = "INVALID", dest_dir = temp_dir),
    "arg"
  )
})

test_that("download_faa_data validates dest_dir argument", {
  expect_error(
    download_faa_data(Sys.Date(), type = "APT", dest_dir = "/nonexistent/dir"),
    class = "simpleError"
  )
})

test_that("delete_old_data removes items from directory", {
  temp_dir <- withr::local_tempdir()

  # Create some files and directories
  file.create(file.path(temp_dir, "test_file.txt"))
  dir.create(file.path(temp_dir, "test_subdir"))

  # Verify they exist
  expect_equal(length(list.files(temp_dir)), 2)

  # Delete them
  result <- delete_old_data(temp_dir)

  expect_equal(result, 2)
  expect_equal(length(list.files(temp_dir)), 0)
})

test_that("delete_old_data handles empty directory", {
  temp_dir <- withr::local_tempdir()

  result <- delete_old_data(temp_dir)

  expect_equal(result, 0)
})

test_that("run_pipeline validates force argument", {
  expect_error(
    run_pipeline(force = "yes"),
    class = "simpleError"
  )

  expect_error(
    run_pipeline(force = NA),
    class = "simpleError"
  )
})

# Integration tests would require network access and are skipped
# test_that("run_pipeline downloads and processes data", {
#   skip_on_cran()
#   skip_if_offline()
#   skip_on_ci()
#
#   # This would be a full integration test
#   # Requires: network access, Supabase credentials, disk space
# })

# --- Tests for get_faa_dates() ---

test_that("get_faa_dates returns list with current_date and preview_date", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()

  result <- get_faa_dates()

  expect_type(result, "list")
  expect_named(result, c("current_date", "preview_date"))
  expect_s3_class(result$current_date, "Date")
  expect_s3_class(result$preview_date, "Date")
})

test_that("get_faa_dates preview_date is after current_date", {
  skip_on_cran()
  skip_if_offline()
  skip_on_ci()

  result <- get_faa_dates()

  expect_true(result$preview_date > result$current_date)
})

test_that("get_faa_dates validates url argument", {
  expect_error(
    get_faa_dates(url = "not-a-url"),
    class = "simpleError"
  )
})
