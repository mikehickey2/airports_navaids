# test-notify.R
# Tests for R/notify.R notification functions

source_project_file("notify.R")

# --- Tests for get_notify_config() ---

test_that("get_notify_config returns default disabled state", {
  withr::local_envvar(
    NOTIFY_EMAIL_ENABLED = NA,
    NOTIFY_SMS_ENABLED = NA
  )

  result <- get_notify_config()

  expect_type(result, "list")
  expect_false(result$email_enabled)
  expect_false(result$sms_enabled)
})

test_that("get_notify_config reads environment variables", {
  withr::local_envvar(
    NOTIFY_EMAIL_ENABLED = "true",
    NOTIFY_SMS_ENABLED = "true",
    NOTIFY_EMAIL = "test@example.com",
    NOTIFY_PHONE = "+15551234567"
  )

  result <- get_notify_config()

  expect_true(result$email_enabled)
  expect_true(result$sms_enabled)
  expect_equal(result$email, "test@example.com")
  expect_equal(result$phone, "+15551234567")
})

# --- Tests for format_notification() ---

test_that("format_notification creates success message", {
  data <- list(
    faa_eff_date = as.Date("2026-02-19"),
    airports_count = 5312,
    navaids_count = 1658,
    next_expected_date = as.Date("2026-03-19")
  )

  result <- format_notification("success", data)

  expect_type(result, "list")
  expect_match(result$subject, "Update Complete")
  expect_match(result$body, "5,312")
  expect_match(result$body, "1,658")
  expect_match(result$body, "March 19, 2026")
})

test_that("format_notification creates failure message", {
  data <- list(
    error_message = "Connection timeout",
    stage = "scrape"
  )

  result <- format_notification("failure", data)

  expect_match(result$subject, "Failed")
  expect_match(result$body, "Connection timeout")
  expect_match(result$body, "scrape")
})

test_that("format_notification creates reminder message", {
  data <- list(
    preview_date = as.Date("2026-02-19")
  )

  result <- format_notification("reminder", data)

  expect_match(result$subject, "Expected Tomorrow")
  expect_match(result$body, "February 19, 2026")
})

# --- Tests for send_email() (dry run mode) ---

test_that("send_email returns TRUE in dry_run mode", {
  withr::local_envvar(DRY_RUN = "true")

  result <- send_email("Test Subject", "Test Body")

  expect_true(result)
})

test_that("send_email validates subject argument", {
  expect_error(
    send_email(123, "body"),
    class = "simpleError"
  )
})

# --- Tests for send_sms() (dry run mode) ---

test_that("send_sms returns TRUE in dry_run mode", {
  withr::local_envvar(DRY_RUN = "true")

  result <- send_sms("Test message")

  expect_true(result)
})

test_that("send_sms validates message argument", {
  expect_error(
    send_sms(123),
    class = "simpleError"
  )
})

# --- Tests for send_notification() ---

test_that("send_notification respects enabled flags", {
  withr::local_envvar(
    DRY_RUN = "true",
    NOTIFY_EMAIL_ENABLED = "true",
    NOTIFY_SMS_ENABLED = "false",
    NOTIFY_EMAIL = "test@example.com"
  )

  data <- list(
    faa_eff_date = as.Date("2026-02-19"),
    airports_count = 5312,
    navaids_count = 1658,
    next_expected_date = as.Date("2026-03-19")
  )

  result <- send_notification("success", data)

  expect_true(result)
})
