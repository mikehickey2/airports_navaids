# NASR Auto-Scrape Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Automate FAA NASR pipeline via GitHub Actions with email/SMS notifications

**Architecture:** Daily workflow checks FAA schedule, runs pipeline during update window, sends notifications via Twilio/SendGrid, logs to Supabase and CSV

**Tech Stack:** R (httr2, tidyverse), GitHub Actions, Twilio (SMS), SendGrid (email), Supabase (PostgreSQL)

---

## Task 1: Create SQL Schema for Pipeline Logs

**Files:**
- Create: `sql/create_pipeline_logs.sql`

**Step 1: Write the SQL schema**

```sql
-- sql/create_pipeline_logs.sql
-- Schema for pipeline run logging
-- Run this in Supabase SQL Editor to create the table

CREATE TABLE IF NOT EXISTS pipeline_logs (
  id SERIAL PRIMARY KEY,
  run_timestamp TIMESTAMPTZ DEFAULT NOW(),
  status TEXT NOT NULL CHECK (status IN ('success', 'failure', 'warning')),
  airports_count INTEGER,
  navaids_count INTEGER,
  faa_eff_date DATE,
  next_expected_date DATE,
  error_message TEXT,
  duration_seconds INTEGER,
  triggered_by TEXT CHECK (triggered_by IN ('scheduled', 'manual', 'local'))
);

-- Enable Row Level Security
ALTER TABLE pipeline_logs ENABLE ROW LEVEL SECURITY;

-- Allow public read access
CREATE POLICY "Allow public read" ON pipeline_logs
  FOR SELECT USING (true);

-- Allow insert for authenticated/anon users (for automation)
CREATE POLICY "Allow insert" ON pipeline_logs
  FOR INSERT WITH CHECK (true);

-- Index for querying by status and date
CREATE INDEX idx_pipeline_logs_status ON pipeline_logs(status);
CREATE INDEX idx_pipeline_logs_timestamp ON pipeline_logs(run_timestamp DESC);
```

**Step 2: Commit**

```bash
git add sql/create_pipeline_logs.sql
git commit -m "feat(sql): add pipeline_logs table schema"
```

---

## Task 2: Initialize Pipeline History CSV

**Files:**
- Create: `data/pipeline_history.csv`

**Step 1: Create the CSV with headers**

```csv
run_timestamp,status,airports_count,navaids_count,faa_eff_date,next_expected_date,error_message,duration_seconds,triggered_by
```

**Step 2: Commit**

```bash
git add data/pipeline_history.csv
git commit -m "feat(data): initialize pipeline history log"
```

---

## Task 3: Update .Renviron.example with Notification Templates

**Files:**
- Modify: `.Renviron.example`

**Step 1: Add notification configuration**

Replace entire file with:

```
# Supabase Configuration
SUPABASE_API_KEY=your_supabase_anon_or_service_role_key_here

# Notification Settings
# Set to "true" or "false" to enable/disable each channel
NOTIFY_EMAIL_ENABLED=false
NOTIFY_SMS_ENABLED=false

# Contact Information
NOTIFY_EMAIL=your_email@example.com
NOTIFY_PHONE=+15551234567

# Twilio Credentials (for SMS)
# Get these from https://console.twilio.com/
TWILIO_ACCOUNT_SID=ACxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
TWILIO_AUTH_TOKEN=xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
TWILIO_FROM_NUMBER=+15559876543

# SendGrid Credentials (for Email)
# Access via Twilio Console > Email > SendGrid
SENDGRID_API_KEY=SG.xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

**Step 2: Commit**

```bash
git add .Renviron.example
git commit -m "docs: add notification config templates to .Renviron.example"
```

---

## Task 4: Write Tests for get_faa_dates() Function

**Files:**
- Modify: `tests/testthat/test-scrape_airports_navaids.R`

**Step 1: Write the failing tests**

Add to end of `tests/testthat/test-scrape_airports_navaids.R`:

```r
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
```

**Step 2: Run test to verify it fails**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-scrape_airports_navaids.R')"`

Expected: FAIL with "could not find function 'get_faa_dates'"

**Step 3: Commit failing test**

```bash
git add tests/testthat/test-scrape_airports_navaids.R
git commit -m "test(scrape): add failing tests for get_faa_dates()"
```

---

## Task 5: Implement get_faa_dates() Function

**Files:**
- Modify: `R/scrape_airports_navaids.R:68-108` (after scrape_faa_current_date)

**Step 1: Add the function**

Insert after `scrape_faa_current_date()` function (around line 108):

```r
#' Get both current and preview dates from FAA website
#'
#' Scrapes the FAA NASR subscription page to extract both the current
#' effective date and the preview (next) date.
#'
#' @param url URL of FAA NASR subscription page
#' @return Named list with current_date and preview_date (both Date objects)
#' @export
get_faa_dates <- function(url = faa_nasr_url) {
  checkmate::assert_string(url, pattern = "^https?://")

  message("Checking FAA website for subscription dates...")

  page <- rvest::read_html(url)
  page_text <- page |> rvest::html_text()

  # Extract current date
  current_match <- stringr::str_match(
    page_text,
    "Current[^A-Za-z]*Subscription effective ([A-Za-z]+ \\d{1,2}, \\d{4})"
  )

  if (is.na(current_match[1, 1])) {
    rlang::abort(
      c(
        "Could not parse current subscription date from FAA website",
        i = "The page format may have changed",
        x = paste("URL:", url)
      ),
      class = "scrape_parse_error"
    )
  }

  current_date <- lubridate::mdy(current_match[1, 2])

  # Extract preview date
  preview_match <- stringr::str_match(
    page_text,
    "Preview[^A-Za-z]*Subscription effective ([A-Za-z]+ \\d{1,2}, \\d{4})"
  )

  if (is.na(preview_match[1, 1])) {
    rlang::abort(
      c(
        "Could not parse preview subscription date from FAA website",
        i = "The page format may have changed",
        x = paste("URL:", url)
      ),
      class = "scrape_parse_error"
    )
  }

  preview_date <- lubridate::mdy(preview_match[1, 2])

  message("Current FAA date: ", format(current_date, "%d %b %Y"))
  message("Preview FAA date: ", format(preview_date, "%d %b %Y"))

  list(
    current_date = current_date,
    preview_date = preview_date
  )
}
```

**Step 2: Run test to verify it passes**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-scrape_airports_navaids.R')"`

Expected: All tests PASS

**Step 3: Run lintr**

Run: `Rscript -e "lintr::lint('R/scrape_airports_navaids.R')"`

Expected: No errors

**Step 4: Commit**

```bash
git add R/scrape_airports_navaids.R
git commit -m "feat(scrape): add get_faa_dates() for current + preview dates"
```

---

## Task 6: Write Tests for Notification Functions

**Files:**
- Create: `tests/testthat/test-notify.R`

**Step 1: Write the failing tests**

```r
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

  # Should not error even with SMS disabled

  result <- send_notification("success", data)

  expect_true(result)
})
```

**Step 2: Run test to verify it fails**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-notify.R')"`

Expected: FAIL with "cannot open file 'R/notify.R'"

**Step 3: Commit failing tests**

```bash
git add tests/testthat/test-notify.R
git commit -m "test(notify): add failing tests for notification functions"
```

---

## Task 7: Implement R/notify.R - Configuration Functions

**Files:**
- Create: `R/notify.R`

**Step 1: Create the file with configuration functions**

```r
# notify.R
# Functions for sending notifications (email, SMS) and logging pipeline runs
#
# Usage:
#   source("R/notify.R")
#   send_notification("success", list(airports_count = 5312, ...))
#
# Environment variables:
#   NOTIFY_EMAIL_ENABLED, NOTIFY_SMS_ENABLED - "true" or "false"
#   NOTIFY_EMAIL, NOTIFY_PHONE - recipient contact info
#   TWILIO_ACCOUNT_SID, TWILIO_AUTH_TOKEN, TWILIO_FROM_NUMBER - for SMS
#   SENDGRID_API_KEY - for email
#   DRY_RUN - if "true", print instead of sending

library(httr2)
library(checkmate)
library(rlang)

#' Get notification configuration from environment
#'
#' @return Named list with email_enabled, sms_enabled, email, phone
#' @keywords internal
get_notify_config <- function() {
  list(
    email_enabled = identical(Sys.getenv("NOTIFY_EMAIL_ENABLED"), "true"),
    sms_enabled = identical(Sys.getenv("NOTIFY_SMS_ENABLED"), "true"),
    email = Sys.getenv("NOTIFY_EMAIL", unset = ""),
    phone = Sys.getenv("NOTIFY_PHONE", unset = ""),
    dry_run = identical(Sys.getenv("DRY_RUN"), "true")
  )
}

#' Check if dry run mode is enabled
#'
#' @return Logical
#' @keywords internal
is_dry_run <- function() {
  identical(Sys.getenv("DRY_RUN"), "true")
}
```

**Step 2: Run tests (should still fail - need format/send functions)**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-notify.R')"`

Expected: Some tests pass, some fail

**Step 3: Commit partial implementation**

```bash
git add R/notify.R
git commit -m "feat(notify): add notification config functions"
```

---

## Task 8: Implement format_notification()

**Files:**
- Modify: `R/notify.R`

**Step 1: Add format_notification function**

Append to `R/notify.R`:

```r
#' Format notification message based on type
#'
#' @param type One of "success", "failure", "reminder", "warning"
#' @param data Named list with relevant data for the message type
#' @return Named list with subject and body
#' @export
format_notification <- function(type = c("success", "failure", "reminder", "warning"),
                                 data = list()) {
  type <- match.arg(type)
  checkmate::assert_list(data)

  switch(type,
    success = format_success_message(data),
    failure = format_failure_message(data),
    reminder = format_reminder_message(data),
    warning = format_warning_message(data)
  )
}

#' @keywords internal
format_success_message <- function(data) {
  checkmate::assert_names(
    names(data),
    must.include = c("faa_eff_date", "airports_count", "navaids_count", "next_expected_date")
  )

  subject <- sprintf(
    "[FAA NASR] Update Complete - %s",
    format(data$faa_eff_date, "%B %d, %Y")
  )

  body <- sprintf(
    "FAA NASR data has been updated.
New effective date: %s
Records loaded:
  - Airports: %s
  - Navaids: %s

Next update expected: %s

Database: Supabase",
    format(data$faa_eff_date, "%B %d, %Y"),
    format(data$airports_count, big.mark = ","),
    format(data$navaids_count, big.mark = ","),
    format(data$next_expected_date, "%B %d, %Y")
  )

  list(subject = subject, body = body)
}

#' @keywords internal
format_failure_message <- function(data) {
  checkmate::assert_names(
    names(data),
    must.include = c("error_message")
  )

  stage <- data$stage %||% "unknown"
  logs_url <- data$logs_url %||% "Check GitHub Actions"

  subject <- "[FAA NASR] Pipeline Failed - Action Required"

  body <- sprintf(
    "The NASR update pipeline encountered an error.
Error: %s
Stage: %s

View logs: %s

This requires manual investigation.",
    data$error_message,
    stage,
    logs_url
  )

  list(subject = subject, body = body)
}

#' @keywords internal
format_reminder_message <- function(data) {
  checkmate::assert_names(
    names(data),
    must.include = c("preview_date")
  )

  subject <- "[FAA NASR] Update Expected Tomorrow"

  body <- sprintf(
    "The next FAA NASR subscription is expected tomorrow.
Expected date: %s
Pipeline will run automatically.

No action needed - you'll receive confirmation when complete.",
    format(data$preview_date, "%B %d, %Y")
  )

  list(subject = subject, body = body)
}

#' @keywords internal
format_warning_message <- function(data) {
  subject <- "[FAA NASR] Update Warning - Review Needed"

  body <- sprintf(
    "Pipeline completed but with warnings.
Expected airports: ~5,300 | Loaded: %s
Expected navaids: ~1,650 | Loaded: %s

This may indicate a data issue. Please verify.",
    format(data$airports_count %||% 0, big.mark = ","),
    format(data$navaids_count %||% 0, big.mark = ",")
  )

  list(subject = subject, body = body)
}
```

**Step 2: Run tests**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-notify.R')"`

Expected: format_notification tests PASS

**Step 3: Commit**

```bash
git add R/notify.R
git commit -m "feat(notify): add format_notification() with message templates"
```

---

## Task 9: Implement send_email()

**Files:**
- Modify: `R/notify.R`

**Step 1: Add send_email function**

Append to `R/notify.R`:

```r
#' Send email via SendGrid API
#'
#' @param subject Email subject line
#' @param body Email body text
#' @return TRUE on success, FALSE on failure
#' @export
send_email <- function(subject, body) {
  checkmate::assert_string(subject, min.chars = 1)
  checkmate::assert_string(body, min.chars = 1)

  config <- get_notify_config()

  if (config$dry_run) {
    message("DRY RUN - Would send email:")
    message("  To: ", config$email)
    message("  Subject: ", subject)
    message("  Body: ", substr(body, 1, 100), "...")
    return(TRUE)
  }

  api_key <- Sys.getenv("SENDGRID_API_KEY")
  if (api_key == "") {
    rlang::warn("SENDGRID_API_KEY not set, skipping email")
    return(FALSE)
  }

  if (config$email == "") {
    rlang::warn("NOTIFY_EMAIL not set, skipping email")
    return(FALSE)
  }

  from_email <- Sys.getenv("SENDGRID_FROM_EMAIL", unset = "noreply@example.com")

  payload <- list(
    personalizations = list(
      list(to = list(list(email = config$email)))
    ),
    from = list(email = from_email),
    subject = subject,
    content = list(
      list(type = "text/plain", value = body)
    )
  )

  resp <- request("https://api.sendgrid.com/v3/mail/send") |>
    req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(payload) |>
    req_method("POST") |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  status <- resp_status(resp)
  if (status >= 400) {
    rlang::warn(
      c(
        "Failed to send email via SendGrid",
        x = paste("HTTP", status)
      )
    )
    return(FALSE)
  }

  message("Email sent successfully to ", config$email)
  TRUE
}
```

**Step 2: Run tests**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-notify.R')"`

Expected: send_email tests PASS

**Step 3: Commit**

```bash
git add R/notify.R
git commit -m "feat(notify): add send_email() with SendGrid integration"
```

---

## Task 10: Implement send_sms()

**Files:**
- Modify: `R/notify.R`

**Step 1: Add send_sms function**

Append to `R/notify.R`:

```r
#' Send SMS via Twilio API
#'
#' @param message SMS message text (max 1600 chars)
#' @return TRUE on success, FALSE on failure
#' @export
send_sms <- function(message) {
  checkmate::assert_string(message, min.chars = 1, max.chars = 1600)

  config <- get_notify_config()

  if (config$dry_run) {
    message("DRY RUN - Would send SMS:")
    message("  To: ", config$phone)
    message("  Message: ", substr(message, 1, 100), "...")
    return(TRUE)
  }

  account_sid <- Sys.getenv("TWILIO_ACCOUNT_SID")
  auth_token <- Sys.getenv("TWILIO_AUTH_TOKEN")
  from_number <- Sys.getenv("TWILIO_FROM_NUMBER")

  if (account_sid == "" || auth_token == "") {
    rlang::warn("Twilio credentials not set, skipping SMS")
    return(FALSE)
  }

  if (config$phone == "") {
    rlang::warn("NOTIFY_PHONE not set, skipping SMS")
    return(FALSE)
  }

  url <- sprintf(
    "https://api.twilio.com/2010-04-01/Accounts/%s/Messages.json",
    account_sid
  )

  resp <- request(url) |>
    req_auth_basic(account_sid, auth_token) |>
    req_body_form(
      To = config$phone,
      From = from_number,
      Body = message
    ) |>
    req_method("POST") |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  status <- resp_status(resp)
  if (status >= 400) {
    body <- resp_body_string(resp)
    rlang::warn(
      c(
        "Failed to send SMS via Twilio",
        x = paste("HTTP", status),
        i = body
      )
    )
    return(FALSE)
  }

  message("SMS sent successfully to ", config$phone)
  TRUE
}
```

**Step 2: Run tests**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-notify.R')"`

Expected: send_sms tests PASS

**Step 3: Commit**

```bash
git add R/notify.R
git commit -m "feat(notify): add send_sms() with Twilio integration"
```

---

## Task 11: Implement send_notification()

**Files:**
- Modify: `R/notify.R`

**Step 1: Add send_notification function**

Append to `R/notify.R`:

```r
#' Send notification via configured channels
#'
#' Formats and sends a notification via email and/or SMS based on
#' environment configuration.
#'
#' @param type One of "success", "failure", "reminder", "warning"
#' @param data Named list with data for the message
#' @return TRUE if at least one notification sent, FALSE if none sent
#' @export
send_notification <- function(type, data) {
  config <- get_notify_config()
  msg <- format_notification(type, data)

  sent_any <- FALSE

  if (config$email_enabled) {
    if (send_email(msg$subject, msg$body)) {
      sent_any <- TRUE
    }
  }

  if (config$sms_enabled) {
    # SMS gets a condensed version
    sms_text <- paste(msg$subject, "\n\n", substr(msg$body, 1, 300))
    if (nchar(msg$body) > 300) {
      sms_text <- paste0(sms_text, "...")
    }
    if (send_sms(sms_text)) {
      sent_any <- TRUE
    }
  }

  if (!config$email_enabled && !config$sms_enabled) {
    message("No notification channels enabled")
  }

  sent_any
}
```

**Step 2: Run all notify tests**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-notify.R')"`

Expected: All tests PASS

**Step 3: Run lintr**

Run: `Rscript -e "lintr::lint('R/notify.R')"`

Expected: No errors (or only style warnings)

**Step 4: Commit**

```bash
git add R/notify.R
git commit -m "feat(notify): add send_notification() dispatcher"
```

---

## Task 12: Write Tests for log_to_supabase()

**Files:**
- Modify: `tests/testthat/test-notify.R`

**Step 1: Add logging tests**

Append to `tests/testthat/test-notify.R`:

```r
# --- Tests for log_to_supabase() ---

test_that("log_to_supabase validates log_data argument", {
  expect_error(
    log_to_supabase("not a list"),
    class = "simpleError"
  )
})

test_that("log_to_supabase requires status field", {
  expect_error(
    log_to_supabase(list(airports_count = 100)),
    class = "simpleError"
  )
})

test_that("log_to_supabase returns TRUE in dry_run mode", {
  withr::local_envvar(DRY_RUN = "true")

  log_data <- list(
    status = "success",
    airports_count = 5312,
    navaids_count = 1658,
    faa_eff_date = as.Date("2026-02-19"),
    next_expected_date = as.Date("2026-03-19"),
    duration_seconds = 45,
    triggered_by = "local"
  )

  result <- log_to_supabase(log_data)

  expect_true(result)
})

# --- Tests for append_to_csv_log() ---

test_that("append_to_csv_log creates file if not exists", {
  temp_file <- withr::local_tempfile(fileext = ".csv")

  log_data <- list(
    run_timestamp = Sys.time(),
    status = "success",
    airports_count = 5312,
    navaids_count = 1658,
    faa_eff_date = as.Date("2026-02-19"),
    next_expected_date = as.Date("2026-03-19"),
    error_message = NA,
    duration_seconds = 45,
    triggered_by = "local"
  )

  result <- append_to_csv_log(log_data, csv_path = temp_file)

  expect_true(result)
  expect_true(file.exists(temp_file))

  # Verify contents
  contents <- readr::read_csv(temp_file, show_col_types = FALSE)
  expect_equal(nrow(contents), 1)
  expect_equal(contents$status, "success")
})

test_that("append_to_csv_log appends to existing file", {
  temp_file <- withr::local_tempfile(fileext = ".csv")

  # Create initial entry
  log_data1 <- list(
    run_timestamp = Sys.time() - 3600,
    status = "success",
    airports_count = 5310,
    navaids_count = 1657,
    faa_eff_date = as.Date("2026-01-22"),
    next_expected_date = as.Date("2026-02-19"),
    error_message = NA,
    duration_seconds = 42,
    triggered_by = "scheduled"
  )
  append_to_csv_log(log_data1, csv_path = temp_file)

  # Append second entry
  log_data2 <- list(
    run_timestamp = Sys.time(),
    status = "success",
    airports_count = 5312,
    navaids_count = 1658,
    faa_eff_date = as.Date("2026-02-19"),
    next_expected_date = as.Date("2026-03-19"),
    error_message = NA,
    duration_seconds = 45,
    triggered_by = "scheduled"
  )
  append_to_csv_log(log_data2, csv_path = temp_file)

  contents <- readr::read_csv(temp_file, show_col_types = FALSE)
  expect_equal(nrow(contents), 2)
})
```

**Step 2: Run tests to verify failure**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-notify.R')"`

Expected: New tests FAIL

**Step 3: Commit**

```bash
git add tests/testthat/test-notify.R
git commit -m "test(notify): add failing tests for logging functions"
```

---

## Task 13: Implement log_to_supabase() and append_to_csv_log()

**Files:**
- Modify: `R/notify.R`

**Step 1: Add logging functions**

Append to `R/notify.R`:

```r
#' Log pipeline run to Supabase
#'
#' @param log_data Named list with run data
#' @return TRUE on success, FALSE on failure
#' @export
log_to_supabase <- function(log_data) {
  checkmate::assert_list(log_data)
  checkmate::assert_names(names(log_data), must.include = "status")

  if (is_dry_run()) {
    message("DRY RUN - Would log to Supabase:")
    message("  Status: ", log_data$status)
    message("  Airports: ", log_data$airports_count %||% "N/A")
    message("  Navaids: ", log_data$navaids_count %||% "N/A")
    return(TRUE)
  }

  # Load Supabase config (reuse from push_to_supabase.R)
  api_key <- Sys.getenv("SUPABASE_API_KEY")
  if (api_key == "") {
    rlang::warn("SUPABASE_API_KEY not set, skipping database log")
    return(FALSE)
  }

  supabase_url <- Sys.getenv(
    "SUPABASE_URL",
    unset = "https://bjmjxipflycjnrwdujxp.supabase.co"
  )

  # Prepare payload
  payload <- list(
    status = log_data$status,
    airports_count = log_data$airports_count,
    navaids_count = log_data$navaids_count,
    faa_eff_date = format(log_data$faa_eff_date, "%Y-%m-%d"),
    next_expected_date = format(log_data$next_expected_date, "%Y-%m-%d"),
    error_message = log_data$error_message,
    duration_seconds = log_data$duration_seconds,
    triggered_by = log_data$triggered_by %||% "local"
  )

  # Remove NULL values
  payload <- payload[!vapply(payload, is.null, logical(1))]

  resp <- request(paste0(supabase_url, "/rest/v1/pipeline_logs")) |>
    req_headers(
      "apikey" = api_key,
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json",
      "Prefer" = "return=minimal"
    ) |>
    req_body_json(payload) |>
    req_method("POST") |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  status <- resp_status(resp)
  if (status >= 400) {
    body <- resp_body_string(resp)
    rlang::warn(
      c(
        "Failed to log to Supabase",
        x = paste("HTTP", status),
        i = body
      )
    )
    return(FALSE)
  }

  message("Logged to Supabase pipeline_logs table")
  TRUE
}

#' Append log entry to CSV file
#'
#' @param log_data Named list with run data
#' @param csv_path Path to CSV file (default: data/pipeline_history.csv)
#' @return TRUE on success
#' @export
append_to_csv_log <- function(log_data,
                               csv_path = "data/pipeline_history.csv") {
  checkmate::assert_list(log_data)
  checkmate::assert_string(csv_path)

  # Ensure run_timestamp exists
  if (is.null(log_data$run_timestamp)) {
    log_data$run_timestamp <- Sys.time()
  }

  # Create tibble row
  log_row <- tibble::tibble(
    run_timestamp = format(log_data$run_timestamp, "%Y-%m-%dT%H:%M:%SZ"),
    status = log_data$status,
    airports_count = log_data$airports_count %||% NA_integer_,
    navaids_count = log_data$navaids_count %||% NA_integer_,
    faa_eff_date = format(log_data$faa_eff_date %||% NA, "%Y-%m-%d"),
    next_expected_date = format(log_data$next_expected_date %||% NA, "%Y-%m-%d"),
    error_message = log_data$error_message %||% NA_character_,
    duration_seconds = log_data$duration_seconds %||% NA_integer_,
    triggered_by = log_data$triggered_by %||% "local"
  )

  # Append to file (create if needed)
  if (file.exists(csv_path)) {
    readr::write_csv(log_row, csv_path, append = TRUE, col_names = FALSE)
  } else {
    readr::write_csv(log_row, csv_path)
  }

  message("Appended to ", csv_path)
  TRUE
}

#' Log pipeline run to both Supabase and CSV
#'
#' @param log_data Named list with run data
#' @return TRUE if at least one succeeded
#' @export
log_pipeline_run <- function(log_data) {
  supabase_ok <- log_to_supabase(log_data)
  csv_ok <- append_to_csv_log(log_data)

  supabase_ok || csv_ok
}
```

**Step 2: Run tests**

Run: `Rscript -e "testthat::test_file('tests/testthat/test-notify.R')"`

Expected: All tests PASS

**Step 3: Run lintr**

Run: `Rscript -e "lintr::lint('R/notify.R')"`

Expected: No errors

**Step 4: Commit**

```bash
git add R/notify.R tests/testthat/test-notify.R
git commit -m "feat(notify): add log_to_supabase() and append_to_csv_log()"
```

---

## Task 14: Enhance run_pipeline() Return Value

**Files:**
- Modify: `R/scrape_airports_navaids.R:165-243`

**Step 1: Update run_pipeline to return structured result**

Replace the `run_pipeline()` function with:

```r
#' Run the full FAA data pipeline
#'
#' Checks for updates, downloads if newer, cleans data, and pushes to Supabase.
#'
#' @param force Logical: if TRUE, download regardless of date comparison
#' @param dry_run Logical: if TRUE, skip actual downloads and pushes
#' @return Named list with status, counts, dates, and timing
#' @export
run_pipeline <- function(force = FALSE, dry_run = FALSE) {
  checkmate::assert_flag(force)
  checkmate::assert_flag(dry_run)

  start_time <- Sys.time()
  result <- list(
    status = "success",
    airports_count = NA_integer_,
    navaids_count = NA_integer_,
    faa_eff_date = NA,
    next_expected_date = NA,
    error_message = NA_character_,
    duration_seconds = NA_integer_,
    triggered_by = if (Sys.getenv("GITHUB_ACTIONS") == "true") "scheduled" else "local"
  )

  # Get local and remote dates
  local_date <- get_local_data_date("data/raw")
  faa_dates <- get_faa_dates()
  current_date <- faa_dates$current_date
  result$next_expected_date <- faa_dates$preview_date

  # Decide whether to update
  needs_update <- force || is.na(local_date) || (current_date > local_date)

  if (!needs_update) {
    message("Data is already up to date. No download needed.")
    result$status <- "no_update"
    result$duration_seconds <- as.integer(difftime(Sys.time(), start_time, units = "secs"))
    return(result)
  }

  if (dry_run) {
    message("DRY RUN - Would download and process data")
    result$status <- "dry_run"
    result$faa_eff_date <- current_date
    result$duration_seconds <- as.integer(difftime(Sys.time(), start_time, units = "secs"))
    return(result)
  }

  if (force) {
    message("Force flag set - downloading regardless of date...")
  } else {
    message("New data available! Downloading...")
  }

  # Delete old data
  delete_old_data("data/raw")

  # Download both datasets
  apt_dir <- download_faa_data(current_date, "APT", "data/raw")
  nav_dir <- download_faa_data(current_date, "NAV", "data/raw")

  # Source the cleaning and push scripts (functions only, no execution)
  source("R/clean_data.R", local = FALSE)
  source("R/push_to_supabase.R", local = FALSE)

  # Run data cleaning
  message("Running data cleaning...")
  dirs <- find_raw_data_dirs("data/raw")

  apt_path <- file.path(dirs$apt_dir, "APT_BASE.csv")
  airports <- clean_airports(apt_path)
  validate_cleaned_data(airports, "airports")
  result$airports_count <- nrow(airports)

  # Ensure clean directory exists
  if (!dir.exists("data/clean")) {
    dir.create("data/clean", recursive = TRUE)
  }

  readr::write_csv(airports, "data/clean/airports.csv")
  message("Wrote ", nrow(airports), " airports to data/clean/airports.csv")

  nav_path <- file.path(dirs$nav_dir, "NAV_BASE.csv")
  navaids <- clean_navaids(nav_path)
  validate_cleaned_data(navaids, "navaids")
  result$navaids_count <- nrow(navaids)

  readr::write_csv(navaids, "data/clean/navaids.csv")
  message("Wrote ", nrow(navaids), " navaids to data/clean/navaids.csv")

  # Remove extra files
  remove_extra_files(dirs$apt_dir, dirs$nav_dir)

  # Push to Supabase
  message("Pushing data to Supabase...")
  airports_lower <- airports |> dplyr::rename_with(tolower)
  navaids_lower <- navaids |> dplyr::rename_with(tolower)

  clear_table("airports")
  push_to_supabase("airports", airports_lower)

  clear_table("navaids")
  push_to_supabase("navaids", navaids_lower)

  result$faa_eff_date <- current_date
  result$duration_seconds <- as.integer(difftime(Sys.time(), start_time, units = "secs"))

  message("Done! Data updated to ", format(current_date, "%d %b %Y"))
  result
}
```

**Step 2: Run all tests**

Run: `Rscript -e "testthat::test_dir('tests/testthat')"`

Expected: All tests PASS

**Step 3: Run lintr**

Run: `Rscript -e "lintr::lint('R/scrape_airports_navaids.R')"`

Expected: No errors

**Step 4: Commit**

```bash
git add R/scrape_airports_navaids.R
git commit -m "feat(scrape): enhance run_pipeline() to return structured result"
```

---

## Task 15: Create GitHub Actions Workflow

**Files:**
- Create: `.github/workflows/nasr-autoscrape.yml`

**Step 1: Write the workflow file**

```yaml
name: NASR Auto-Scrape

on:
  schedule:
    # Daily at 12:00 UTC
    - cron: '0 12 * * *'
  workflow_dispatch:
    inputs:
      force:
        description: 'Force download even if data is current'
        required: false
        default: 'false'
        type: boolean

jobs:
  check-schedule:
    runs-on: ubuntu-latest
    outputs:
      run_pipeline: ${{ steps.check.outputs.run_pipeline }}
      send_reminder: ${{ steps.check.outputs.send_reminder }}
      current_date: ${{ steps.check.outputs.current_date }}
      preview_date: ${{ steps.check.outputs.preview_date }}
    steps:
      - name: Check FAA NASR dates
        id: check
        run: |
          # Fetch FAA page and extract dates
          PAGE=$(curl -s "https://www.faa.gov/air_traffic/flight_info/aeronav/aero_data/NASR_Subscription/")

          # Extract current date
          CURRENT=$(echo "$PAGE" | grep -oP 'Current[^A-Za-z]*Subscription effective \K[A-Za-z]+ \d{1,2}, \d{4}' | head -1)
          # Extract preview date
          PREVIEW=$(echo "$PAGE" | grep -oP 'Preview[^A-Za-z]*Subscription effective \K[A-Za-z]+ \d{1,2}, \d{4}' | head -1)

          echo "Current FAA date: $CURRENT"
          echo "Preview FAA date: $PREVIEW"

          # Convert to YYYY-MM-DD
          CURRENT_ISO=$(date -d "$CURRENT" +%Y-%m-%d 2>/dev/null || echo "")
          PREVIEW_ISO=$(date -d "$PREVIEW" +%Y-%m-%d 2>/dev/null || echo "")
          TODAY=$(date +%Y-%m-%d)
          TOMORROW=$(date -d "+1 day" +%Y-%m-%d)
          YESTERDAY=$(date -d "-1 day" +%Y-%m-%d)

          echo "current_date=$CURRENT_ISO" >> $GITHUB_OUTPUT
          echo "preview_date=$PREVIEW_ISO" >> $GITHUB_OUTPUT

          # Check if we should run pipeline (within ±1 day of preview date)
          if [[ "$TODAY" == "$PREVIEW_ISO" ]] || [[ "$YESTERDAY" == "$PREVIEW_ISO" ]] || [[ "$TOMORROW" == "$PREVIEW_ISO" ]]; then
            echo "run_pipeline=true" >> $GITHUB_OUTPUT
            echo "Pipeline window is OPEN"
          else
            echo "run_pipeline=false" >> $GITHUB_OUTPUT
            echo "Pipeline window is closed"
          fi

          # Check if we should send reminder (tomorrow is preview date)
          if [[ "$TOMORROW" == "$PREVIEW_ISO" ]]; then
            echo "send_reminder=true" >> $GITHUB_OUTPUT
            echo "Will send reminder"
          else
            echo "send_reminder=false" >> $GITHUB_OUTPUT
          fi

  send-reminder:
    needs: check-schedule
    if: needs.check-schedule.outputs.send_reminder == 'true'
    runs-on: ubuntu-latest
    steps:
      - name: Send reminder notification
        env:
          NOTIFY_EMAIL_ENABLED: ${{ secrets.NOTIFY_EMAIL_ENABLED }}
          NOTIFY_SMS_ENABLED: ${{ secrets.NOTIFY_SMS_ENABLED }}
          NOTIFY_EMAIL: ${{ secrets.NOTIFY_EMAIL }}
          NOTIFY_PHONE: ${{ secrets.NOTIFY_PHONE }}
          TWILIO_ACCOUNT_SID: ${{ secrets.TWILIO_ACCOUNT_SID }}
          TWILIO_AUTH_TOKEN: ${{ secrets.TWILIO_AUTH_TOKEN }}
          TWILIO_FROM_NUMBER: ${{ secrets.TWILIO_FROM_NUMBER }}
          SENDGRID_API_KEY: ${{ secrets.SENDGRID_API_KEY }}
          PREVIEW_DATE: ${{ needs.check-schedule.outputs.preview_date }}
        run: |
          SUBJECT="[FAA NASR] Update Expected Tomorrow"
          BODY="The next FAA NASR subscription is expected tomorrow.\n\nExpected date: $PREVIEW_DATE\nPipeline will run automatically.\n\nNo action needed."

          # Send email if enabled
          if [[ "$NOTIFY_EMAIL_ENABLED" == "true" ]] && [[ -n "$SENDGRID_API_KEY" ]]; then
            curl -X POST "https://api.sendgrid.com/v3/mail/send" \
              -H "Authorization: Bearer $SENDGRID_API_KEY" \
              -H "Content-Type: application/json" \
              -d "{\"personalizations\":[{\"to\":[{\"email\":\"$NOTIFY_EMAIL\"}]}],\"from\":{\"email\":\"noreply@faa-nasr.example.com\"},\"subject\":\"$SUBJECT\",\"content\":[{\"type\":\"text/plain\",\"value\":\"$BODY\"}]}"
            echo "Email sent"
          fi

          # Send SMS if enabled
          if [[ "$NOTIFY_SMS_ENABLED" == "true" ]] && [[ -n "$TWILIO_ACCOUNT_SID" ]]; then
            curl -X POST "https://api.twilio.com/2010-04-01/Accounts/$TWILIO_ACCOUNT_SID/Messages.json" \
              -u "$TWILIO_ACCOUNT_SID:$TWILIO_AUTH_TOKEN" \
              -d "To=$NOTIFY_PHONE" \
              -d "From=$TWILIO_FROM_NUMBER" \
              -d "Body=$SUBJECT - $PREVIEW_DATE"
            echo "SMS sent"
          fi

  run-pipeline:
    needs: check-schedule
    if: needs.check-schedule.outputs.run_pipeline == 'true' || github.event.inputs.force == 'true'
    runs-on: ubuntu-latest
    steps:
      - name: Checkout repository
        uses: actions/checkout@v4

      - name: Setup R
        uses: r-lib/actions/setup-r@v2
        with:
          r-version: '4.3'

      - name: Setup renv
        uses: r-lib/actions/setup-renv@v2

      - name: Run pipeline
        id: pipeline
        env:
          SUPABASE_API_KEY: ${{ secrets.SUPABASE_API_KEY }}
          NOTIFY_EMAIL_ENABLED: ${{ secrets.NOTIFY_EMAIL_ENABLED }}
          NOTIFY_SMS_ENABLED: ${{ secrets.NOTIFY_SMS_ENABLED }}
          NOTIFY_EMAIL: ${{ secrets.NOTIFY_EMAIL }}
          NOTIFY_PHONE: ${{ secrets.NOTIFY_PHONE }}
          TWILIO_ACCOUNT_SID: ${{ secrets.TWILIO_ACCOUNT_SID }}
          TWILIO_AUTH_TOKEN: ${{ secrets.TWILIO_AUTH_TOKEN }}
          TWILIO_FROM_NUMBER: ${{ secrets.TWILIO_FROM_NUMBER }}
          SENDGRID_API_KEY: ${{ secrets.SENDGRID_API_KEY }}
        run: |
          Rscript -e "
          source('R/scrape_airports_navaids.R')
          source('R/notify.R')

          result <- run_pipeline(force = ${{ github.event.inputs.force || 'FALSE' }})

          # Log the run
          log_pipeline_run(result)

          # Send notification based on status
          if (result\$status == 'success') {
            send_notification('success', result)
          } else if (result\$status == 'no_update') {
            message('No update needed, skipping notification')
          }
          "

      - name: Commit pipeline history
        if: success()
        run: |
          git config user.name "github-actions[bot]"
          git config user.email "github-actions[bot]@users.noreply.github.com"
          git add data/pipeline_history.csv
          git diff --staged --quiet || git commit -m "data: log pipeline run $(date +%Y-%m-%d)"
          git push

      - name: Send failure notification
        if: failure()
        env:
          NOTIFY_EMAIL_ENABLED: ${{ secrets.NOTIFY_EMAIL_ENABLED }}
          NOTIFY_SMS_ENABLED: ${{ secrets.NOTIFY_SMS_ENABLED }}
          NOTIFY_EMAIL: ${{ secrets.NOTIFY_EMAIL }}
          NOTIFY_PHONE: ${{ secrets.NOTIFY_PHONE }}
          TWILIO_ACCOUNT_SID: ${{ secrets.TWILIO_ACCOUNT_SID }}
          TWILIO_AUTH_TOKEN: ${{ secrets.TWILIO_AUTH_TOKEN }}
          TWILIO_FROM_NUMBER: ${{ secrets.TWILIO_FROM_NUMBER }}
          SENDGRID_API_KEY: ${{ secrets.SENDGRID_API_KEY }}
          GITHUB_RUN_URL: ${{ github.server_url }}/${{ github.repository }}/actions/runs/${{ github.run_id }}
        run: |
          SUBJECT="[FAA NASR] Pipeline Failed - Action Required"
          BODY="The NASR update pipeline encountered an error.\n\nView logs: $GITHUB_RUN_URL\n\nThis requires manual investigation."

          if [[ "$NOTIFY_EMAIL_ENABLED" == "true" ]] && [[ -n "$SENDGRID_API_KEY" ]]; then
            curl -X POST "https://api.sendgrid.com/v3/mail/send" \
              -H "Authorization: Bearer $SENDGRID_API_KEY" \
              -H "Content-Type: application/json" \
              -d "{\"personalizations\":[{\"to\":[{\"email\":\"$NOTIFY_EMAIL\"}]}],\"from\":{\"email\":\"noreply@faa-nasr.example.com\"},\"subject\":\"$SUBJECT\",\"content\":[{\"type\":\"text/plain\",\"value\":\"$BODY\"}]}"
          fi

          if [[ "$NOTIFY_SMS_ENABLED" == "true" ]] && [[ -n "$TWILIO_ACCOUNT_SID" ]]; then
            curl -X POST "https://api.twilio.com/2010-04-01/Accounts/$TWILIO_ACCOUNT_SID/Messages.json" \
              -u "$TWILIO_ACCOUNT_SID:$TWILIO_AUTH_TOKEN" \
              -d "To=$NOTIFY_PHONE" \
              -d "From=$TWILIO_FROM_NUMBER" \
              -d "Body=$SUBJECT - Check GitHub Actions"
          fi
```

**Step 2: Commit**

```bash
git add .github/workflows/nasr-autoscrape.yml
git commit -m "feat(ci): add NASR auto-scrape workflow with notifications"
```

---

## Task 16: Update README with Auto-Scrape Documentation

**Files:**
- Modify: `README.md`

**Step 1: Add auto-scrape section**

Add after the "API Access" section in README.md:

```markdown
---

## Automated Updates

The pipeline can run automatically via GitHub Actions.

### How It Works

1. **Daily check** at 12:00 UTC - scrapes FAA website for current and preview dates
2. **Update window** - full pipeline runs when within ±1 day of new data release
3. **Notifications** - email and/or SMS alerts for success, failure, and reminders

### Setup

1. **Create accounts:**
   - Twilio (SMS): https://www.twilio.com/try-twilio
   - SendGrid (Email): Access via Twilio Console

2. **Add GitHub Secrets** (Settings → Secrets → Actions):

   | Secret | Description |
   |--------|-------------|
   | `NOTIFY_EMAIL_ENABLED` | `true` or `false` |
   | `NOTIFY_SMS_ENABLED` | `true` or `false` |
   | `NOTIFY_EMAIL` | Your email address |
   | `NOTIFY_PHONE` | Your phone (+1...) |
   | `TWILIO_ACCOUNT_SID` | From Twilio console |
   | `TWILIO_AUTH_TOKEN` | From Twilio console |
   | `TWILIO_FROM_NUMBER` | Your Twilio number |
   | `SENDGRID_API_KEY` | From SendGrid |
   | `SUPABASE_API_KEY` | Already configured |

3. **Enable workflow:**
   - Go to Actions tab → Enable workflows
   - Optionally trigger manually via "Run workflow"

### Notification Types

| Type | When | Content |
|------|------|---------|
| Success | Pipeline completes | Record counts, next expected date |
| Failure | Pipeline error | Error details, link to logs |
| Reminder | 1 day before update | "Update expected tomorrow" |
```

**Step 2: Commit**

```bash
git add README.md
git commit -m "docs: add auto-scrape and notification setup guide"
```

---

## Task 17: Run Full Test Suite and Lint

**Step 1: Run all tests**

Run: `Rscript -e "testthat::test_dir('tests/testthat')"`

Expected: All tests PASS

**Step 2: Run lintr on all R files**

Run: `Rscript -e "lintr::lint_dir('R')"`

Expected: No errors

**Step 3: Verify workflow syntax**

Run: `cat .github/workflows/nasr-autoscrape.yml | head -20`

Expected: Valid YAML

---

## Task 18: Final Commit and Summary

**Step 1: Check git status**

Run: `git status`

Expected: Clean working tree

**Step 2: View commit history**

Run: `git log --oneline -15`

Review all commits from this implementation.

---

## Verification Checklist

After implementation, verify:

1. [ ] `sql/create_pipeline_logs.sql` exists and is valid SQL
2. [ ] `data/pipeline_history.csv` exists with headers
3. [ ] `.Renviron.example` has all notification variables
4. [ ] `R/notify.R` has all functions: `get_notify_config`, `format_notification`, `send_email`, `send_sms`, `send_notification`, `log_to_supabase`, `append_to_csv_log`, `log_pipeline_run`
5. [ ] `R/scrape_airports_navaids.R` has `get_faa_dates()` and enhanced `run_pipeline()`
6. [ ] `tests/testthat/test-notify.R` exists with passing tests
7. [ ] `.github/workflows/nasr-autoscrape.yml` exists
8. [ ] `lintr::lint_dir('R')` reports no errors
9. [ ] `testthat::test_dir('tests/testthat')` all pass
10. [ ] README.md documents auto-scrape setup

## Post-Implementation

1. Run SQL in Supabase SQL Editor to create `pipeline_logs` table
2. Add GitHub Secrets for notifications
3. Manually trigger workflow via Actions tab to test
4. Verify notification received
