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
library(tibble)
library(readr)

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
    next_expected_date = format(
      log_data$next_expected_date %||% NA, "%Y-%m-%d"
    ),
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
