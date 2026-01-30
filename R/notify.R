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
