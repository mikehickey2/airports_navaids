# push_to_supabase.R
# Functions for pushing data to Supabase via REST API
#
# Usage:
#   source("R/push_to_supabase.R")
#   push_to_supabase("airports", airports_df)
#   clear_table("airports")
#
# Or run directly to push current clean data:
#   Rscript R/push_to_supabase.R

library(httr2)
library(dplyr)
library(readr)
library(jsonlite)
library(checkmate)
library(rlang)

# --- Configuration ---
# Default Supabase URL (can be overridden via SUPABASE_URL env var)
default_supabase_url <- "https://bjmjxipflycjnrwdujxp.supabase.co"

#' Get Supabase configuration from environment
#'
#' @return Named list with url and api_key
#' @keywords internal
get_supabase_config <- function() {
  # Try to load .Renviron if API key not set

  if (Sys.getenv("SUPABASE_API_KEY") == "") {
    renviron_path <- file.path(getwd(), ".Renviron")
    if (file.exists(renviron_path)) {
      readRenviron(renviron_path)
    }
  }

  api_key <- Sys.getenv("SUPABASE_API_KEY")
  if (api_key == "") {
    rlang::abort(
      c(
        "Missing Supabase credentials",
        i = "Set SUPABASE_API_KEY in .Renviron or as environment variable"
      ),
      class = "supabase_auth_error"
    )
  }

  url <- Sys.getenv("SUPABASE_URL", unset = default_supabase_url)

  list(url = url, api_key = api_key)
}

#' Push data to a Supabase table via REST API
#'
#' @param table_name Character string: name of the Supabase table
#' @param data Data frame to push
#' @param batch_size Integer: number of rows per batch (default 100, max 5000)
#'
#' @return Invisibly returns TRUE on success
#' @export
push_to_supabase <- function(table_name, data, batch_size = 100L) {
  # --- Input validation ---
  checkmate::assert_string(table_name, min.chars = 1)
  checkmate::assert_data_frame(data, min.rows = 1)
  checkmate::assert_integerish(batch_size, lower = 1, upper = 5000, len = 1)

  batch_size <- as.integer(batch_size)

  # Log environment info for CI debugging
  if (Sys.getenv("CI") != "") {
    message("CI Environment detected")
    message("  R version: ", R.version.string)
    message("  Locale: ", Sys.getlocale("LC_CTYPE"))
    message("  jsonlite version: ", packageVersion("jsonlite"))
    message("  httr2 version: ", packageVersion("httr2"))
  }

  # Get configuration
  config <- get_supabase_config()

  # CI DEBUG: Test with hardcoded minimal JSON first
  if (Sys.getenv("CI") != "" && table_name == "airports") {
    message("DEBUG: Testing connection with minimal hardcoded JSON...")
    test_json <- '[{"eff_date":"2026-01-22","site_no":"TEST","arpt_id":"XXX"}]'
    tmp_test <- tempfile(fileext = ".json")
    writeLines(test_json, tmp_test)
    message("  Test JSON: ", test_json)
    message("  Test file size: ", file.info(tmp_test)$size, " bytes")

    test_cmd <- sprintf(
      'curl -v -X POST "%s/rest/v1/%s" -H "apikey: %s" -H "Authorization: Bearer %s" -H "Content-Type: application/json" -H "Prefer: return=minimal" --data-binary "@%s" 2>&1',
      config$url, table_name, config$api_key, config$api_key, tmp_test
    )
    test_result <- system(test_cmd, intern = TRUE)
    message("  Test result:")
    cat(paste(test_result, collapse = "\n"), "\n")
    unlink(tmp_test)
  }

  # Pre-push validation: check for problematic values
  message("Validating data before push...")

  # Debug: show data info
  message("Data dimensions: ", nrow(data), " rows x ", ncol(data), " cols")
  message("Column names: ", paste(names(data), collapse = ", "))

  # Check for parsing problems if vroom/readr was used
  probs <- attr(data, "problems")
  if (!is.null(probs) && is.data.frame(probs) && nrow(probs) > 0) {
    message("Warning: ", nrow(probs), " parsing problems detected")
    print(head(probs, 10))
  }

  # Check for unexpected column types that could cause JSON issues
  for (col in names(data)) {
    vals <- data[[col]]
    if (is.list(vals)) {
      rlang::abort(
        c(
          paste0("Column '", col, "' is a list - cannot serialize to JSON"),
          i = "Flatten or convert to atomic vector before push"
        ),
        class = "supabase_validation_error"
      )
    }
  }

  total_rows <- nrow(data)
  batches <- ceiling(total_rows / batch_size)

  message(
    "Pushing ", total_rows, " rows to '", table_name,
    "' in ", batches, " batches..."
  )

  for (i in seq_len(batches)) {
    start_idx <- (i - 1L) * batch_size + 1L
    end_idx <- min(i * batch_size, total_rows)
    batch_data <- data[start_idx:end_idx, ]

    # Convert tibble to list of lists (required format for PostgREST)
    # Replace NA values with NULL for proper JSON serialization
    batch_list <- lapply(seq_len(nrow(batch_data)), function(j) {
      row <- as.list(batch_data[j, ])
      lapply(row, function(x) if (length(x) == 0 || is.na(x)) NULL else x)
    })

    # Validate batch is not empty
    if (length(batch_list) == 0) {
      rlang::abort(
        c(
          paste0("Empty batch ", i),
          i = paste("Batch rows:", start_idx, "-", end_idx)
        ),
        class = "supabase_validation_error"
      )
    }

    # Serialize JSON explicitly - we control exactly what's sent
    # Using null = "null" ensures NULL values serialize properly
    json_body <- jsonlite::toJSON(batch_list, auto_unbox = TRUE, null = "null")

    # Validate serialization succeeded
    if (nchar(json_body) < 3) {
      rlang::abort(
        c(
          paste0("JSON serialization produced invalid output for batch ", i),
          x = paste0("JSON length: ", nchar(json_body)),
          i = paste("Batch rows:", start_idx, "-", end_idx)
        ),
        class = "supabase_validation_error"
      )
    }

    # Convert to plain character and ensure UTF-8 encoding
    json_str <- as.character(json_body)
    Encoding(json_str) <- "UTF-8"

    # Convert to raw bytes for transmission
    json_bytes <- charToRaw(json_str)

    message(
      "  Sending batch ", i, " (", length(batch_list), " rows, ",
      length(json_bytes), " bytes)..."
    )

    # Try using system curl to bypass httr2 entirely
    # Write JSON to temp file and use curl --data-binary
    tmp_file <- tempfile(fileext = ".json")
    on.exit(unlink(tmp_file), add = TRUE)
    writeBin(json_bytes, tmp_file)

    verbose_mode <- Sys.getenv("CI") != ""

    if (verbose_mode && i == 1) {
      message("  DEBUG: Using system curl")
      message("  DEBUG: Temp file size: ", file.info(tmp_file)$size, " bytes")
      message("  DEBUG: First 100 bytes: ", rawToChar(json_bytes[1:100]))
    }

    curl_cmd <- sprintf(
      'curl -s -w "\\n%%{http_code}" -X POST "%s/rest/v1/%s" -H "apikey: %s" -H "Authorization: Bearer %s" -H "Content-Type: application/json" -H "Prefer: return=minimal" --data-binary "@%s"',
      config$url, table_name, config$api_key, config$api_key, tmp_file
    )

    curl_result <- system(curl_cmd, intern = TRUE)

    # Last line is the HTTP status code
    status <- as.integer(tail(curl_result, 1))
    body <- paste(head(curl_result, -1), collapse = "\n")

    if (status >= 400L) {

      # Debug: show sample of problematic batch
      message("Debug: First row of failed batch:")
      first_row_json <- jsonlite::toJSON(
        batch_list[[1]], auto_unbox = TRUE, null = "null"
      )
      message(first_row_json)

      rlang::abort(
        c(
          paste0("Failed to push batch ", i, " to '", table_name, "'"),
          x = paste0("HTTP ", status),
          i = paste("Supabase response:", body),
          i = paste("Batch rows:", start_idx, "-", end_idx)
        ),
        class = "supabase_push_error"
      )
    }

    message(
      "  Batch ", i, "/", batches,
      " complete (rows ", start_idx, "-", end_idx, ")"
    )
  }

  message("Successfully pushed ", total_rows, " rows to '", table_name, "'")
  invisible(TRUE)
}

#' Delete all rows from a Supabase table
#'
#' @param table_name Character string: name of the Supabase table
#'
#' @return Invisibly returns TRUE on success, FALSE if table didn't exist
#' @export
clear_table <- function(table_name) {
  # --- Input validation ---
  checkmate::assert_string(table_name, min.chars = 1)

  # Get configuration
  config <- get_supabase_config()

  message("Clearing existing data from '", table_name, "'...")

  resp <- request(paste0(config$url, "/rest/v1/", table_name)) |>
    req_headers(
      "apikey" = config$api_key,
      "Authorization" = paste("Bearer", config$api_key),
      "Prefer" = "return=minimal"
    ) |>
    req_url_query(`id` = "gte.0") |>
    req_method("DELETE") |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  status <- resp_status(resp)
  if (status >= 400L && status != 404L) {
    body <- resp_body_string(resp)
    rlang::warn(
      c(
        paste0("Could not clear table '", table_name, "' (may not exist yet)"),
        x = paste0("HTTP ", status),
        i = body
      ),
      class = "supabase_clear_warning"
    )
    return(invisible(FALSE))
  }

  invisible(TRUE)
}

#' Convert NA values to NULL in a data frame row
#'
#' @param row A list representing a single row
#' @return List with NA values replaced by NULL
#' @keywords internal
convert_na_to_null <- function(row) {
  lapply(row, function(x) if (length(x) == 0 || is.na(x)) NULL else x)
}

# --- Main execution (only when run directly) ---
if (sys.nframe() == 0L) {
  # Script is being run directly (not sourced)
  message("Running push_to_supabase.R as main script...")

  # Read clean data; convert column names to lowercase for Postgres
  message("Reading airports data...")
  airports <- read_csv("data/clean/airports.csv", show_col_types = FALSE) |>
    rename_with(tolower)

  message("Reading navaids data...")
  navaids <- read_csv("data/clean/navaids.csv", show_col_types = FALSE) |>
    rename_with(tolower)

  # Clear and push airports
  clear_table("airports")
  push_to_supabase("airports", airports)

  # Clear and push navaids
  clear_table("navaids")
  push_to_supabase("navaids", navaids)

  message("Done! Both tables pushed to Supabase.")
}
