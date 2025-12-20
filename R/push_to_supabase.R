library(httr2)
library(tidyverse)

# Load environment variables from project .Renviron if not already loaded
if (Sys.getenv("SUPABASE_API_KEY") == "") {
  readRenviron(".Renviron")
}

# Check that credentials are available
if (Sys.getenv("SUPABASE_API_KEY") == "") {
  stop("Missing Supabase credentials. Please set SUPABASE_API_KEY in .Renviron")
}

# Supabase project configuration
supabase_url <- "https://bjmjxipflycjnrwdujxp.supabase.co"
api_key <- Sys.getenv("SUPABASE_API_KEY")

# Function to upsert data to Supabase table
push_to_supabase <- function(table_name, data, batch_size = 500) {
  total_rows <- nrow(data)
  batches <- ceiling(total_rows / batch_size)

  message("Pushing ", total_rows, " rows to '", table_name, "' in ", batches, " batches...")

  for (i in seq_len(batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, total_rows)
    batch_data <- data[start_idx:end_idx, ]

    # Convert tibble to list of lists (required format for PostgREST)
    # Replace NA values with NULL for proper JSON serialization
    batch_list <- lapply(seq_len(nrow(batch_data)), function(j) {
      row <- as.list(batch_data[j, ])
      lapply(row, function(x) if (length(x) == 0 || is.na(x)) NULL else x)
    })

    resp <- request(paste0(supabase_url, "/rest/v1/", table_name)) |>
      req_headers(
        "apikey" = api_key,
        "Authorization" = paste("Bearer", api_key),
        "Content-Type" = "application/json",
        "Prefer" = "return=minimal"
      ) |>
      req_body_json(batch_list, auto_unbox = TRUE) |>
      req_method("POST") |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()

    status <- resp_status(resp)
    if (status >= 400) {
      body <- resp_body_string(resp)
      stop("Error pushing batch ", i, ": ", status, " - ", body)
    }

    message("  Batch ", i, "/", batches, " complete (rows ", start_idx, "-", end_idx, ")")
  }

  message("Successfully pushed ", total_rows, " rows to '", table_name, "'")
}

# Function to delete all rows from a table
clear_table <- function(table_name) {
  message("Clearing existing data from '", table_name, "'...")

  resp <- request(paste0(supabase_url, "/rest/v1/", table_name)) |>
    req_headers(
      "apikey" = api_key,
      "Authorization" = paste("Bearer", api_key),
      "Prefer" = "return=minimal"
    ) |>
    req_url_query(`id` = "gte.0") |>  # Match all rows
    req_method("DELETE") |>
    req_error(is_error = function(resp) FALSE) |>
    req_perform()

  status <- resp_status(resp)
  if (status >= 400 && status != 404) {
    body <- resp_body_string(resp)
    warning("Could not clear table (may not exist yet): ", status, " - ", body)
  }
}

# Read the clean data and convert column names to lowercase (to match Postgres)
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