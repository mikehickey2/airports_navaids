# scrape_airports_navaids.R
# Orchestrates FAA data pipeline: check for updates, download, clean, push
#
# Usage:
#   source("R/scrape_airports_navaids.R")
#   run_pipeline()           # Check for updates and run if newer
#   run_pipeline(force = TRUE)  # Force download even if data is current
#
# Or run directly:
#   Rscript R/scrape_airports_navaids.R

library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(rvest)
library(checkmate)
library(rlang)

# --- Configuration ---
# nolint start: line_length_linter
faa_nasr_url <- "https://www.faa.gov/air_traffic/flight_info/aeronav/aero_data/NASR_Subscription/"
# nolint end
faa_download_base <- "https://nfdc.faa.gov/webContent/28DaySub/extra/"
pipeline_history_file <- "data/pipeline_history.csv"

#' Log pipeline run to history file
#'
#' Appends a row to the pipeline history CSV tracking counts over time.
#'
#' @param faa_date Date of the FAA subscription
#' @param airports_count Number of airports
#' @param navaids_count Number of navaids
#' @keywords internal
log_pipeline_history <- function(faa_date, airports_count, navaids_count) {
  history_row <- data.frame(
    faa_date = as.character(faa_date),
    airports = airports_count,
    navaids = navaids_count,
    run_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  if (file.exists(pipeline_history_file)) {
    readr::write_csv(history_row, pipeline_history_file, append = TRUE)
  } else {
    readr::write_csv(history_row, pipeline_history_file)
  }

  message("Logged to ", pipeline_history_file)
}

#' Get the date of local raw data
#'
#' Examines directory names in raw_dir to extract the most recent data date.
#'
#' @param raw_dir Path to raw data directory (default: "data/raw")
#' @return Date object or NA if no data exists
#' @export
get_local_data_date <- function(raw_dir = "data/raw") {
  checkmate::assert_directory_exists(raw_dir)

  raw_dirs <- list.dirs(raw_dir, full.names = FALSE, recursive = FALSE)

  # Extract dates from subdirectory names (e.g., "30_Oct_2025_APT_CSV")
  raw_dates <- raw_dirs |>
    stringr::str_extract("^\\d{2}_[A-Za-z]{3}_\\d{4}") |>
    purrr::discard(is.na) |>
    unique() |>
    stringr::str_trim()

  if (length(raw_dates) == 0) {
    message("No existing data found in '", raw_dir, "'")
    return(NA_Date_)
  }

  # Convert to Date format
  parsed_dates <- lubridate::dmy(raw_dates)
  parsed_dates <- parsed_dates[!is.na(parsed_dates)]

  if (length(parsed_dates) == 0) {
    message("Could not parse any dates from directory names")
    return(NA_Date_)
  }

  latest <- max(parsed_dates, na.rm = TRUE)
  message("Latest local data date: ", format(latest, "%d %b %Y"))
  latest
}

#' Scrape the current FAA subscription date from their website
#'
#' @param url URL of FAA NASR subscription page
#' @return Date object representing the current subscription date
#' @export
scrape_faa_current_date <- function(url = faa_nasr_url) {
  checkmate::assert_string(url, pattern = "^https?://")

  message("Checking FAA website for current subscription date...")

  page <- rvest::read_html(url)
  page_text <- page |> rvest::html_text()

  # Extract date from "Current ... Subscription effective Month DD, YYYY"
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

  current_date_text <- current_match[1, 2]
  current_date <- lubridate::mdy(current_date_text)

  if (is.na(current_date)) {
    rlang::abort(
      c(
        "Failed to parse date string from FAA website",
        x = paste("Date text:", current_date_text)
      ),
      class = "scrape_parse_error"
    )
  }

  message("Current FAA subscription date: ", format(current_date, "%d %b %Y"))
  current_date
}

#' Download and extract FAA data
#'
#' @param date Date object for the subscription to download
#' @param type Character: "APT" or "NAV"
#' @param dest_dir Destination directory (default: "data/raw")
#' @return Path to extracted directory
#' @export
download_faa_data <- function(date,
                              type = c("APT", "NAV"),
                              dest_dir = "data/raw") {
  checkmate::assert_date(date)
  type <- match.arg(type)
  checkmate::assert_directory_exists(dest_dir)

  # Format date for URL (DD_MMM_YYYY)
  date_str <- format(date, "%d_%b_%Y")
  zip_filename <- paste0(date_str, "_", type, "_CSV.zip")
  zip_url <- paste0(faa_download_base, zip_filename)

  # Target directory
  extract_dir <- file.path(dest_dir, paste0(date_str, "_", type, "_CSV"))
  zip_path <- file.path(dest_dir, zip_filename)

  # Create extraction directory
  if (!dir.exists(extract_dir)) {
    dir.create(extract_dir, recursive = TRUE)
  }

  message("Downloading ", type, " data from: ", zip_url)
  download.file(zip_url, zip_path, mode = "wb", quiet = FALSE)

  message("Extracting ", type, " data...")
  unzip(zip_path, exdir = extract_dir)

  # Clean up zip file
  file.remove(zip_path)

  message("Extracted to: ", extract_dir)
  extract_dir
}

#' Delete old data directories
#'
#' @param raw_dir Path to raw data directory
#' @return Number of items deleted
#' @keywords internal
delete_old_data <- function(raw_dir = "data/raw") {
  old_items <- list.files(raw_dir, full.names = TRUE)
  if (length(old_items) > 0) {
    message("Deleting ", length(old_items), " old data items...")
    unlink(old_items, recursive = TRUE)
  }
  length(old_items)
}

#' Run data cleaning step of pipeline
#'
#' Cleans airports and navaids data from raw directories.
#'
#' @param apt_dir Path to APT directory
#' @param nav_dir Path to NAV directory
#' @return Named list with airports, navaids, airports_count, navaids_count
#' @keywords internal
run_cleaning <- function(apt_dir, nav_dir) {
  checkmate::assert_directory_exists(apt_dir)
  checkmate::assert_directory_exists(nav_dir)

  # Clean airports
  apt_path <- file.path(apt_dir, "APT_BASE.csv")
  airports <- clean_airports(apt_path)
  validate_cleaned_data(airports, "airports")

  # Ensure clean directory exists
  if (!dir.exists("data/clean")) {
    dir.create("data/clean", recursive = TRUE)
  }

  readr::write_csv(airports, "data/clean/airports.csv")
  message("Wrote ", nrow(airports), " airports to data/clean/airports.csv")

  # Clean navaids
  nav_path <- file.path(nav_dir, "NAV_BASE.csv")
  navaids <- clean_navaids(nav_path)
  validate_cleaned_data(navaids, "navaids")

  readr::write_csv(navaids, "data/clean/navaids.csv")
  message("Wrote ", nrow(navaids), " navaids to data/clean/navaids.csv")

  # Remove extra files
  remove_extra_files(apt_dir, nav_dir)

  list(
    airports = airports,
    navaids = navaids,
    airports_count = nrow(airports),
    navaids_count = nrow(navaids)
  )
}

#' Run the full FAA data pipeline
#'
#' Checks for updates, downloads if newer, cleans data, and pushes to Supabase.
#'
#' @param force Logical: if TRUE, download regardless of date comparison
#' @return List with status, airports_count, navaids_count
#' @export
run_pipeline <- function(force = FALSE) {
  checkmate::assert_flag(force)

  result <- list(
    status = "success",
    airports_count = NA_integer_,
    navaids_count = NA_integer_
  )

  # Get local and remote dates
  local_date <- get_local_data_date("data/raw")
  current_date <- scrape_faa_current_date()

  # Decide whether to update
  needs_update <- force || is.na(local_date) || (current_date > local_date)

  if (!needs_update) {
    message("Data is already up to date. No download needed.")
    result$status <- "no_update"
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
  cleaning_result <- run_cleaning(dirs$apt_dir, dirs$nav_dir)

  result$airports_count <- cleaning_result$airports_count
  result$navaids_count <- cleaning_result$navaids_count

  # Push to Supabase
  message("Pushing data to Supabase...")
  airports_lower <- cleaning_result$airports |> dplyr::rename_with(tolower)
  navaids_lower <- cleaning_result$navaids |> dplyr::rename_with(tolower)

  clear_table("airports")
  push_to_supabase("airports", airports_lower)

  clear_table("navaids")
  push_to_supabase("navaids", navaids_lower)

  # Log to history file

  log_pipeline_history(current_date, result$airports_count, result$navaids_count)

  message("Done! Data updated to ", format(current_date, "%d %b %Y"))
  result
}

# --- Main execution (only when run directly) ---
if (sys.nframe() == 0L) {
  message("Running scrape_airports_navaids.R as main script...")
  run_pipeline(force = FALSE)
}
