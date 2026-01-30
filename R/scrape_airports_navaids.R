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
library(purrr)
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
    triggered_by = if (Sys.getenv("GITHUB_ACTIONS") == "true") {
      "scheduled"
    } else {
      "local"
    }
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
    result$duration_seconds <- as.integer(
      difftime(Sys.time(), start_time, units = "secs")
    )
    return(result)
  }

  if (dry_run) {
    message("DRY RUN - Would download and process data")
    result$status <- "dry_run"
    result$faa_eff_date <- current_date
    result$duration_seconds <- as.integer(
      difftime(Sys.time(), start_time, units = "secs")
    )
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
  result$duration_seconds <- as.integer(
    difftime(Sys.time(), start_time, units = "secs")
  )

  message("Done! Data updated to ", format(current_date, "%d %b %Y"))
  result
}

# --- Main execution (only when run directly) ---
if (sys.nframe() == 0L) {
  message("Running scrape_airports_navaids.R as main script...")
  run_pipeline(force = FALSE)
}
