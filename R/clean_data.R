# clean_data.R
# Functions for cleaning FAA airport and navaid data
#
# Usage:
#   source("R/clean_data.R")
#   airports <- clean_airports("data/raw/19_DEC_2024_APT_CSV/APT_BASE.csv")
#   navaids <- clean_navaids("data/raw/19_DEC_2024_NAV_CSV/NAV_BASE.csv")
#
# Or run directly to clean current raw data:
#   Rscript R/clean_data.R

library(dplyr)
library(readr)
library(checkmate)
library(rlang)

# --- Column definitions ---
# Expected columns for airports table (per docs/data_validation.md)
airports_columns <- c(
  "EFF_DATE", "SITE_NO", "SITE_TYPE_CODE", "STATE_CODE", "ARPT_ID",
  "CITY", "COUNTRY_CODE", "STATE_NAME", "COUNTY_NAME", "ARPT_NAME",
  "LAT_DEG", "LAT_MIN", "LAT_SEC", "LAT_HEMIS", "LAT_DECIMAL",
  "LONG_DEG", "LONG_MIN", "LONG_SEC", "LONG_HEMIS", "LONG_DECIMAL",
  "ELEV", "ELEV_METHOD_CODE", "MAG_VARN", "MAG_HEMIS", "MAG_VARN_YEAR",
  "ICAO_ID"
)

# Expected columns for navaids table (per docs/data_validation.md)
navaids_columns <- c(
  "EFF_DATE", "NAV_ID", "NAV_TYPE", "STATE_CODE", "CITY",
  "COUNTRY_CODE", "NAME", "STATE_NAME", "REGION_CODE",
  "LAT_HEMIS", "LAT_DEG", "LAT_MIN", "LAT_SEC", "LAT_DECIMAL",
  "LONG_HEMIS", "LONG_DEG", "LONG_MIN", "LONG_SEC", "LONG_DECIMAL",
  "ELEV", "MAG_VARN", "MAG_VARN_HEMIS", "MAG_VARN_YEAR", "ALT_CODE"
)

#' Clean airport data from FAA APT_BASE.csv
#'
#' Reads APT_BASE.csv, filters out 4-character airport IDs (military),
#' and selects relevant columns.
#'
#' @param apt_base_path Path to APT_BASE.csv file
#' @return Tibble with cleaned airport data
#' @export
clean_airports <- function(apt_base_path) {
  # --- Input validation ---
  checkmate::assert_file_exists(apt_base_path, extension = "csv")

  message("Reading airports from: ", apt_base_path)

  # FAA files use ISO-8859-1 (Latin-1) encoding for special characters
  airports_raw <- readr::read_csv(
    apt_base_path,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "ISO-8859-1")
  )

  # Verify required columns exist
  missing_cols <- setdiff(airports_columns, names(airports_raw))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        "APT_BASE.csv missing required columns",
        x = paste("Missing:", paste(missing_cols, collapse = ", "))
      ),
      class = "clean_data_schema_error"
    )
  }

  airports <- airports_raw |>
    dplyr::filter(nchar(ARPT_ID) != 4) |>
    dplyr::select(dplyr::all_of(airports_columns))

  # Validate result is not empty
  if (nrow(airports) == 0) {
    rlang::abort(
      c(
        "No airports remaining after filtering",
        i = "All records had 4-character ARPT_ID (military airports)"
      ),
      class = "clean_data_empty_result"
    )
  }

  message("Cleaned ", nrow(airports), " airports")
  airports
}

#' Clean navaid data from FAA NAV_BASE.csv
#'
#' Reads NAV_BASE.csv and selects relevant columns.
#'
#' @param nav_base_path Path to NAV_BASE.csv file
#' @return Tibble with cleaned navaid data
#' @export
clean_navaids <- function(nav_base_path) {
  # --- Input validation ---
  checkmate::assert_file_exists(nav_base_path, extension = "csv")

  message("Reading navaids from: ", nav_base_path)

  # FAA files use ISO-8859-1 (Latin-1) encoding for special characters
  navaids_raw <- readr::read_csv(
    nav_base_path,
    show_col_types = FALSE,
    locale = readr::locale(encoding = "ISO-8859-1")
  )

  # Verify required columns exist
  missing_cols <- setdiff(navaids_columns, names(navaids_raw))
  if (length(missing_cols) > 0) {
    rlang::abort(
      c(
        "NAV_BASE.csv missing required columns",
        x = paste("Missing:", paste(missing_cols, collapse = ", "))
      ),
      class = "clean_data_schema_error"
    )
  }

  navaids <- navaids_raw |>
    dplyr::select(dplyr::all_of(navaids_columns))

  # Validate result is not empty
  if (nrow(navaids) == 0) {
    rlang::abort(
      "No navaids found in NAV_BASE.csv",
      class = "clean_data_empty_result"
    )
  }

  message("Cleaned ", nrow(navaids), " navaids")
  navaids
}

#' Validate cleaned data against schema rules
#'
#' Performs data quality checks on cleaned airports or navaids data.
#'
#' @param data Tibble of cleaned data
#' @param schema_type One of "airports" or "navaids"
#' @return The input data (invisibly) if valid, otherwise aborts
#' @export
validate_cleaned_data <- function(data,
                                  schema_type = c("airports", "navaids")) {
  checkmate::assert_data_frame(data, min.rows = 1)
  schema_type <- match.arg(schema_type)

  message("Validating ", schema_type, " data...")

  if (schema_type == "airports") {
    # Check expected row count (warning only)
    if (nrow(data) < 5000) {
      rlang::warn(
        paste("Expected at least 5000 airports, got", nrow(data)),
        class = "validation_warning"
      )
    }

    # Check ARPT_ID length (error - critical)
    if (any(nchar(data$ARPT_ID) > 3)) {
      rlang::abort(
        "Found airports with ARPT_ID > 3 characters after filtering",
        class = "validation_error"
      )
    }

    # Check latitude range (warning only)
    lat_valid <- data$LAT_DECIMAL >= 18 & data$LAT_DECIMAL <= 72
    if (!all(lat_valid, na.rm = TRUE)) {
      rlang::warn(
        "Some latitude values outside expected US range (18-72)",
        class = "validation_warning"
      )
    }

    # Check longitude range (warning only)
    long_valid <- data$LONG_DECIMAL >= -180 & data$LONG_DECIMAL <= -64
    if (!all(long_valid, na.rm = TRUE)) {
      rlang::warn(
        "Some longitude values outside expected US range (-180 to -64)",
        class = "validation_warning"
      )
    }
  } else {
    # Navaid-specific validation
    # Check expected row count (warning only)
    if (nrow(data) < 1000) {
      rlang::warn(
        paste("Expected at least 1000 navaids, got", nrow(data)),
        class = "validation_warning"
      )
    }

    # Check NAV_TYPE values (warning only)
    valid_types <- c(
      "VOR", "VORTAC", "NDB", "NDB/DME", "TACAN",
      "VOR/DME", "FAN MARKER", "VOT", "DME", "MARINE NDB"
    )
    if (!all(data$NAV_TYPE %in% valid_types)) {
      rlang::warn(
        "Some NAV_TYPE values are unexpected",
        class = "validation_warning"
      )
    }
  }

  message("Validation complete for ", schema_type)
  invisible(data)
}

#' Find raw data directories
#'
#' Searches for APT and NAV directories in the raw data folder.
#'
#' @param raw_dir Path to raw data directory (default: "data/raw")
#' @return Named list with apt_dir and nav_dir paths
#' @keywords internal
find_raw_data_dirs <- function(raw_dir = "data/raw") {
  checkmate::assert_directory_exists(raw_dir)

  raw_dirs <- list.dirs(raw_dir, full.names = TRUE, recursive = FALSE)
  apt_dir <- raw_dirs[grepl("_APT_CSV$", raw_dirs)]
  nav_dir <- raw_dirs[grepl("_NAV_CSV$", raw_dirs)]

  if (length(apt_dir) == 0 || length(nav_dir) == 0) {
    rlang::abort(
      c(
        "Could not find APT or NAV directories",
        i = paste("Searched in:", raw_dir)
      ),
      class = "clean_data_missing_dirs"
    )
  }

  # Use first match if multiple exist
  list(
    apt_dir = apt_dir[1],
    nav_dir = nav_dir[1]
  )
}

#' Remove extra CSV files from raw data directories
#'
#' @param apt_dir Path to APT directory
#' @param nav_dir Path to NAV directory
#' @return Number of files removed
#' @keywords internal
remove_extra_files <- function(apt_dir, nav_dir) {
  apt_extra <- c(
    "APT_ATT.csv", "APT_ARS.csv", "APT_CON.csv",
    "APT_RMK.csv", "APT_RWY_END.csv", "APT_RWY.csv"
  )
  nav_extra <- c("NAV_CKPT.csv", "NAV_RMK.csv")

  apt_files <- file.path(apt_dir, apt_extra)
  nav_files <- file.path(nav_dir, nav_extra)
  all_files <- c(apt_files, nav_files)
  files_to_remove <- all_files[file.exists(all_files)]

  if (length(files_to_remove) > 0) {
    file.remove(files_to_remove)
    message("Removed ", length(files_to_remove), " extra CSV files")
  }

  length(files_to_remove)
}

# --- Main execution (only when run directly) ---
if (sys.nframe() == 0L) {
  message("Running clean_data.R as main script...")

  # Find raw data directories
  dirs <- find_raw_data_dirs("data/raw")
  message("Processing APT data from: ", dirs$apt_dir)
  message("Processing NAV data from: ", dirs$nav_dir)

  # Clean airports
  apt_path <- file.path(dirs$apt_dir, "APT_BASE.csv")
  airports <- clean_airports(apt_path)
  validate_cleaned_data(airports, "airports")

  # Ensure clean directory exists
  if (!dir.exists("data/clean")) {
    dir.create("data/clean", recursive = TRUE)
  }

  readr::write_csv(airports, "data/clean/airports.csv")
  message("Wrote ", nrow(airports), " airports to data/clean/airports.csv")

  # Clean navaids
  nav_path <- file.path(dirs$nav_dir, "NAV_BASE.csv")
  navaids <- clean_navaids(nav_path)
  validate_cleaned_data(navaids, "navaids")

  readr::write_csv(navaids, "data/clean/navaids.csv")
  message("Wrote ", nrow(navaids), " navaids to data/clean/navaids.csv")

  # Remove extra files
  remove_extra_files(dirs$apt_dir, dirs$nav_dir)

  message("Data cleaning complete!")
}
