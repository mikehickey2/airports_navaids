library(tidyverse)
library(lubridate)
library(rvest)
library(stringr)

# Step 1: Check the date of the raw files in "data/raw"
raw_dirs <- list.dirs("data/raw", full.names = FALSE, recursive = FALSE)

# Extract dates from subdirectory names (e.g., "30_Oct_2025_APT_CSV" -> "30_Oct_2025")
raw_dates <- raw_dirs |>
  str_extract("^\\d{2}_[A-Za-z]{3}_\\d{4}") |>
  discard(is.na) |>
  unique() |>
  str_trim()

# Convert to Date format
raw_dates <- dmy(raw_dates)

# Get the latest date from the extracted dates (or set to very old date if none exist)
if (length(raw_dates) == 0 || all(is.na(raw_dates))) {
  message("No existing data found in 'data/raw'. Will download fresh data.")
  latest_raw_date <- as.Date("1900-01-01")
} else {
  latest_raw_date <- max(raw_dates, na.rm = TRUE)
  message("Latest local data date: ", format(latest_raw_date, "%d %b %Y"))
}

# Step 2: Scrape the FAA website for the "CURRENT" subscription date
faa_base_url <- "https://www.faa.gov/air_traffic/flight_info/aeronav/aero_data/NASR_Subscription/"
page <- read_html(faa_base_url)

# Find the "Current" section header and get the link that follows it
# The page has Preview, Current, and Archives sections - we need Current specifically
page_text <- page |> html_text()

# Extract the date from the "Current" section text (e.g., "Current ... effective November 27, 2025")
current_match <- str_match(page_text, "Current[^A-Za-z]*Subscription effective ([A-Za-z]+ \\d{1,2}, \\d{4})")
current_date_text <- current_match[1, 2]

# Parse the date (e.g., "November 27, 2025")
current_date <- mdy(current_date_text)

message("Current FAA subscription date: ", format(current_date, "%d %b %Y"))

# Step 3: Compare dates and download if newer
if (current_date > latest_raw_date) {
  message("New data available! Downloading...")

  # Format the date for the download URL (DD_MMM_YYYY)
  download_date_str <- format(current_date, "%d_%b_%Y")

  # Construct download URLs
  download_base <- "https://nfdc.faa.gov/webContent/28DaySub/extra/"
  apt_zip_url <- paste0(download_base, download_date_str, "_APT_CSV.zip")
  nav_zip_url <- paste0(download_base, download_date_str, "_NAV_CSV.zip")

  # Define subdirectory names and zip file paths
  apt_dir <- paste0("data/raw/", download_date_str, "_APT_CSV")
  nav_dir <- paste0("data/raw/", download_date_str, "_NAV_CSV")
  apt_zip_file <- paste0(apt_dir, ".zip")
  nav_zip_file <- paste0(nav_dir, ".zip")

  # Step 4: Delete old directories and files
  old_items <- list.files("data/raw", full.names = TRUE)
  if (length(old_items) > 0) {
    message("Deleting old data...")
    unlink(old_items, recursive = TRUE)
  }

  # Step 5: Create subdirectories for extraction
  dir.create(apt_dir, recursive = TRUE)
  dir.create(nav_dir, recursive = TRUE)

  # Step 6: Download the new zip files
  message("Downloading APT data from: ", apt_zip_url)
  download.file(apt_zip_url, apt_zip_file, mode = "wb")

  message("Downloading NAV data from: ", nav_zip_url)
  download.file(nav_zip_url, nav_zip_file, mode = "wb")

  # Step 7: Unzip the new files into their subdirectories
  message("Extracting APT data...")
  unzip(apt_zip_file, exdir = apt_dir)

  message("Extracting NAV data...")
  unzip(nav_zip_file, exdir = nav_dir)

  # Step 8: Delete the zip files
  message("Cleaning up zip files...")
  file.remove(apt_zip_file, nav_zip_file)

  # Step 8: Source and run the clean_data.R script
  message("Running data cleaning script...")
  source("R/clean_data.R")

  message("Done! Data updated to ", format(current_date, "%d %b %Y"))
} else {
  message("Data is already up to date. No download needed.")
}
