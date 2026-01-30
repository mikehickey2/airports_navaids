# helper-setup.R
# Shared fixtures and utilities for testthat tests
# This file is automatically sourced before tests run

library(testthat)
library(tibble)
library(dplyr)
library(withr)

# --- Source R files from project ---
# Determine project root from test directory
project_root <- function() {
 # When running from tests/testthat, go up two levels
 # When running from project root, stay there
 candidates <- c(
   ".",
   "..",
   "../..",
   file.path(getwd(), "../.."),
   Sys.getenv("AIRPORTS_NAVAIDS_ROOT", unset = NA)
 )

 for (path in candidates) {
   if (!is.na(path) && file.exists(file.path(path, "R", "push_to_supabase.R")))
     return(normalizePath(path))
 }

 rlang::abort("Could not find project root. Set AIRPORTS_NAVAIDS_ROOT env var.")
}

source_project_file <- function(filename) {
 source(file.path(project_root(), "R", filename), local = FALSE)
}

# --- Mock Supabase credentials ---
# Use fake credentials for testing (never real keys)
MOCK_SUPABASE_API_KEY <- "test_key_not_real_do_not_use"
MOCK_SUPABASE_URL <- "https://test-project.supabase.co"

# Set mock env vars for testing (will be restored after each test via withr)
setup_mock_credentials <- function() {
 withr::local_envvar(
   SUPABASE_API_KEY = MOCK_SUPABASE_API_KEY,
   SUPABASE_URL = MOCK_SUPABASE_URL
 )
}

# --- Sample data fixtures ---
# Minimal valid airports tibble matching expected schema
sample_airports <- function(n = 3) {
 tibble::tibble(
   eff_date = as.Date("2024-12-19"),
   site_no = sprintf("TEST%04d", seq_len(n)),
   site_type_code = "A",
   state_code = rep(c("CA", "TX", "NY"), length.out = n),
   arpt_id = sprintf("TS%d", seq_len(n)),
   city = sprintf("Test City %d", seq_len(n)),
   country_code = "US",
   state_name = rep(c("California", "Texas", "New York"), length.out = n),
   county_name = sprintf("Test County %d", seq_len(n)),
   arpt_name = sprintf("Test Airport %d", seq_len(n)),
   lat_deg = rep(34L, n),
   lat_min = rep(3L, n),
   lat_sec = sprintf("%.4f", runif(n, 0, 60)),
   lat_hemis = "N",
   lat_decimal = runif(n, 25, 49),
   long_deg = rep(118L, n),
   long_min = rep(14L, n),
   long_sec = sprintf("%.4f", runif(n, 0, 60)),
   long_hemis = "W",
   long_decimal = runif(n, -125, -70),
   elev = runif(n, 0, 10000),
   elev_method_code = "S",
   mag_varn = runif(n, -20, 20),
   mag_hemis = rep(c("E", "W"), length.out = n),
   mag_varn_year = 2020L,
   icao_id = sprintf("KTS%d", seq_len(n))
 )
}

# Minimal valid navaids tibble matching expected schema
sample_navaids <- function(n = 3) {
 tibble::tibble(
   eff_date = as.Date("2024-12-19"),
   nav_id = sprintf("TST%d", seq_len(n)),
   nav_type = rep(c("VOR", "NDB", "VORTAC"), length.out = n),
   state_code = rep(c("CA", "TX", "NY"), length.out = n),
   city = sprintf("Test City %d", seq_len(n)),
   country_code = "US",
   name = sprintf("Test Navaid %d", seq_len(n)),
   state_name = rep(c("California", "Texas", "New York"), length.out = n),
   region_code = "AWP",
   lat_hemis = "N",
   lat_deg = rep(34L, n),
   lat_min = rep(3L, n),
   lat_sec = sprintf("%.4f", runif(n, 0, 60)),
   lat_decimal = runif(n, 25, 49),
   long_hemis = "W",
   long_deg = rep(118L, n),
   long_min = rep(14L, n),
   long_sec = sprintf("%.4f", runif(n, 0, 60)),
   long_decimal = runif(n, -125, -70),
   elev = runif(n, 0, 10000),
   mag_varn = runif(n, -20, 20),
   mag_varn_hemis = rep(c("E", "W"), length.out = n),
   mag_varn_year = 2020L,
   alt_code = "LOW"
 )
}

# --- HTTP mock utilities ---
# Create a mock HTTP response for testing push_to_supabase
mock_http_response <- function(status_code = 200, body = "[]") {
 structure(
   list(
     status_code = status_code,
     headers = list(`content-type` = "application/json"),
     body = charToRaw(body)
   ),
   class = "httr2_response"
 )
}

# --- Temp directory utilities ---
# Create a temp directory with sample CSV files for testing clean_data.R
create_test_raw_data_dir <- function() {
 temp_dir <- withr::local_tempdir(pattern = "faa_test_")

 # Create APT and NAV subdirectories
 apt_dir <- file.path(temp_dir, "19_DEC_2024_APT_CSV")
 nav_dir <- file.path(temp_dir, "19_DEC_2024_NAV_CSV")
 dir.create(apt_dir)
 dir.create(nav_dir)

 # Write sample APT_BASE.csv
 apt_data <- tibble::tibble(
   EFF_DATE = "12/19/2024",
   SITE_NO = c("00001", "00002", "00003", "00004"),
   SITE_TYPE_CODE = "A",
   STATE_CODE = c("CA", "TX", "NY", "FL"),
   ARPT_ID = c("LAX", "DFW", "JFK", "KMIA"),
   CITY = c("Los Angeles", "Dallas", "New York", "Miami"),
   COUNTRY_CODE = "US",
   STATE_NAME = c("California", "Texas", "New York", "Florida"),
   COUNTY_NAME = c("Los Angeles", "Tarrant", "Queens", "Miami-Dade"),
   ARPT_NAME = c("Los Angeles Intl", "Dallas Fort Worth", "JFK Intl", "Miami Intl"),
   LAT_DEG = c(33L, 32L, 40L, 25L),
   LAT_MIN = c(56L, 53L, 38L, 47L),
   LAT_SEC = c("33.0000", "35.0000", "23.0000", "36.0000"),
   LAT_HEMIS = "N",
   LAT_DECIMAL = c(33.9425, 32.8931, 40.6398, 25.7933),
   LONG_DEG = c(118L, 97L, 73L, 80L),
   LONG_MIN = c(24L, 2L, 46L, 17L),
   LONG_SEC = c("29.0000", "44.0000", "44.0000", "25.0000"),
   LONG_HEMIS = "W",
   LONG_DECIMAL = c(-118.4081, -97.0456, -73.7789, -80.2903),
   ELEV = c(128, 607, 13, 9),
   ELEV_METHOD_CODE = "S",
   MAG_VARN = c(13.0, 4.0, -13.0, -5.0),
   MAG_HEMIS = c("E", "E", "W", "W"),
   MAG_VARN_YEAR = 2020L,
   ICAO_ID = c("KLAX", "KDFW", "KJFK", "KMIA")
 )
 readr::write_csv(apt_data, file.path(apt_dir, "APT_BASE.csv"))

 # Write sample NAV_BASE.csv
 nav_data <- tibble::tibble(
   EFF_DATE = "12/19/2024",
   NAV_ID = c("LAX", "DFW", "JFK"),
   NAV_TYPE = c("VOR", "VORTAC", "VOR"),
   STATE_CODE = c("CA", "TX", "NY"),
   CITY = c("Los Angeles", "Dallas", "New York"),
   COUNTRY_CODE = "US",
   NAME = c("Los Angeles VOR", "Cowboy VORTAC", "Kennedy VOR"),
   STATE_NAME = c("California", "Texas", "New York"),
   REGION_CODE = c("AWP", "ASW", "AEA"),
   LAT_HEMIS = "N",
   LAT_DEG = c(33L, 32L, 40L),
   LAT_MIN = c(56L, 53L, 38L),
   LAT_SEC = c("33.0000", "35.0000", "23.0000"),
   LAT_DECIMAL = c(33.9425, 32.8931, 40.6398),
   LONG_HEMIS = "W",
   LONG_DEG = c(118L, 97L, 73L),
   LONG_MIN = c(24L, 2L, 46L),
   LONG_SEC = c("29.0000", "44.0000", "44.0000"),
   LONG_DECIMAL = c(-118.4081, -97.0456, -73.7789),
   ELEV = c(128, 607, 13),
   MAG_VARN = c(13.0, 4.0, -13.0),
   MAG_VARN_HEMIS = c("E", "E", "W"),
   MAG_VARN_YEAR = 2020L,
   ALT_CODE = "LOW"
 )
 readr::write_csv(nav_data, file.path(nav_dir, "NAV_BASE.csv"))

 temp_dir
}
