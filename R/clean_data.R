library(tidyverse)

# Dynamically find the APT and NAV directories
raw_dirs <- list.dirs("data/raw", full.names = TRUE, recursive = FALSE)
apt_dir <- raw_dirs[grepl("_APT_CSV$", raw_dirs)]
nav_dir <- raw_dirs[grepl("_NAV_CSV$", raw_dirs)]

if (length(apt_dir) == 0 || length(nav_dir) == 0) {
  stop("Could not find APT or NAV directories in data/raw/")
}

# Use the first match if multiple exist
apt_dir <- apt_dir[1]
nav_dir <- nav_dir[1]

message("Processing APT data from: ", apt_dir)
message("Processing NAV data from: ", nav_dir)

# Clean airport data
airports_raw <- read_csv(file.path(apt_dir, "APT_BASE.csv"), show_col_types = FALSE)

airports <- airports_raw |>
  filter(nchar(ARPT_ID) != 4) |>
  select(
    EFF_DATE,
    SITE_NO,
    SITE_TYPE_CODE,
    STATE_CODE,
    ARPT_ID,
    CITY,
    COUNTRY_CODE,
    STATE_NAME,
    COUNTY_NAME,
    ARPT_NAME,
    LAT_DEG,
    LAT_MIN,
    LAT_SEC,
    LAT_HEMIS,
    LAT_DECIMAL,
    LONG_DEG,
    LONG_MIN,
    LONG_SEC,
    LONG_HEMIS,
    LONG_DECIMAL,
    ELEV,
    ELEV_METHOD_CODE,
    MAG_VARN,
    MAG_HEMIS,
    MAG_VARN_YEAR,
    ICAO_ID
  )

# Ensure clean directory exists
if (!dir.exists("data/clean")) {
  dir.create("data/clean", recursive = TRUE)
}

write_csv(airports, "data/clean/airports.csv")
message("Wrote ", nrow(airports), " airports to data/clean/airports.csv")

# Clean NAVAID data
navaids_raw <- read_csv(file.path(nav_dir, "NAV_BASE.csv"), show_col_types = FALSE)

navaids <- navaids_raw |>
  select(
    EFF_DATE,
    NAV_ID,
    NAV_TYPE,
    STATE_CODE,
    CITY,
    COUNTRY_CODE,
    NAME,
    STATE_NAME,
    REGION_CODE,
    LAT_HEMIS,
    LAT_DEG,
    LAT_MIN,
    LAT_SEC,
    LAT_DECIMAL,
    LONG_HEMIS,
    LONG_DEG,
    LONG_MIN,
    LONG_SEC,
    LONG_DECIMAL,
    ELEV,
    MAG_VARN,
    MAG_VARN_HEMIS,
    MAG_VARN_YEAR,
    ALT_CODE
  )

write_csv(navaids, "data/clean/navaids.csv")
message("Wrote ", nrow(navaids), " navaids to data/clean/navaids.csv")

rm(airports_raw, navaids_raw)

# Remove extra CSV files from APT directory
apt_extra_files <- c("APT_ATT.csv", "APT_ARS.csv", "APT_CON.csv",
                     "APT_RMK.csv", "APT_RWY_END.csv", "APT_RWY.csv")
apt_files_to_remove <- file.path(apt_dir, apt_extra_files)
apt_files_to_remove <- apt_files_to_remove[file.exists(apt_files_to_remove)]

# Remove extra CSV files from NAV directory
nav_extra_files <- c("NAV_CKPT.csv", "NAV_RMK.csv")
nav_files_to_remove <- file.path(nav_dir, nav_extra_files)
nav_files_to_remove <- nav_files_to_remove[file.exists(nav_files_to_remove)]

# Remove all extra files
files_to_remove <- c(apt_files_to_remove, nav_files_to_remove)
if (length(files_to_remove) > 0) {
  invisible(file.remove(files_to_remove))
  message("Removed ", length(files_to_remove), " extra CSV files")
}

message("Data cleaning complete!")