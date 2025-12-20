# FAA Aeronautical Reference Platform

Airports and Navaids on Supabase with API Access

**Status:** Active
**Version:** 1.0
**Date:** 2025-12-20
**Author:** Mike
**Primary Data Source:** [FAA NASR 28-Day Subscription](https://www.faa.gov/air_traffic/flight_info/aeronav/aero_data/NASR_Subscription/)

---

## Overview

This project provides a reusable backend platform that ingests publicly available FAA aeronautical reference data (airports and navaids), stores it in a Supabase-hosted Postgres database, and exposes it via a REST API. The platform supports multiple downstream projects requiring authoritative lookup of identifiers and coordinates.

**Current Data:**
- 5,310 airports
- 1,657 navaids

---

## Quick Start

### Prerequisites
- R 4.x with packages: `tidyverse`, `lubridate`, `rvest`, `httr2`
- Supabase account with API key

### Setup

1. Clone the repository
2. Copy `.Renviron.example` to `.Renviron` and add your Supabase API key:
   ```
   SUPABASE_API_KEY=your_key_here
   ```
3. Run the SQL in `sql/create_tables.sql` in Supabase SQL Editor (first time only)

### Update Data

```r
source("R/scrape_airports_navaids.R")
```

This will:
- Check FAA website for new NASR subscription data
- Download if newer than local data
- Clean and filter the data
- Push to Supabase

---

## Project Structure

```
airports_navaids/
├── R/
│   ├── scrape_airports_navaids.R  # Main pipeline - orchestrates everything
│   ├── clean_data.R               # Filters and cleans raw FAA CSV data
│   └── push_to_supabase.R         # Pushes to Supabase via REST API
├── sql/
│   └── create_tables.sql          # PostgreSQL schema for Supabase
├── data/
│   ├── raw/                       # Downloaded FAA CSV files
│   └── clean/                     # Cleaned CSV outputs
├── .Renviron                      # API credentials (gitignored)
└── .Renviron.example              # Template for credentials
```

---

## API Access

Query the data via Supabase REST API:

```bash
# Get airports in California
curl "https://bjmjxipflycjnrwdujxp.supabase.co/rest/v1/airports?state_code=eq.CA" \
  -H "apikey: YOUR_ANON_KEY"

# Get VOR navaids
curl "https://bjmjxipflycjnrwdujxp.supabase.co/rest/v1/navaids?nav_type=eq.VOR" \
  -H "apikey: YOUR_ANON_KEY"

# Search airport by identifier
curl "https://bjmjxipflycjnrwdujxp.supabase.co/rest/v1/airports?arpt_id=eq.LAX" \
  -H "apikey: YOUR_ANON_KEY"
```

### From R

```r
library(httr2)

api_key <- Sys.getenv("SUPABASE_API_KEY")

resp <- request("https://bjmjxipflycjnrwdujxp.supabase.co/rest/v1/airports") |>
  req_headers(apikey = api_key) |>
  req_url_query(state_code = "eq.CA") |>
  req_perform() |>
  resp_body_json()
```

---

## Data Schema

### airports
| Column | Type | Description |
|--------|------|-------------|
| arpt_id | TEXT | Airport identifier (e.g., "LAX") |
| arpt_name | TEXT | Airport name |
| city | TEXT | City |
| state_code | TEXT | State code |
| lat_decimal | NUMERIC | Latitude |
| long_decimal | NUMERIC | Longitude |
| elev | NUMERIC | Elevation (feet) |
| icao_id | TEXT | ICAO identifier |

### navaids
| Column | Type | Description |
|--------|------|-------------|
| nav_id | TEXT | Navaid identifier |
| nav_type | TEXT | Type (NDB, VOR, VORTAC, etc.) |
| name | TEXT | Navaid name |
| state_code | TEXT | State code |
| lat_decimal | NUMERIC | Latitude |
| long_decimal | NUMERIC | Longitude |
| elev | NUMERIC | Elevation (feet) |

---

## Goals

- [x] Provide a single authoritative reference database for airports and navaids
- [x] Provide fast read access through a stable API for downstream tools
- [x] Support cycle-based updates when FAA publishes new data (every 28 days)
- [x] Keep write access limited to ingestion automation
- [x] Keep the design portable for future AWS migration

---

## Future Enhancements

- Add database indexes for frequently queried columns
- Create R/Python client packages
- Add additional FAA data types (airways, fixes, procedures)
- Scheduled automation via cron or GitHub Actions