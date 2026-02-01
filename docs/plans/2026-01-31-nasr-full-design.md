# Full NASR Database Design

**Date:** 2026-01-31
**Status:** Draft
**Author:** Mike Hickey + Claude

## Overview

Create a comprehensive FAA NASR (National Airspace System Resources) database containing all available aeronautical reference data. This will be a separate project from `airports_navaids`, which remains focused on quick airport/navaid lookups.

## Goals

1. Ingest 100% of FAA NASR 28-day subscription data
2. Maintain 28-day update cycle aligned with FAA releases
3. Provide REST API access via Supabase
4. Establish relational integrity between data types (fixes referenced by airways, etc.)

## Architecture Decision

**Approach:** Clone + Extend

The `airports_navaids` project remains unchanged. A new `nasr_full` project will:
- Clone the existing pipeline structure
- Extend to support 15+ data types
- Use a separate Supabase project

**Rationale:** The two projects serve different purposes. `airports_navaids` is optimized for sharp, fast queries on a minimal dataset. `nasr_full` is a comprehensive reference database. Keeping them separate avoids scope creep and allows independent evolution.

## Data Inventory

### Current (airports_navaids)

| Table | Rows | Columns | Size |
|-------|------|---------|------|
| airports | 5,308 | 26 | 780 KB |
| navaids | 1,650 | 24 | 218 KB |
| **Total** | | | **~1 MB** (29 MB in Supabase) |

### Target (nasr_full)

| Data Type | FAA Code | Raw Size | Est. Rows | Description |
|-----------|----------|----------|-----------|-------------|
| Airports | APT | 9.5 MB | 12,500 | All airports, all 90 columns |
| Navaids | NAV | 740 KB | 1,700 | All navaids, all 72 columns |
| Fixes/Waypoints | FIX | 17.6 MB | 100,000 | Named reporting points |
| Airways | AWY | 4.2 MB | 15,000 | Victor, Jet, Q routes |
| ILS | ILS | 1.8 MB | 1,500 | Instrument landing systems |
| Holding Patterns | HPF | 3.8 MB | 3,000 | Published holds |
| STARs | STAR | 2.7 MB | 2,500 | Standard terminal arrivals |
| Departure Procedures | DP | 2.5 MB | 2,500 | SIDs |
| Preferred Routes | PFR | 9.9 MB | 20,000 | TEC routes, preferred IFR |
| Military Training Routes | MTR | 3.5 MB | 700 | IR/VR routes |
| ATC Facilities | ATC | 1.9 MB | 600 | Towers, TRACONs |
| ARTCC Boundaries | ARB | 766 KB | 21 | Center boundaries |
| Class Airspace | CLS_ARSP | 494 KB | 750 | B/C/D/E shapes |
| Communications | COM | 697 KB | 2,000 | RCO, FSS frequencies |
| AWOS/ASOS | AWOS | ~500 KB | 2,000 | Weather stations |
| **Total** | | | | **~120-150 MB** |

## Database Schema

### Core Tables (Full Columns)

```sql
-- airports: all 90 columns from APT_BASE.csv
CREATE TABLE airports (
  id SERIAL PRIMARY KEY,
  eff_date DATE NOT NULL,
  site_no TEXT NOT NULL,
  site_type_code TEXT,
  state_code TEXT,
  arpt_id TEXT NOT NULL,
  city TEXT,
  country_code TEXT,
  region_code TEXT,
  ado_code TEXT,
  state_name TEXT,
  county_name TEXT,
  county_assoc_state TEXT,
  arpt_name TEXT,
  ownership_type_code TEXT,
  facility_use_code TEXT,
  lat_deg NUMERIC,
  lat_min NUMERIC,
  lat_sec NUMERIC,
  lat_hemis TEXT,
  lat_decimal NUMERIC,
  long_deg NUMERIC,
  long_min NUMERIC,
  long_sec NUMERIC,
  long_hemis TEXT,
  long_decimal NUMERIC,
  survey_method_code TEXT,
  elev NUMERIC,
  elev_method_code TEXT,
  mag_varn NUMERIC,
  mag_hemis TEXT,
  mag_varn_year INTEGER,
  tpa INTEGER,
  chart_name TEXT,
  dist_city_to_airport INTEGER,
  direction_code TEXT,
  acreage INTEGER,
  resp_artcc_id TEXT,
  computer_id TEXT,
  artcc_name TEXT,
  fss_on_arpt_flag TEXT,
  fss_id TEXT,
  fss_name TEXT,
  phone_no TEXT,
  toll_free_no TEXT,
  alt_fss_id TEXT,
  alt_fss_name TEXT,
  alt_toll_free_no TEXT,
  notam_id TEXT,
  notam_flag TEXT,
  activation_date TEXT,
  arpt_status TEXT,
  far_139_type_code TEXT,
  far_139_carrier_ser_code TEXT,
  arff_cert_type_date TEXT,
  nasp_code TEXT,
  asp_anlys_dtrm_code TEXT,
  cust_flag TEXT,
  lndg_rights_flag TEXT,
  joint_use_flag TEXT,
  mil_lndg_flag TEXT,
  inspect_method_code TEXT,
  inspector_code TEXT,
  last_inspection DATE,
  last_info_response DATE,
  fuel_types TEXT,
  airframe_repair_ser_code TEXT,
  pwr_plant_repair_ser TEXT,
  bottled_oxy_type TEXT,
  bulk_oxy_type TEXT,
  lgt_sked TEXT,
  bcn_lgt_sked TEXT,
  twr_type_code TEXT,
  seg_circle_mkr_flag TEXT,
  bcn_lens_color TEXT,
  lndg_fee_flag TEXT,
  medical_use_flag TEXT,
  arpt_psn_source TEXT,
  position_src_date DATE,
  arpt_elev_source TEXT,
  elevation_src_date DATE,
  contr_fuel_avbl TEXT,
  trns_strg_buoy_flag TEXT,
  trns_strg_hgr_flag TEXT,
  trns_strg_tie_flag TEXT,
  other_services TEXT,
  wind_indcr_flag TEXT,
  icao_id TEXT,
  min_op_network TEXT,
  user_fee_flag TEXT,
  cta TEXT
);

-- fixes: all columns from FIX_BASE.csv
CREATE TABLE fixes (
  id SERIAL PRIMARY KEY,
  eff_date DATE NOT NULL,
  fix_id TEXT NOT NULL,
  fix_type TEXT,
  state_code TEXT,
  icao_region TEXT,
  lat_decimal NUMERIC NOT NULL,
  long_decimal NUMERIC NOT NULL,
  -- additional columns TBD from FIX DATA LAYOUT.pdf
  UNIQUE(fix_id, fix_type)
);

-- airways: segments from AWY_BASE.csv
CREATE TABLE airways (
  id SERIAL PRIMARY KEY,
  eff_date DATE NOT NULL,
  awy_id TEXT NOT NULL,
  awy_type TEXT, -- V, J, Q, T
  seq_no INTEGER,
  from_fix TEXT,
  to_fix TEXT,
  mea INTEGER, -- minimum enroute altitude
  maa INTEGER, -- maximum authorized altitude
  moca INTEGER, -- minimum obstruction clearance altitude
  distance NUMERIC,
  bearing NUMERIC
);

-- Additional tables follow same pattern
```

### Indexes

```sql
-- High-value query patterns
CREATE INDEX idx_airports_state ON airports(state_code);
CREATE INDEX idx_airports_arpt_id ON airports(arpt_id);
CREATE INDEX idx_airports_icao ON airports(icao_id);
CREATE INDEX idx_airports_coords ON airports(lat_decimal, long_decimal);

CREATE INDEX idx_fixes_id ON fixes(fix_id);
CREATE INDEX idx_fixes_coords ON fixes(lat_decimal, long_decimal);

CREATE INDEX idx_airways_id ON airways(awy_id);
CREATE INDEX idx_airways_fixes ON airways(from_fix, to_fix);

CREATE INDEX idx_navaids_id ON navaids(nav_id);
CREATE INDEX idx_navaids_type ON navaids(nav_type);
```

### Row Level Security

```sql
-- Read-only public access (same as airports_navaids)
ALTER TABLE airports ENABLE ROW LEVEL SECURITY;
CREATE POLICY "Public read access" ON airports FOR SELECT USING (true);

-- Repeat for all tables
```

## Project Structure

```
nasr_full/
├── R/
│   ├── scrape_nasr.R           # Main orchestrator
│   ├── download_nasr.R         # Parallel ZIP downloads
│   ├── clean_airports.R        # APT cleaning (full columns)
│   ├── clean_navaids.R         # NAV cleaning (full columns)
│   ├── clean_fixes.R           # FIX cleaning
│   ├── clean_airways.R         # AWY cleaning
│   ├── clean_procedures.R      # STAR, DP combined
│   ├── clean_airspace.R        # CLS_ARSP, ARB combined
│   ├── clean_facilities.R      # ATC, COM, FSS combined
│   ├── push_to_supabase.R      # Generic batch uploader
│   └── utils.R                 # Shared helpers
├── sql/
│   ├── create_tables.sql       # All table definitions
│   ├── create_indexes.sql      # Performance indexes
│   └── create_policies.sql     # RLS policies
├── tests/testthat/
│   ├── helper-setup.R
│   ├── test-clean_airports.R
│   ├── test-clean_fixes.R
│   └── ...
├── data/
│   ├── raw/                    # DD_MMM_YYYY_*_CSV/ directories
│   ├── clean/                  # Cleaned CSVs
│   └── pipeline_history.csv
├── docs/
│   └── data_validation.md      # Schema rules per table
├── .Renviron                   # New Supabase credentials
├── .Renviron.example
├── CLAUDE.md
├── README.md
└── renv.lock
```

## Implementation Plan

### Phase 1: Bootstrap (Week 1)

1. Clone `airports_navaids` to `nasr_full`
2. Create new Supabase project (`nasr-full`)
3. Update `.Renviron` with new credentials
4. Rename/refactor orchestrator to `scrape_nasr.R`
5. Verify existing APT/NAV pipeline works
6. Create initial `create_tables.sql` for airports/navaids (full columns)

### Phase 2: Fixes (Week 2)

1. Download and analyze FIX_BASE.csv structure
2. Create `clean_fixes.R` following project conventions
3. Add `fixes` table to schema
4. Write tests for `clean_fixes.R`
5. Verify end-to-end: download -> clean -> push -> query

### Phase 3: Airways (Week 3)

1. Analyze AWY CSV structure (multiple related files)
2. Design `airways` and `airway_segments` tables
3. Implement `clean_airways.R`
4. Handle fix references (foreign key or denormalized)
5. Test relational queries

### Phase 4: Procedures & ILS (Week 4)

1. Add ILS, HPF, STAR, DP tables
2. Implement cleaning functions
3. Consider grouping similar data types

### Phase 5: Remaining Data Types (Week 5-6)

1. PFR (Preferred Routes)
2. MTR (Military Training Routes)
3. ATC, COM, FSS (Facilities)
4. ARB, CLS_ARSP (Airspace)
5. AWOS (Weather)

### Phase 6: Polish (Week 7)

1. Add comprehensive indexes
2. Performance testing with large queries
3. Documentation
4. GitHub Actions for automated updates

## Testing Strategy

### Unit Tests

Each `clean_*.R` gets tests for:
- Happy path with sample data
- Missing required columns (expect error)
- Empty input (expect error)
- Edge cases (nulls, special characters)

### Integration Tests

- Full pipeline run with real FAA data
- Verify row counts match expectations
- Query tests across related tables

### Validation Rules

From `docs/data_validation.md`:
- Airports: lat 18-72, long -180 to -64
- Fixes: unique (fix_id, fix_type) pairs
- Airways: valid fix references
- All: eff_date matches FAA subscription date

## API Examples

```bash
# Get all fixes in a bounding box
curl "https://nasr-full.supabase.co/rest/v1/fixes?lat_decimal=gte.33&lat_decimal=lte.34&long_decimal=gte.-118&long_decimal=lte.-117" \
  -H "apikey: PUBLIC_KEY"

# Get airway segments for V23
curl "https://nasr-full.supabase.co/rest/v1/airways?awy_id=eq.V23&order=seq_no" \
  -H "apikey: PUBLIC_KEY"

# Get airports with ILS
curl "https://nasr-full.supabase.co/rest/v1/ils?select=arpt_id,rwy_id,ils_type" \
  -H "apikey: PUBLIC_KEY"
```

## Risk Assessment

| Risk | Likelihood | Impact | Mitigation |
|------|------------|--------|------------|
| FAA format changes | Medium | High | Version CSV structure files; alert on parse failures |
| Supabase rate limits | Low | Medium | Batch uploads already implemented |
| Storage exceeds 500MB | Low | High | Monitoring; can prune historical data |
| Relational integrity issues | Medium | Medium | Validate fix references before insert |

## Success Criteria

1. All 15+ data types loading without errors
2. Pipeline completes in < 30 minutes
3. API queries return expected results
4. 28-day updates work automatically via GitHub Actions
5. Database size stable at ~120-150 MB

## Open Questions

1. **Relational integrity:** Should airways have foreign keys to fixes, or just store fix_id as text?
   - Recommendation: Text (denormalized) for simplicity; fixes table is authoritative

2. **Historical data:** Keep previous cycles or overwrite?
   - Recommendation: Overwrite (same as airports_navaids); git tracks schema changes

3. **Subset views:** Create views matching airports_navaids schema for compatibility?
   - Recommendation: Not needed; projects serve different purposes

---

## Next Steps

1. Review and approve this design
2. Create `nasr_full` repository
3. Begin Phase 1 implementation
