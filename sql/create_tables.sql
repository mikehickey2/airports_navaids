-- Drop existing tables if they exist
DROP TABLE IF EXISTS airports;
DROP TABLE IF EXISTS navaids;

-- Create airports table (lowercase column names)
CREATE TABLE airports (
  id SERIAL PRIMARY KEY,
  eff_date DATE,
  site_no TEXT,
  site_type_code TEXT,
  state_code TEXT,
  arpt_id TEXT,
  city TEXT,
  country_code TEXT,
  state_name TEXT,
  county_name TEXT,
  arpt_name TEXT,
  lat_deg INTEGER,
  lat_min INTEGER,
  lat_sec NUMERIC,
  lat_hemis TEXT,
  lat_decimal NUMERIC,
  long_deg INTEGER,
  long_min INTEGER,
  long_sec NUMERIC,
  long_hemis TEXT,
  long_decimal NUMERIC,
  elev NUMERIC,
  elev_method_code TEXT,
  mag_varn NUMERIC,
  mag_hemis TEXT,
  mag_varn_year INTEGER,
  icao_id TEXT
);

-- Create navaids table (lowercase column names)
CREATE TABLE navaids (
  id SERIAL PRIMARY KEY,
  eff_date DATE,
  nav_id TEXT,
  nav_type TEXT,
  state_code TEXT,
  city TEXT,
  country_code TEXT,
  name TEXT,
  state_name TEXT,
  region_code TEXT,
  lat_hemis TEXT,
  lat_deg INTEGER,
  lat_min INTEGER,
  lat_sec NUMERIC,
  lat_decimal NUMERIC,
  long_hemis TEXT,
  long_deg INTEGER,
  long_min INTEGER,
  long_sec NUMERIC,
  long_decimal NUMERIC,
  elev NUMERIC,
  mag_varn NUMERIC,
  mag_varn_hemis TEXT,
  mag_varn_year INTEGER,
  alt_code TEXT
);

-- Enable Row Level Security
ALTER TABLE airports ENABLE ROW LEVEL SECURITY;
ALTER TABLE navaids ENABLE ROW LEVEL SECURITY;

-- Create policies for public read access
CREATE POLICY "Allow public read access on airports" ON airports
  FOR SELECT USING (true);

CREATE POLICY "Allow public read access on navaids" ON navaids
  FOR SELECT USING (true);

-- Write access: service_role key bypasses RLS
-- No explicit INSERT/DELETE policies needed for pipeline
-- The service_role key is used by the data pipeline (R scripts, GitHub Actions)
-- The anon key is used by API consumers (read-only via SELECT policy above)