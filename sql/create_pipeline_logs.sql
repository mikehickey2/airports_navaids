-- sql/create_pipeline_logs.sql
-- Schema for pipeline run logging
-- Run this in Supabase SQL Editor to create the table

CREATE TABLE IF NOT EXISTS pipeline_logs (
  id SERIAL PRIMARY KEY,
  run_timestamp TIMESTAMPTZ DEFAULT NOW(),
  status TEXT NOT NULL CHECK (status IN ('success', 'failure', 'warning')),
  airports_count INTEGER,
  navaids_count INTEGER,
  faa_eff_date DATE,
  next_expected_date DATE,
  error_message TEXT,
  duration_seconds INTEGER,
  triggered_by TEXT CHECK (triggered_by IN ('scheduled', 'manual', 'local'))
);

-- Enable Row Level Security
ALTER TABLE pipeline_logs ENABLE ROW LEVEL SECURITY;

-- Allow public read access
CREATE POLICY "Allow public read" ON pipeline_logs
  FOR SELECT USING (true);

-- Allow insert for authenticated/anon users (for automation)
CREATE POLICY "Allow insert" ON pipeline_logs
  FOR INSERT WITH CHECK (true);

-- Index for querying by status and date
CREATE INDEX idx_pipeline_logs_status ON pipeline_logs(status);
CREATE INDEX idx_pipeline_logs_timestamp ON pipeline_logs(run_timestamp DESC);
