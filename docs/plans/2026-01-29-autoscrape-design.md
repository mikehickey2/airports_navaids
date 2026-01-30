# NASR Auto-Scrape with Notifications

**Date:** 2026-01-29
**Branch:** `feat-autoscrape-01hy`
**Status:** Design approved

## Overview

Automate the FAA NASR data pipeline to run via GitHub Actions, with email and SMS notifications for updates, failures, and reminders.

## Goals

1. Daily check of FAA NASR page for new subscription data
2. Automatic pipeline execution when new data is available
3. Notifications via email (SendGrid) and/or SMS (Twilio)
4. Reminder notification 1 day before expected updates
5. Logging to Supabase table and repo CSV file

## Schedule Logic

The FAA NASR page shows both "Current" and "Preview" subscription dates. The workflow:

1. Runs daily at 12:00 UTC
2. Scrapes FAA page for Current + Preview dates
3. Compares to today's date:
   - If tomorrow = Preview date: send reminder
   - If today within +/- 1 day of Preview becoming Current: run pipeline
   - Otherwise: exit early (~10 seconds)

## Notification Types

| Type | Trigger | Content |
|------|---------|---------|
| Success | Pipeline completes | New data date, record counts, next expected date |
| Failure | Pipeline error | Error message, failed stage, link to logs |
| Reminder | 1 day before expected update | "Update expected tomorrow" |
| Warning | Row count mismatch | Expected vs actual counts |

## User Configuration

### GitHub Secrets (required for Actions)

```
NOTIFY_EMAIL_ENABLED    # "true" or "false"
NOTIFY_SMS_ENABLED      # "true" or "false"
NOTIFY_EMAIL            # recipient@example.com
NOTIFY_PHONE            # +15551234567
TWILIO_ACCOUNT_SID      # ACxxxxx
TWILIO_AUTH_TOKEN       # xxxxx
TWILIO_FROM_NUMBER      # +15559876543
SENDGRID_API_KEY        # SG.xxxxx
SUPABASE_API_KEY        # (already configured)
```

### .Renviron (for local runs)

Same variables as above, stored in `.Renviron` (gitignored).

Users choose email, SMS, or both by setting the `_ENABLED` flags.

## Logging

### Supabase table: `pipeline_logs`

```sql
CREATE TABLE pipeline_logs (
  id SERIAL PRIMARY KEY,
  run_timestamp TIMESTAMPTZ DEFAULT NOW(),
  status TEXT NOT NULL,
  airports_count INTEGER,
  navaids_count INTEGER,
  faa_eff_date DATE,
  next_expected_date DATE,
  error_message TEXT,
  duration_seconds INTEGER,
  triggered_by TEXT
);
```

### Repo file: `data/pipeline_history.csv`

Committed to repo for visible history. Columns mirror the Supabase table.

Retention: Forever (estimated 26 KB per decade).

## GitHub Actions Workflow

**File:** `.github/workflows/nasr-autoscrape.yml`

```
Jobs:
1. check-schedule
   - Scrape FAA page
   - Output: run_pipeline (bool), send_reminder (bool), preview_date

2. send-reminder (if send_reminder = true)
   - Send "update expected tomorrow" notification

3. run-pipeline (if run_pipeline = true)
   - Checkout repo
   - Setup R + restore renv
   - Run pipeline
   - On success: notify + log
   - On failure: notify + log
   - Commit pipeline_history.csv
```

## File Changes

### New Files

| File | Purpose |
|------|---------|
| `R/notify.R` | Notification functions (email, SMS, logging) |
| `.github/workflows/nasr-autoscrape.yml` | Daily auto-scrape workflow |
| `sql/create_pipeline_logs.sql` | Schema for logs table |
| `data/pipeline_history.csv` | Local log file (committed) |

### Modified Files

| File | Changes |
|------|---------|
| `R/scrape_airports_navaids.R` | Add `get_faa_dates()`, enhance `run_pipeline()` return |
| `R/push_to_supabase.R` | Add `log_to_supabase()` function |
| `.Renviron.example` | Add notification credential templates |
| `README.md` | Document auto-scrape setup |

## R Functions

### R/notify.R (new)

```r
send_notification(type, data)
send_email(subject, body)
send_sms(message)
log_pipeline_run(status, airports_count, navaids_count, ...)
```

### R/scrape_airports_navaids.R (modified)

```r
get_faa_dates()
# Returns: list(current_date, preview_date)

run_pipeline(force = FALSE, dry_run = FALSE)
# Enhanced to return structured result with counts and dates
```

### R/push_to_supabase.R (modified)

```r
log_to_supabase(log_data)
# POST to pipeline_logs table
```

## Message Templates

### Success
```
Subject: [FAA NASR] Update Complete - {date}

FAA NASR data has been updated.

New effective date: February 19, 2026
Records loaded:
  - Airports: 5,312
  - Navaids: 1,658

Next update expected: March 19, 2026
```

### Failure
```
Subject: [FAA NASR] Pipeline Failed - Action Required

The NASR update pipeline encountered an error.

Error: {error_message}
Stage: {scrape|clean|push}

View logs: {github_actions_run_url}
```

### Reminder
```
Subject: [FAA NASR] Update Expected Tomorrow

The next FAA NASR subscription is expected tomorrow.

Expected date: February 19, 2026
Pipeline will run automatically.
```

## Verification

### Unit Tests (tests/testthat/test-notify.R)

- `send_email()` with mock/dry-run
- `send_sms()` with mock/dry-run
- `log_to_supabase()` writes correctly
- `get_faa_dates()` parses FAA page

### Integration Tests (manual)

1. Trigger workflow manually via `workflow_dispatch`
2. Verify notification received
3. Verify `pipeline_logs` table updated
4. Verify `pipeline_history.csv` committed

### Dry-Run Mode

`DRY_RUN=true` environment variable:
- Prints notifications to console
- Skips Supabase push
- Useful for local testing

### Quality Gates

1. `lintr::lint_dir('R')` - no errors
2. `testthat::test_dir('tests/testthat')` - all pass
3. Workflow runs successfully in GitHub Actions

## Dependencies

### External Services

| Service | Purpose | Setup |
|---------|---------|-------|
| Twilio | SMS notifications | https://www.twilio.com/try-twilio |
| SendGrid | Email notifications | Access via Twilio console |
| Supabase | Database (existing) | Already configured |

### R Packages (existing)

- httr2 (API calls)
- tidyverse, checkmate, rlang (already in renv.lock)

No new R package dependencies required.
