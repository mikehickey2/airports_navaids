# Contributing to FAA Airports & Navaids

## Purpose

This repository provides a data pipeline for ingesting FAA aeronautical reference data
(airports and navaids) from the NASR 28-day subscription, storing it in Supabase PostgreSQL,
and exposing it via REST API.

## Setup

```bash
git clone https://github.com/mikehickey2/airports_navaids.git
cd airports_navaids
Rscript -e "renv::restore()"
```

## Common Commands

### Quality Gates

```bash
# Run all tests
Rscript -e "testthat::test_dir('tests/testthat')"

# Run a single test file
Rscript -e "testthat::test_file('tests/testthat/test-clean_data.R')"

# Lint R files
Rscript -e "lintr::lint_dir('R')"

# Lint a single file
Rscript -e "lintr::lint('R/clean_data.R')"
```

### Package Management

```bash
# Restore packages from lockfile
Rscript -e "renv::restore()"

# Add a new package
Rscript -e "renv::install('package_name')"

# Snapshot current packages
Rscript -e "renv::snapshot(type = 'all')"
```

### Run Pipeline

```r
# Full pipeline: check FAA, download, clean, push
source("R/scrape_airports_navaids.R")

# Push existing clean data to Supabase
source("R/push_to_supabase.R")
```

## Quality Gates

All enforced gates must pass before merge.

### Enforced Gates

| Gate | Tool | Fails When |
|------|------|------------|
| Line length | `.lintr` (lintr) | Any line exceeds 110 characters |
| Lint check | `lintr::lint_dir('R')` | lintr reports any issues |
| Test suite | `testthat::test_dir()` | Any testthat test fails |

### Policy Gates (Code Review)

| Gate | Expectation |
|------|-------------|
| Script size soft limit | Scripts should stay under 300 lines; refactor if exceeded |
| Function size limit | Functions should stay under 80 lines |
| Fail-loud conventions | No `suppressWarnings()`, `suppressMessages()`, or `tryCatch` without approval |
| Tidyverse style | Follow tidyverse conventions; use `styler` for formatting |

## Branch and PR Workflow

### Branch Naming

Use descriptive prefixes:

```
feat/add-runway-data
fix/coordinate-parsing
docs/update-schema-docs
refactor/extract-api-helpers
test/add-push-coverage
data/update-faa-subscription
```

### Commit Messages

Follow [Conventional Commits](https://www.conventionalcommits.org/):

```
<type>(<scope>): <subject>
```

**Types:** `feat`, `fix`, `data`, `docs`, `refactor`, `test`, `chore`

**Examples:**

```
feat(scrape): add retry logic for FAA downloads
fix(clean): correct state_code filtering
docs: update Supabase configuration section
refactor(push): extract batch upload to helper
test(clean): add edge cases for empty CSV
chore(renv): snapshot new package version
data: update to December 2025 FAA subscription
```

### Push and PR

1. Run all quality gates locally before pushing
2. Push branch and open a PR
3. Request review before merging (even for solo work)
4. Merge is blocked if any enforced gate fails

## Directory Structure

```
airports_navaids/
|-- R/
|   |-- scrape_airports_navaids.R  # Main orchestrator
|   |-- clean_data.R               # Data transformation
|   |-- push_to_supabase.R         # Database upload
|-- sql/
|   |-- create_tables.sql          # PostgreSQL schema
|-- tests/testthat/                # testthat test files
|-- data/
|   |-- raw/                       # Downloaded FAA CSV files (dated dirs)
|   |-- clean/                     # Processed outputs (airports.csv, navaids.csv)
|-- .claude/                       # Claude Code agents and skills
```

**Do not create new top-level directories without approval.**

Approved directories: `R/`, `scripts/`, `tests/`, `sql/`, `data/`, `.claude/`, `renv/`

## Testing Conventions

### File Naming

Test files must match source files:
- `R/clean_data.R` -> `tests/testthat/test-clean_data.R`
- `R/push_to_supabase.R` -> `tests/testthat/test-push_to_supabase.R`

### Patterns

- Use `on.exit(unlink(tmp, recursive = TRUE), add = TRUE)` for temp file cleanup
- Use `expect_error(..., class = "error_class")` over pattern matching
- Use `expect_identical()` for deterministic outputs (hashes, IDs)
- Source files with `source(here::here("R/file.R"))`

## Raw Data Handling

Raw data files in `data/raw/` should not be modified after download.

The pipeline:
1. Downloads dated FAA subscription files to `data/raw/{date}_*_CSV/`
2. Cleans and filters data to `data/clean/`
3. Pushes clean data to Supabase (deletes then inserts)

Old raw data directories are deleted when new data is downloaded.

## Line Length Policy

- **Default target**: 80 characters per line for general R code
- **Allowed exceptions up to 110 characters** when wrapping reduces clarity:
  - File paths, regex patterns, URL strings
  - Long function calls that become harder to read when split
- **Hard stop at 120 characters**: refactor if exceeded
- Use `# nolint` only for rare cases with explanatory comment
