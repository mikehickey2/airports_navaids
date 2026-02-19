# test-update_readme.R
# Tests for R/update_readme.R

source_project_file("update_readme.R")

# --- Input validation tests ---

test_that("update_readme validates faa_date argument", {
  temp_dir <- withr::local_tempdir()
  readme <- file.path(temp_dir, "README.md")
  writeLines("test", readme)

  expect_error(
    update_readme("not-a-date", 5000, 1500, readme_path = readme)
  )

  expect_error(
    update_readme(NULL, 5000, 1500, readme_path = readme)
  )
})

test_that("update_readme validates count arguments", {
  temp_dir <- withr::local_tempdir()
  readme <- file.path(temp_dir, "README.md")
  writeLines("test", readme)

  expect_error(
    update_readme(Sys.Date(), "abc", 1500, readme_path = readme)
  )

  expect_error(
    update_readme(Sys.Date(), 5000, -1, readme_path = readme)
  )
})

test_that("update_readme validates readme_path exists", {
  expect_error(
    update_readme(Sys.Date(), 5000, 1500,
                  readme_path = "/nonexistent/README.md")
  )
})

# --- Happy path tests ---

test_that("update_readme replaces marker values", {
  temp_dir <- withr::local_tempdir()
  readme <- file.path(temp_dir, "README.md")

  writeLines(c(
    "# Test README",
    "**Date:** <!-- pipeline:faa_date -->2025-01-01<!-- /pipeline:faa_date -->",
    "- <!-- pipeline:airports_count -->1,000<!-- /pipeline:airports_count --> airports",
    "- <!-- pipeline:navaids_count -->500<!-- /pipeline:navaids_count --> navaids"
  ), readme)

  result <- update_readme(
    faa_date = as.Date("2026-02-28"),
    airports_count = 5308,
    navaids_count = 1650,
    readme_path = readme
  )

  expect_true(result)

  content <- readLines(readme)
  expect_true(any(grepl("2026-02-28", content)))
  expect_true(any(grepl("5,308", content)))
  expect_true(any(grepl("1,650", content)))
})

test_that("update_readme replaces all occurrences of same marker", {
  temp_dir <- withr::local_tempdir()
  readme <- file.path(temp_dir, "README.md")

  writeLines(c(
    "First: <!-- pipeline:airports_count -->1,000<!-- /pipeline:airports_count -->",
    "Second: <!-- pipeline:airports_count -->1,000<!-- /pipeline:airports_count -->"
  ), readme)

  update_readme(
    faa_date = as.Date("2026-01-01"),
    airports_count = 5308,
    navaids_count = 1650,
    readme_path = readme
  )

  content <- paste(readLines(readme), collapse = "\n")
  matches <- gregexpr("5,308", content)[[1]]
  expect_equal(length(matches), 2)
})

# --- Edge cases ---

test_that("update_readme returns FALSE when content unchanged", {
  temp_dir <- withr::local_tempdir()
  readme <- file.path(temp_dir, "README.md")

  writeLines(c(
    "**Date:** <!-- pipeline:faa_date -->2026-01-31<!-- /pipeline:faa_date -->",
    "<!-- pipeline:airports_count -->5,308<!-- /pipeline:airports_count --> airports",
    "<!-- pipeline:navaids_count -->1,650<!-- /pipeline:navaids_count --> navaids"
  ), readme)

  result <- update_readme(
    faa_date = as.Date("2026-01-31"),
    airports_count = 5308,
    navaids_count = 1650,
    readme_path = readme
  )

  expect_false(result)
})

test_that("update_readme preserves non-marker content", {
  temp_dir <- withr::local_tempdir()
  readme <- file.path(temp_dir, "README.md")

  writeLines(c(
    "# My Project",
    "Some description here.",
    "**Date:** <!-- pipeline:faa_date -->2025-01-01<!-- /pipeline:faa_date -->",
    "More content below.",
    "## Section Two"
  ), readme)

  update_readme(
    faa_date = as.Date("2026-02-28"),
    airports_count = 5000,
    navaids_count = 1500,
    readme_path = readme
  )

  content <- readLines(readme)
  expect_equal(content[1], "# My Project")
  expect_equal(content[2], "Some description here.")
  expect_equal(content[4], "More content below.")
  expect_equal(content[5], "## Section Two")
})

test_that("update_readme handles zero counts", {
  temp_dir <- withr::local_tempdir()
  readme <- file.path(temp_dir, "README.md")

  writeLines(c(
    "<!-- pipeline:airports_count -->5,308<!-- /pipeline:airports_count --> airports"
  ), readme)

  result <- update_readme(
    faa_date = as.Date("2026-01-01"),
    airports_count = 0,
    navaids_count = 0,
    readme_path = readme
  )

  content <- readLines(readme)
  expect_true(grepl("<!-- pipeline:airports_count -->0<!-- /pipeline:airports_count -->", content[1]))
})

# --- Error condition tests ---

test_that("update_readme does not crash on README without markers", {
  temp_dir <- withr::local_tempdir()
  readme <- file.path(temp_dir, "README.md")

  writeLines(c(
    "# Plain README",
    "No markers here.",
    "5,308 airports mentioned in prose."
  ), readme)

  result <- update_readme(
    faa_date = as.Date("2026-01-01"),
    airports_count = 6000,
    navaids_count = 1700,
    readme_path = readme
  )

  # No markers found, so content is unchanged
  expect_false(result)

  # Original content preserved
  content <- readLines(readme)
  expect_equal(content[3], "5,308 airports mentioned in prose.")
})
