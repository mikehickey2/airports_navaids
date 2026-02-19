# test-update_readme.R
# Tests for R/update_readme.R

source_project_file("update_readme.R")

# --- Helper ---

write_test_readme <- function(dir, lines) {
  path <- file.path(dir, "README.md")
  writeLines(lines, path)
  path
}

# --- Input validation tests ---

test_that("update_readme validates faa_date argument", {
  temp_dir <- withr::local_tempdir()
  readme <- write_test_readme(temp_dir, "test")

  expect_error(
    update_readme("not-a-date", 5000, 1500, readme_path = readme)
  )

  expect_error(
    update_readme(NULL, 5000, 1500, readme_path = readme)
  )
})

test_that("update_readme validates count arguments", {
  temp_dir <- withr::local_tempdir()
  readme <- write_test_readme(temp_dir, "test")

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
  readme <- write_test_readme(temp_dir, c(
    "# Test README",
    "**Date:** <!-- pipeline:faa_date -->2025-01-01<!-- /pipeline:faa_date -->",
    "- <!-- pipeline:airports_count -->1,000<!-- /pipeline:airports_count --> airports",
    "- <!-- pipeline:navaids_count -->500<!-- /pipeline:navaids_count --> navaids"
  ))

  result <- update_readme(
    faa_date = as.Date("2026-02-28"),
    airports_count = 5308,
    navaids_count = 1650,
    readme_path = readme
  )

  expect_true(result)

  content <- readr::read_file(readme)
  expect_true(grepl("2026-02-28", content))
  expect_true(grepl("5,308", content))
  expect_true(grepl("1,650", content))
})

test_that("update_readme replaces all occurrences of same marker", {
  temp_dir <- withr::local_tempdir()
  readme <- write_test_readme(temp_dir, c(
    "First: <!-- pipeline:airports_count -->1,000<!-- /pipeline:airports_count -->",
    "Second: <!-- pipeline:airports_count -->1,000<!-- /pipeline:airports_count -->"
  ))

  # Suppress missing-marker warning (faa_date and navaids_count not present)
  suppressWarnings(
    update_readme(
      faa_date = as.Date("2026-01-01"),
      airports_count = 5308,
      navaids_count = 1650,
      readme_path = readme
    )
  )

  content <- readr::read_file(readme)
  matches <- gregexpr("5,308", content)[[1]]
  expect_equal(length(matches), 2)
})

# --- Edge cases ---

test_that("update_readme returns FALSE when content unchanged", {
  temp_dir <- withr::local_tempdir()
  readme <- write_test_readme(temp_dir, c(
    "**Date:** <!-- pipeline:faa_date -->2026-01-31<!-- /pipeline:faa_date -->",
    "<!-- pipeline:airports_count -->5,308<!-- /pipeline:airports_count --> airports",
    "<!-- pipeline:navaids_count -->1,650<!-- /pipeline:navaids_count --> navaids"
  ))

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
  readme <- write_test_readme(temp_dir, c(
    "# My Project",
    "Some description here.",
    "**Date:** <!-- pipeline:faa_date -->2025-01-01<!-- /pipeline:faa_date -->",
    "<!-- pipeline:airports_count -->100<!-- /pipeline:airports_count --> airports",
    "<!-- pipeline:navaids_count -->50<!-- /pipeline:navaids_count --> navaids",
    "More content below.",
    "## Section Two"
  ))

  update_readme(
    faa_date = as.Date("2026-02-28"),
    airports_count = 5000,
    navaids_count = 1500,
    readme_path = readme
  )

  content <- readLines(readme)
  expect_equal(content[1], "# My Project")
  expect_equal(content[2], "Some description here.")
  expect_equal(content[6], "More content below.")
  expect_equal(content[7], "## Section Two")
})

test_that("update_readme handles zero counts", {
  temp_dir <- withr::local_tempdir()
  readme <- write_test_readme(temp_dir, c(
    "<!-- pipeline:faa_date -->2025-01-01<!-- /pipeline:faa_date -->",
    "<!-- pipeline:airports_count -->5,308<!-- /pipeline:airports_count --> airports",
    "<!-- pipeline:navaids_count -->1,650<!-- /pipeline:navaids_count --> navaids"
  ))

  update_readme(
    faa_date = as.Date("2026-01-01"),
    airports_count = 0,
    navaids_count = 0,
    readme_path = readme
  )

  content <- readr::read_file(readme)
  expect_true(grepl(
    "<!-- pipeline:airports_count -->0<!-- /pipeline:airports_count -->",
    content,
    fixed = TRUE
  ))
  expect_true(grepl(
    "<!-- pipeline:navaids_count -->0<!-- /pipeline:navaids_count -->",
    content,
    fixed = TRUE
  ))
})

# --- Warning tests ---

test_that("update_readme warns when markers are missing", {
  temp_dir <- withr::local_tempdir()
  readme <- write_test_readme(temp_dir, c(
    "# Plain README",
    "No markers here.",
    "5,308 airports mentioned in prose."
  ))

  expect_warning(
    result <- update_readme(
      faa_date = as.Date("2026-01-01"),
      airports_count = 6000,
      navaids_count = 1700,
      readme_path = readme
    ),
    class = "update_readme_missing_markers"
  )

  # Content unchanged since no markers to replace
  expect_false(result)

  # Original content preserved
  content <- readLines(readme)
  expect_equal(content[3], "5,308 airports mentioned in prose.")
})

test_that("update_readme warns when some markers are missing", {
  temp_dir <- withr::local_tempdir()
  readme <- write_test_readme(temp_dir, c(
    "<!-- pipeline:faa_date -->2025-01-01<!-- /pipeline:faa_date -->",
    "No count markers here."
  ))

  expect_warning(
    update_readme(
      faa_date = as.Date("2026-01-01"),
      airports_count = 5000,
      navaids_count = 1500,
      readme_path = readme
    ),
    class = "update_readme_missing_markers"
  )
})
