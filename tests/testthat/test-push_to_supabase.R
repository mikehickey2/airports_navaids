# test-push_to_supabase.R
# Tests for R/push_to_supabase.R functions

# Source the refactored functions
source_project_file("push_to_supabase.R")

test_that("push_to_supabase validates table_name argument", {
  data <- sample_airports(3)

  # NULL table_name
  expect_error(
    push_to_supabase(NULL, data),
    class = "simpleError"
  )

  # Empty string table_name
  expect_error(
    push_to_supabase("", data),
    class = "simpleError"
  )

  # Numeric table_name
  expect_error(
    push_to_supabase(123, data),
    class = "simpleError"
  )
})

test_that("push_to_supabase validates data argument", {
  # NULL data
  expect_error(
    push_to_supabase("airports", NULL),
    class = "simpleError"
  )

  # Not a data frame
  expect_error(
    push_to_supabase("airports", "not a dataframe"),
    class = "simpleError"
  )

  # Empty data frame
  expect_error(
    push_to_supabase("airports", tibble::tibble()),
    class = "simpleError"
  )
})

test_that("push_to_supabase validates batch_size argument", {
  data <- sample_airports(3)

  # batch_size = 0
  expect_error(
    push_to_supabase("airports", data, batch_size = 0),
    class = "simpleError"
  )

  # Negative batch_size
  expect_error(
    push_to_supabase("airports", data, batch_size = -1),
    class = "simpleError"
  )

  # batch_size > 5000 (upper limit)
  expect_error(
    push_to_supabase("airports", data, batch_size = 10000),
    class = "simpleError"
  )
})

test_that("clear_table validates table_name argument", {
  # NULL table_name
  expect_error(
    clear_table(NULL),
    class = "simpleError"
  )

  # Empty string table_name
  expect_error(
    clear_table(""),
    class = "simpleError"
  )
})

test_that("get_supabase_config aborts when SUPABASE_API_KEY is missing", {
  withr::local_envvar(
    SUPABASE_API_KEY = ""
  )

  expect_error(
    get_supabase_config(),
    class = "supabase_auth_error"
  )
})

test_that("NA values are converted to NULL in JSON", {
  # This tests the convert_na_to_null helper function
  data_with_na <- tibble::tibble(
    name = c("Test", NA, "Another"),
    value = c(1, 2, NA)
  )

  row_with_na <- as.list(data_with_na[2, ])
  converted <- convert_na_to_null(row_with_na)

  expect_null(converted$name)
  expect_equal(converted$value, 2)
})

test_that("get_supabase_config returns config when API key is set", {
  withr::local_envvar(
    SUPABASE_API_KEY = "test_key_not_real"
  )

  config <- get_supabase_config()

  expect_type(config, "list")
  expect_named(config, c("url", "api_key"))
  expect_equal(config$api_key, "test_key_not_real")
  expect_match(config$url, "supabase.co")
})

test_that("get_supabase_config respects SUPABASE_URL override", {
  withr::local_envvar(
    SUPABASE_API_KEY = "test_key_not_real",
    SUPABASE_URL = "https://custom.supabase.co"
  )

  config <- get_supabase_config()

  expect_equal(config$url, "https://custom.supabase.co")
})

# --- HTTP Mocking Tests (httptest2) ---

test_that("push_to_supabase makes correct POST request", {
  skip_if_not_installed("httptest2")
  source_project_file("push_to_supabase.R")

  withr::local_envvar(
    SUPABASE_API_KEY = MOCK_SUPABASE_API_KEY,
    SUPABASE_URL = MOCK_SUPABASE_URL
  )

  data <- sample_airports(2)

  # without_internet blocks requests; httptest2 verifies request method and URL
  httptest2::without_internet({
    httptest2::expect_POST(
      push_to_supabase("airports", data),
      url = paste0(MOCK_SUPABASE_URL, "/rest/v1/airports")
    )
  })
})

test_that("push_to_supabase sends JSON body with data", {
  skip_if_not_installed("httptest2")
  source_project_file("push_to_supabase.R")

  withr::local_envvar(
    SUPABASE_API_KEY = MOCK_SUPABASE_API_KEY,
    SUPABASE_URL = MOCK_SUPABASE_URL
  )

  data <- sample_airports(1)

  # Verify request body contains expected field (use regex to match in body)
  httptest2::without_internet({
    expect_error(
      push_to_supabase("airports", data),
      regexp = '"arpt_id"',
      fixed = TRUE
    )
  })
})

test_that("clear_table makes correct DELETE request", {
  skip_if_not_installed("httptest2")
  source_project_file("push_to_supabase.R")

  withr::local_envvar(
    SUPABASE_API_KEY = MOCK_SUPABASE_API_KEY,
    SUPABASE_URL = MOCK_SUPABASE_URL
  )

  httptest2::without_internet({
    httptest2::expect_DELETE(
      clear_table("airports"),
      url = paste0(MOCK_SUPABASE_URL, "/rest/v1/airports")
    )
  })
})

test_that("clear_table targets correct table in URL", {
  skip_if_not_installed("httptest2")
  source_project_file("push_to_supabase.R")

  withr::local_envvar(
    SUPABASE_API_KEY = MOCK_SUPABASE_API_KEY,
    SUPABASE_URL = MOCK_SUPABASE_URL
  )

  # Verify table name appears in URL for navaids table
  httptest2::without_internet({
    httptest2::expect_DELETE(
      clear_table("navaids"),
      url = paste0(MOCK_SUPABASE_URL, "/rest/v1/navaids")
    )
  })
})
