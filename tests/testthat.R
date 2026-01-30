# testthat runner for airports_navaids project
# Run with: Rscript tests/testthat.R
# Or: testthat::test_dir("tests/testthat")

library(testthat)

# Source all R files to make functions available
r_dir <- file.path(dirname(dirname(getwd())), "R")
if (dir.exists("R")) {
 r_dir <- "R"
} else if (dir.exists("../R")) {
 r_dir <- "../R"
} else if (dir.exists("../../R")) {
 r_dir <- "../../R"
}

# Run tests
test_dir(
 path = "tests/testthat",
 reporter = "progress",
 stop_on_failure = FALSE
)
