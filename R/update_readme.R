# update_readme.R
# Updates README.md with current pipeline data (counts and FAA date)

library(readr)
library(checkmate)
library(rlang)

# Expected marker keys that should be present in README.md
expected_markers <- c("faa_date", "airports_count", "navaids_count")

#' Update README.md with current pipeline data
#'
#' Replaces marker-delimited values in README.md with current counts
#' and FAA subscription date. Markers use the format:
#' `<!-- pipeline:key -->value<!-- /pipeline:key -->`
#'
#' @param faa_date Date object: FAA subscription effective date
#' @param airports_count Integer: number of airports
#' @param navaids_count Integer: number of navaids
#' @param readme_path Character: path to README.md (default: "README.md")
#' @return Logical TRUE if README was modified, FALSE if unchanged
#' @export
update_readme <- function(faa_date,
                          airports_count,
                          navaids_count,
                          readme_path = "README.md") {
  checkmate::assert_date(faa_date, len = 1)
  checkmate::assert_integerish(airports_count, lower = 0, len = 1)
  checkmate::assert_integerish(navaids_count, lower = 0, len = 1)
  checkmate::assert_file_exists(readme_path)

  original <- readr::read_file(readme_path)

  # Build replacements: marker key -> formatted value
  replacements <- list(
    faa_date = format(faa_date, "%Y-%m-%d"),
    airports_count = trimws(format(airports_count, big.mark = ",")),
    navaids_count = trimws(format(navaids_count, big.mark = ","))
  )

  # Check for missing markers before attempting replacements
  warn_missing_markers(original)

  updated <- original
  for (key in names(replacements)) {
    pattern <- paste0(
      "<!-- pipeline:", key, " -->",
      ".*?",
      "<!-- /pipeline:", key, " -->"
    )
    replacement <- paste0(
      "<!-- pipeline:", key, " -->",
      replacements[[key]],
      "<!-- /pipeline:", key, " -->"
    )
    updated <- gsub(pattern, replacement, updated, perl = TRUE)
  }

  if (identical(updated, original)) {
    message("README.md is already up to date")
    return(invisible(FALSE))
  }

  readr::write_file(updated, readme_path)
  message("Updated README.md with current pipeline data")
  invisible(TRUE)
}

#' Warn if expected pipeline markers are missing from content
#'
#' @param content Character string of README content
#' @return Invisible NULL
#' @keywords internal
warn_missing_markers <- function(content) {
  missing <- character(0)
  for (key in expected_markers) {
    pattern <- paste0("<!-- pipeline:", key, " -->")
    if (!grepl(pattern, content, fixed = TRUE)) {
      missing <- c(missing, key)
    }
  }

  if (length(missing) > 0) {
    rlang::warn(
      c(
        "README.md is missing expected pipeline markers",
        i = paste("Missing:", paste(missing, collapse = ", ")),
        i = "Markers use format: <!-- pipeline:key -->value<!-- /pipeline:key -->"
      ),
      class = "update_readme_missing_markers"
    )
  }

  invisible(NULL)
}
