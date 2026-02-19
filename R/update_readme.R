# update_readme.R
# Updates README.md with current pipeline data (counts and FAA date)

library(checkmate)

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

  original_text <- readLines(readme_path, warn = FALSE)
  content <- paste(original_text, collapse = "\n")

  # Build replacements: marker key -> formatted value
  replacements <- list(
    faa_date = format(faa_date, "%Y-%m-%d"),
    airports_count = trimws(format(airports_count, big.mark = ",")),
    navaids_count = trimws(format(navaids_count, big.mark = ","))
  )

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
    content <- gsub(pattern, replacement, content, perl = TRUE)
  }

  new_text <- strsplit(content, "\n", fixed = TRUE)[[1]]

  if (identical(new_text, original_text)) {
    message("README.md is already up to date")
    return(invisible(FALSE))
  }

  writeLines(new_text, readme_path)
  message("Updated README.md with current pipeline data")
  invisible(TRUE)
}
