months_lookup <- c(
  January=1, February=2, March=3, April=4, May=5, June=6,
  July=7, August=8, September=9, October=10, November=11, December=12
)

parse_fasola_date <- function(date_str, year = NA) {
  na_result <- list(date_start = NA_character_, date_end = NA_character_)
  if (is.na(date_str) || nchar(trimws(date_str)) == 0) return(na_result)

  s <- trimws(date_str)
  s <- sub("\\.$", "", s)                                    # trailing period
  s <- sub("^(\\w+)\\s*\\.\\s*", "\\1 ", s)                 # "August. 24" -> "August 24"
  weekdays_pat <- "Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday"
  s <- sub(paste0("^(?:", weekdays_pat, ")\\s+[Nn]ight,?\\s+"), "", s)   # "Friday Night, ..."
  s <- sub(paste0("^(?:", weekdays_pat, "),\\s+"), "", s)                 # "Friday, ..."
  s <- gsub(paste0("([-\u2013]\\s*)(?:", weekdays_pat, "),\\s+"), "\\1", s)   # weekday on both sides of cross-month dash
  s <- gsub("\\bl(\\d{3})\\b", "1\\1", s)                  # OCR: 'l' instead of '1' in year
  s <- gsub("([A-Za-z])(\\d)", "\\1 \\2", s)               # missing space: "June12" -> "June 12"
  s <- gsub("^(\\w+),\\s+(\\d)", "\\1 \\2", s)             # extra comma after month: "February, 27"
  s <- gsub("(\\d)\\s+-", "\\1-", s)                       # space before dash: "15 -16" -> "15-16"
  s <- sub("(,\\s*\\d{4}).*$", "\\1", s)                   # truncate duplicate appended date
  s <- gsub("(\\d)\\s+(\\d{4})$", "\\1, \\2", s)           # missing comma before year
  s <- gsub("\\b(\\d{4})\\d+\\b", "\\1", s)                # typo year: "20001" -> "2000" (truncate)

  # Cross-month with lead day: "March 31, April 1-2, 1995"
  m <- regmatches(s, regexec(
    "^(\\w+)\\s+(\\d+),\\s+(\\w+)\\s+(\\d+)-(\\d+),\\s*(\\d{4})$", s))[[1]]
  if (length(m) == 7) {
    yr    <- as.integer(m[7])
    start <- lubridate::make_date(yr, months_lookup[m[2]], as.integer(m[3]))
    end   <- lubridate::make_date(yr, months_lookup[m[4]], as.integer(m[6]))
    return(list(date_start = as.character(start), date_end = as.character(end)))
  }

  # Comma-separated multi-day: "June 12, 13, 14, 1997" -> date_start = June 12, date_end = June 14
  m <- regmatches(s, regexec(
    "^(\\w+)\\s+(\\d+)(?:,\\s*\\d+)*,\\s*(\\d+),\\s*(\\d{4})$", s))[[1]]
  if (length(m) == 5) {
    yr    <- as.integer(m[5])
    month <- months_lookup[m[2]]
    start <- lubridate::make_date(yr, month, as.integer(m[3]))
    end   <- lubridate::make_date(yr, month, as.integer(m[4]))
    return(list(date_start = as.character(start), date_end = as.character(end)))
  }

  # Cross-month spaced: "Month D, Year - Month D, Year" or "Month D-Month D, Year"
  m <- regmatches(s, regexec(
    "^(\\w+)\\s+(\\d+),?\\s*\\d*\\s*-\\s*(\\w+)\\s+(\\d+),\\s*(\\d{4})$", s))[[1]]
  if (length(m) == 6) {
    yr    <- as.integer(m[6])
    start <- lubridate::make_date(yr, months_lookup[m[2]], as.integer(m[3]))
    end   <- lubridate::make_date(yr, months_lookup[m[4]], as.integer(m[5]))
    return(list(date_start = as.character(start), date_end = as.character(end)))
  }

  # Cross-month year boundary: "Month D, Year-Month D, Year"
  m <- regmatches(s, regexec(
    "^(\\w+)\\s+(\\d+),\\s*(\\d{4})\\s*-\\s*(\\w+)\\s+(\\d+),\\s*(\\d{4})$", s))[[1]]
  if (length(m) == 7) {
    start <- lubridate::make_date(as.integer(m[4]), months_lookup[m[2]], as.integer(m[3]))
    end   <- lubridate::make_date(as.integer(m[7]), months_lookup[m[5]], as.integer(m[6]))
    return(list(date_start = as.character(start), date_end = as.character(end)))
  }

  # Same-month range: "Month D-D, Year"
  m <- regmatches(s, regexec(
    "^(\\w+)\\s+(\\d+)-(\\d+),\\s*(\\d{4})$", s))[[1]]
  if (length(m) == 5) {
    yr    <- as.integer(m[5])
    month <- months_lookup[m[2]]
    start <- lubridate::make_date(yr, month, as.integer(m[3]))
    end   <- lubridate::make_date(yr, month, as.integer(m[4]))
    return(list(date_start = as.character(start), date_end = as.character(end)))
  }

  # Single day
  d <- lubridate::mdy(s, quiet = TRUE)
  if (!is.na(d)) return(list(date_start = as.character(d), date_end = as.character(d)))

  # Fallback: try appending the Year column value
  if (!is.na(year)) {
    d <- lubridate::mdy(paste0(s, ", ", year), quiet = TRUE)
    if (!is.na(d)) return(list(date_start = as.character(d), date_end = as.character(d)))
  }

  na_result
}
