#' Query Mark's fasola parsing minutes
#'
#' @param query_fn A function that takes a lazy tbl and returns a query
#' @export
query_db <- function(query_fn = identity) {

  # The raw URL ensures we get the actual SQLite file
  db_url <- "https://raw.githubusercontent.com/marktgodfrey/fasolaminutes_parsing/master/minutes.db"
  cache_path <- "data/database.sqlite"

  # Download if not cached
  if (!fs::file_exists(cache_path)) {
    message("Downloading database... this will take a second.")
    fs::dir_create(fs::path_dir(cache_path))
    download.file(db_url, cache_path, mode = "wb")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), cache_path)

  # TryCatch ensures the connection closes safely even if your query fails
  tryCatch({
    result <- query_fn(dplyr::tbl(con, "minutes")) |>
      dplyr::collect()
  }, finally = {
    DBI::dbDisconnect(con)
  })

  return(result)
}
