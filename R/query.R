utils::globalVariables(c("id", "Date", "Year", "parsed", "minutes_id", "date_start", "date_end"))

#' Query the fasola minutes database
#'
#' @param query_fn A function that takes a pre-joined tbl and returns a query.
#' @param include_location Add city, state_province, country, gps_lat, gps_long.
#' @param include_song_detail Add meter, music_attribution, three_liner.
#' @param include_leader_detail Add lesson_count, song_entropy, top20_count, location_count.
#' @param name Name of the variable assigned in the global environment.
#' @export
query_db <- function(
  query_fn = identity,
  include_location      = FALSE,
  include_song_detail   = FALSE,
  include_leader_detail = FALSE,
  name = "fasola_df"
) {

  db_url     <- "https://raw.githubusercontent.com/marktgodfrey/fasolaminutes_parsing/master/minutes.db"
  cache_dir  <- tools::R_user_dir("fasola", "cache")
  cache_path <- file.path(cache_dir, "database.sqlite")

  if (!fs::file_exists(cache_path)) {
    message("Downloading database... this will take a second.")
    fs::dir_create(fs::path_dir(cache_path))
    utils::download.file(db_url, cache_path, mode = "wb")
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), cache_path)

  feeling_lucky()

  tryCatch({
    cli::cli_progress_step("Querying database")
    joined <- dplyr::tbl(con, "song_leader_joins") |>
      dplyr::inner_join(dplyr::tbl(con, "leaders"), by = c("leader_id" = "id")) |>
      dplyr::inner_join(dplyr::tbl(con, "songs"),   by = c("song_id"   = "id")) |>
      dplyr::inner_join(dplyr::tbl(con, "minutes"), by = c("minutes_id" = "id"))

    if (include_location) {
      joined <- joined |>
        dplyr::inner_join(dplyr::tbl(con, "minutes_location_joins"), by = "minutes_id") |>
        dplyr::inner_join(dplyr::tbl(con, "locations"), by = c("location_id" = "id"))
    }

    cli::cli_progress_step("Parsing dates")
    minutes_raw <- dplyr::tbl(con, "minutes") |>
      dplyr::select(minutes_id = id, Date, Year) |>
      dplyr::collect()

    pb <- cli::cli_progress_bar("Parsing dates", total = nrow(minutes_raw))
    minutes_dates <- minutes_raw |>
      dplyr::mutate(
        parsed = purrr::map2(Date, Year, function(d, y) { cli::cli_progress_update(id = pb); parse_fasola_date(d, year = y) }),
        date_start = as.Date(purrr::map_chr(parsed, "date_start")),
        date_end   = as.Date(purrr::map_chr(parsed, "date_end"))
      ) |>
      dplyr::select(minutes_id, date_start, date_end)
    cli::cli_progress_done(id = pb)

    cli::cli_progress_step("Collecting results")
    result <- query_fn(joined) |>
      dplyr::collect() |>
      dplyr::left_join(minutes_dates, by = "minutes_id")
  }, finally = {
    DBI::dbDisconnect(con)
  })

  # Base columns
  base_cols <- c("leader_id", "leader_name" = "name", "song_id",
                 "song_title" = "title", "minutes_id", "year" = "Year",
                 "date_start", "date_end")

  leader_detail_cols <- c("lesson_count", "song_entropy", "top20_count", "location_count")
  song_detail_cols   <- c("meter", "music_attribution", "three_liner", "denson" = "IsDenson", "denson_year" = "DensonYear")
  location_cols      <- c("location_id", "city", "county", "state_province", "country", "gps_lat", "gps_long", "virtual" = "IsVirtual")

  keep <- base_cols
  if (include_leader_detail) keep <- c(keep, leader_detail_cols)
  if (include_song_detail)   keep <- c(keep, song_detail_cols)
  if (include_location)      keep <- c(keep, location_cols)

  result <- dplyr::select(result, dplyr::any_of(keep)) |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c("denson", "virtual")), as.logical))

  assign(name, result, envir = .GlobalEnv)
}
