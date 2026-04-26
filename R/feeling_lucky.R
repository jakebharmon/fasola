#' Print a random Sacred Harp song lyric
#'
#' Fetches a random lyric from the 2025 Sacred Harp edition on GitHub and prints it.
#' Called automatically when loading the database, or run it directly anytime.
#'
#' @export
feeling_lucky <- function() {
  tryCatch({
    api_url  <- "https://api.github.com/repos/Sacred-Harp-Publishing-Company/2025-edition/contents/lyrics"
    response <- jsonlite::fromJSON(api_url)
    file     <- response$download_url[sample(nrow(response), 1)]
    lines    <- readLines(url(file), warn = FALSE)

    title  <- sub("^#\\s*", "", lines[1])
    lyrics <- paste(lines[-1], collapse = "\n") |> trimws()

    cli::cli_rule(center = paste0(" ", title, " "))
    cli::cli_text("{.emph {lyrics}}")
    cli::cli_rule()
    cat("\n")
  }, error = function(e) invisible(NULL))
}
