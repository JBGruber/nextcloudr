#' List remote files
#'
#' @param path remote path starting in your home directory.
#' @param token token. Use `login()` to connect to a server or provide token
#'   directly here.
#' @param verbose print status to screen?
#'
#' @return a data.frame of files and metadata
#' @export
#'
#' @examples
#' \dontrun{
#' list_files()
#' }
list_files <- function(path = "",
                       token = NULL,
                       verbose = TRUE) {
  if (is.null(token)) {
    token <- get_token()
    if (is.null(token)) cli::cli_abort("You need to `login()` first.")
  }

  base_path <- file.path("/remote.php/dav/files", token$loginName)

  resp <- build_request(token, base_path, path) |>
    httr2::req_method("PROPFIND") |>
    httr2::req_perform()

  responses <- resp |>
    httr2::resp_body_xml() |>
    xml2::xml_find_all(".//d:response")

  purrr::map(responses, function(r) {
    tibble::tibble(
      path     = xml2::xml_text(xml2::xml_find_first(r, ".//d:href")) |>
        gsub(base_path, "", x = _, fixed = TRUE),
      modified = xml2::xml_text(xml2::xml_find_first(r, ".//d:getlastmodified")),
      size     = xml2::xml_text(xml2::xml_find_first(r, ".//d:getcontentlength")),
      type     = xml2::xml_text(xml2::xml_find_first(r, ".//d:getcontenttype"))
    )
  }) |>
    purrr::list_rbind()
}


#' Download remote files
#'
#' @inheritParams list_files
#' @param destination local file path to download to.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' list_files()
#' }
download_files <- function(path = "",
                           destination = basename(path),
                           token = NULL,
                           verbose = TRUE) {

  if (is.null(token)) {
    token <- get_token()
    if (is.null(token)) cli::cli_abort("You need to `login()` first.")
  }

  base_path <- file.path("/remote.php/dav/files", token$loginName)

  resp <- build_request(token, base_path, path) |>
    httr2::req_method("GET") |>
    httr2::req_perform(path = destination)

  invisible(destination)
}


#' Upload local files
#'
#' @inheritParams list_files
#' @param source local file to upload.
#' @param overwrite do you want to overwrite existing files (`TRUE`) or skip
#'   them (`FALSE`)?
#'
#' @export
#'
#' @examples
#' \dontrun{
#' list_files()
#' }
upload_files <- function(source,
                         path,
                         overwrite = TRUE,
                         token = NULL,
                         verbose = TRUE) {

  if (is.null(token)) {
    token <- get_token()
    if (is.null(token))
      cli::cli_abort("You need to {.help [{.fun login}](nextcloudr::login)} first.")
  }
  if (length(source) != length(path)) {
    cli::cli_abort("source and path must have the same length")
  }

  base_path <- file.path("/remote.php/dav/files", token$loginName)

  purrr::walk2(source, path, function(s, p) {
    if (!overwrite) {
      if (!methods::is(try(list_files(p), silent = TRUE), "try-error")) {
        cli::cli_alert_info("File {p} already present.")
        return()
      }
    }
    build_request(token, base_path, path = p) |>
      httr2::req_method("PUT") |>
      httr2::req_body_file(s) |>
      httr2::req_perform()
  }, .progress = pb_config("upload", verbose))
  cli::cli_progress_done()

  invisible(path)
}

