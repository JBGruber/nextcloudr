#' Create or delete folders
#'
#' @inheritParams list_files
#' @param recursive create folders recursively. Default is `FALSE` for
#'   performance.
#' @param error_on_exist throw an error when the folder already exists.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' create_folder("/test/test2", recursive = TRUE)
#' delete_folder("/test/test2")
#' }
create_folder <- function(path, recursive = FALSE, error_on_exist = FALSE, token = NULL) {

  if (is.null(token)) {
    token <- get_token()
    if (is.null(token)) cli::cli_abort("You need to `login()` first.")
  }

  base_path <- file.path("/remote.php/dav/files", token$loginName)

  if (recursive) {
    path <- split_path(path)
  }

  purrr::walk(path, function(p) {
    build_request(token, base_path, path = p,
                  ignore_error = ifelse(error_on_exist, 1, 405)) |>
      httr2::req_method("MKCOL") |>
      httr2::req_perform()
  })

  cli::cli_alert_success("Folder {path} created!")
}

#' @rdname create_folder
#' @export
delete_folder <- function(path, recursive = FALSE, token = NULL) {
  if (recursive) {
    path <- rev(split_path(path))
    path <- paste0(path, "/")
  }
  purrr::walk(path, function(p) {
    delete_files(path = p, token = token)
    # folder might still be blocked without wait
    Sys.sleep(1)
  })
  cli::cli_alert_success("Folder {path} deleted!")
}
