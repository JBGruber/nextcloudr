#' Create folders
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
#' list_files()
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
