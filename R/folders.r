#' Create folders
#'
#' @inheritParams list_files
#'
#' @export
#'
#' @examples
#' \dontrun{
#' list_files()
#' }
create_folder <- function(path, token = NULL) {

  if (is.null(token)) {
    token <- get_token()
    if (is.null(token)) cli::cli_abort("You need to `login()` first.")
  }

  base_path <- file.path("/remote.php/dav/files", token$loginName)

  build_request(token, base_path, path) |>
    httr2::req_method("MKCOL") |>
    httr2::req_perform()

  cli::cli_alert_success("Folder {path} created!")
}
