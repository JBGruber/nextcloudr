#' Login to a server
#'
#' @param server the URL to a 'Nextcloud' server.
#' @param user optional. The user name (only relevant if multiple user tokens
#'   are cached).
#' @param use_cached `FALSE` forces the creation of a new token.
#'
#' @return token (invisible)
#' @export
#'
#' @examples
#' \dontrun{
#' login("https://cloud.example.com")
#' }
login <- function(server, user = NULL, use_cached = TRUE) {

  # see if token is cached
  cache_path <- tools::R_user_dir("nextcloudr", "cache")
  token <- character()
  if (use_cached)
    token <- check_tokens(cache_path, server, user)
  if (identical(length(token), 0L)) {
    token <- auth_routine(server)
    choice <- utils::askYesNo(
      msg = "Do you want cache the token on disk?",
      default = FALSE
    )

    if (choice) {
      dir.create(cache_path, showWarnings = FALSE, recursive = TRUE)
      tok_path <- file.path(
        cache_path,
        paste0(basename(server), "_", token$loginName)
      )
      httr2::secret_write_rds(
        x = token, path = tok_path,
        key = I(rlang::hash("supergeheim"))
      )
      cli::cli_alert_success("Token cached in {cache_path}")
    }

  }
  rlang::env_poke(env = the, nm = "nextcloud_token", value = token, create = TRUE)

  # store in cache
  cli::cli_process_done(msg_done = "Auth complete")
  invisible(token)
}


check_tokens <- function(cache_path, server, user) {
  # check env
  cached_tokens <- get_token()
  if (!is.null(cached_tokens)) {
    return(cached_tokens)
  }

  # check disk
  cached_tokens <- list.files(cache_path, basename(server))
  if (length(cached_tokens) > 1) {
    users <- regmatches(cached_tokens, regexpr("(?<=_).+$", cached_tokens, perl = TRUE))
    if (!is.null(user)) {
      cached_tokens <- cached_tokens[users == user]
    } else {
      sel <- utils::menu(users, "Who are you?")
      cached_tokens <- cached_tokens[sel]
    }
  }

  if (length(cached_tokens) > 0) {
    cached_tokens <- httr2::secret_read_rds(file.path(cache_path, cached_tokens),
                                            I(rlang::hash("supergeheim")))
  }
  return(cached_tokens)
}


get_token <- function() {
  if (rlang::env_has(the, nms = "nextcloud_token")) {
    return(rlang::env_get(the, nm = "nextcloud_token"))
  }
}


auth_routine <- function(server) {
  challange <- httr2::request(server) |>
    httr2::req_url_path("index.php/login/v2") |>
    httr2::req_user_agent("nextcloudr") |>
    httr2::req_method("POST") |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  cli::cli_progress_step("Waiting for authentication", spinner = TRUE)
  utils::browseURL(challange$login)

  resp <- httr2::response(status_code = 404L)
  while (httr2::resp_status(resp) != 200L) {
    resp <- httr2::request(challange$poll$endpoint) |>
      httr2::req_method("POST") |>
      httr2::req_body_form(token = challange$poll$token) |>
      httr2::req_error(is_error = function(resp) httr2::resp_status(resp) >= 500L) |>
      httr2::req_perform()
    cli::cli_progress_update()
    Sys.sleep(3/100)
  }
  return(httr2::resp_body_json(resp))
}


