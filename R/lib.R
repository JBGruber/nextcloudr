build_request <- function(token, base_path, path, ignore_error = NULL) {
  httr2::request(token$server) |>
    httr2::req_url_path(base_path) |>
    httr2::req_url_path_append(path) |>
    httr2::req_auth_basic(
      username = token$loginName,
      password = token$appPassword
    ) |>
    httr2::req_error(
      is_error = function(resp)
        !httr2::resp_status(resp) %in% c(1:399, ignore_error),
      body = parse_error
    )
}


parse_error <- function(resp) {
  if (httr2::resp_content_type(resp) %in% c("application/xml", "text/xml")) {
    resp |>
      httr2::resp_body_xml() |>
      xml2::xml_find_first(".//s:message") |>
      xml2::xml_text()
  }
}
