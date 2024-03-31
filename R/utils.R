split_path <- function(path) {
  components <- unlist(strsplit(path, split = "/", fixed = TRUE))
  components <- components[components != ""]
  purrr::map_chr(seq_along(components), function(i) {
    paste("/", paste(components[1:i], collapse = "/"), sep = "")
  })
}


pb_config <- function(verb, verbose) {
  if (verbose) {
    return(list(
      clear = FALSE,
      format = paste0("{cli::pb_spin} ", verb, "ing files ",
                     "({cli::pb_percent} done, {cli::pb_current}/{cli::pb_total}) ",
                     "| ETA: {cli::pb_eta}"),
      format_done = paste0("{cli::pb_total} file{?s} ", verb,
                           "ed! [{cli::pb_elapsed}]")
    ))
  } else {
    return(FALSE)
  }
}
