split_path <- function(path) {
  components <- unlist(strsplit(path, split = "/", fixed = TRUE))[components != ""]
  purrr::map_chr(seq_along(components), function(i) {
    paste("/", paste(components[1:i], collapse = "/"), sep = "")
  })
}
