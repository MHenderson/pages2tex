pages_df <- function(X) {
  tibble::tibble(
    paths = list.files(X, full.names = TRUE)
  ) |>
    dplyr::mutate(
      ymd = lubridate::ymd(gsub(".md", "", basename(paths)))
    ) |>
    dplyr::mutate(
      text = purrr::map_chr(paths, readr::read_file)
    )
}