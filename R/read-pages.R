read_pages <- function(path, years = c(2021)) {
  tibble::tibble(
    paths = list.files(
      path = path,
      recursive = TRUE,
      pattern = "[0123456789]{2}_[0123456789]{2}_[0123456789]{2}.md$",
      full.names = TRUE
    )
  ) |>
    dplyr::mutate(
      text = purrr::map_chr(paths, readr::read_file)
    ) |>
    dplyr::mutate(
      ymd = lubridate::ymd(gsub(".md", "", basename(paths)))
    ) |>
    dplyr::filter(lubridate::year(ymd) %in% years)
}