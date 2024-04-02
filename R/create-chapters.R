create_chapters <- function(X) {
  X |>
    dplyr::mutate(month = lubridate::month(ymd)) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      tex_chapter = paste0(tex_w_heading, collapse = "\n")
    ) |>
    dplyr::mutate(
      tex_chapter = paste0("\\chapter{", lubridate::month(month, label = TRUE, abbr = FALSE), "}", tex_chapter)
    )
}