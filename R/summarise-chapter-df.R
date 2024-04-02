summarise_chapter_df <- function(X) {
  
  month <- lubridate::month(X$ymd[1])
  month_name <- lubridate::month(X$ymd[1], label = TRUE, abbr = FALSE) |> as.character()
  
  X |>
    dplyr::summarise(
      tex_chapter = paste0(tex_w_heading, collapse = "\n")
    ) |>
    dplyr::mutate(
      tex_chapter = paste0("\\chapter{", month_name, "}", tex_chapter)
    ) |>
    dplyr::mutate(
      path = file.path(paste0(month, ".tex"))
    )
}