create_chapter_df <- function(pages) {
  pages |>
    dplyr::mutate(
      time_label = llinyn::extract_time_label(text),
            text = llinyn::strip_time_headings(text)
    ) |>
    dplyr::mutate(
      tex = llinyn::repair_latex_string(text)
    ) |>
    dplyr::mutate(
      tex = purrr::map_chr(tex, llinyn::gqg, textwidth = 72)
    ) |>
    dplyr::mutate(
      tex_w_heading = paste0("\\section{", llinyn::ymd_text_format(ymd), "}\n\n", "\\hspace*{\\fill}", time_label, "\\vspace{5mm}\n\n", tex)
    )
}