create_chapter_df <- function(pages) {
  pages |>
    llinyn::augment_time_label() |>
    llinyn::repair_latex() |>
    llinyn::left_align() |>
    llinyn::add_heading()
}