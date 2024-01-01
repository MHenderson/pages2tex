source_folder <- function(year = 2022, month = 9) {
  two_digit_year <- substr(year, 3, 4)
  two_digit_month <- sprintf("%02d", month)
  file.path(Sys.getenv("PAGESPATH"), "src", two_digit_year, two_digit_month)
}

pages_df <- function(X) {
  tibble::tibble(
    paths = list.files(X, full.names = TRUE)
  ) %>%
  dplyr::mutate(
    ymd = lubridate::ymd(gsub(".md", "", basename(paths)))
  ) %>%
  dplyr::mutate(
    text = purrr::map_chr(paths, readr::read_file)
  )
}

create_chapter_df <- function(pages) {
  pages %>%
    augment_time_label() %>%
    repair_latex() %>%
    left_align() %>%
    add_heading()
}

summarise_chapter_df <- function(X) {

  month <- lubridate::month(X$ymd[1])
  month_name <- lubridate::month(X$ymd[1], label = TRUE, abbr = FALSE) %>% as.character()

  X %>%
    dplyr::summarise(
      tex_chapter = paste0(tex_w_heading, collapse = "\n")
    ) %>%
    dplyr::mutate(
      tex_chapter = paste0("\\chapter{", month_name, "}", tex_chapter)
    ) %>%
    mutate(
      path = here::here("src", "chapter", paste0(month, ".tex"))
    )
}

write_latex <- function(chapters_) {
  purrr::walk2(.x = chapters_$tex_chapter, .y = chapters_$path, .f = readr::write_file)
}
