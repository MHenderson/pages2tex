source_folder <- function(year = 2022, month = 9) {
  two_digit_year <- substr(year, 3, 4)
  two_digit_month <- sprintf("%02d", month)
  file.path(Sys.getenv("PAGESPATH"), "src", two_digit_year, two_digit_month)
}

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

create_chapter_df <- function(pages) {
  pages |>
    llinyn::augment_time_label() |>
    llinyn::repair_latex() |>
    llinyn::left_align() |>
    llinyn::add_heading()
}

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

write_latex <- function(chapters, output_location = '.') {
  purrr::walk(dirname(file.path(output_location, chapters$path)), dir.create, recursive = TRUE, showWarnings = FALSE)
  purrr::walk2(.x = chapters$tex_chapter, .y = file.path(output_location, chapters$path), .f = readr::write_file)
}
