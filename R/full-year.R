library(pages2df)

full_year <- function(pagespath, output_location) {

  # the only change is recursive = TRUE
  # does that work in the monthly case as well?
  X <- tibble::tibble(
      paths = list.files(pagespath, full.names = TRUE, recursive = TRUE)
    ) |>
    dplyr::mutate(
      ymd = lubridate::ymd(gsub(".md", "", basename(paths)))
    ) |>
    dplyr::mutate(
      text = purrr::map_chr(paths, readr::read_file)
    )
  
  # this works (not sure where the NA month comes from)
  chapters <- X |>
    create_chapter_df() |>
    create_chapters() |>
    dplyr::filter(!is.na(month)) |>
    dplyr::mutate(
      path = file.path(paste0(month, ".tex"))
    )
  
  purrr::walk(dirname(file.path(output_location, chapters$path)), dir.create, recursive = TRUE, showWarnings = FALSE)
  purrr::walk2(.x = chapters$tex_chapter, .y = file.path(output_location, chapters$path), .f = readr::write_file)

}

# Full year export invocation
#
# output location is slightly different from pages2tex.R - which uses the OUTPATH environment variable (usually set to pagesbook-2024 folder)
# full_year(
#        pagespath = file.path(Sys.getenv("PAGESPATH"), "src", 22), 
#  output_location = file.path("/Users/matthew/001_Project/Writing/Memoir/pagesbook-2022/src/chapter/")
# )
