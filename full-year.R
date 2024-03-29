# source folder needs to modified slightly
X <- file.path(Sys.getenv("PAGESPATH"), "src", 22) 

# the only change is recursive = TRUE
# does that work in the monthly case as well?
X <- tibble::tibble(
    paths = list.files(X, full.names = TRUE, recursive = TRUE)
  ) |>
  dplyr::mutate(
    ymd = lubridate::ymd(gsub(".md", "", basename(paths)))
  ) |>
  dplyr::mutate(
    text = purrr::map_chr(paths, readr::read_file)
  )

# this works (not sure where the NA month comes from)
XX <- X |>
  create_chapter_df() |>
  create_chapters() |>
  dplyr::filter(!is.na(month)) |>
  dplyr::mutate(
    path = file.path(paste0(month, ".tex"))
  )

write_latex(XX, output_location = "./chapters/")

