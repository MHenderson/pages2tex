source("R/create-chapter-df.R")
source("R/create-chapters.R")
source("R/write-latex.R")

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

# this is slightly different from the update script - which uses the OUTPATH environment variable (usually set to pagesbook-2024 folder)
write_latex(XX, output_location = file.path("/Users/matthew/001_Project/Writing/Memoir/pagesbook-2022/src/chapter/"))

