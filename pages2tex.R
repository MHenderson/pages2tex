library(dplyr)

source("R/build_functions.R")
source("R/pagesbook.R")

source_folder(year = 2024, month = 1) |>
  pages_df() |>
  create_chapter_df() |>
  summarise_chapter_df() |>
  write_latex()
