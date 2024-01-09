source("R/build_functions.R")
source("R/pagesbook.R")

today <- Sys.Date()
this_month <- lubridate::month(today)
this_year <- lubridate::year(today)

source_folder(year = this_year, month = this_month) |>
  pages_df() |>
  create_chapter_df() |>
  summarise_chapter_df() |>
  write_latex(output_location = Sys.getenv("OUTPATH"))
