library(lubridate)

source("R/create-chapter-df.R")
source("R/pages-df.R")
source("R/source-folder.R")
source("R/summarise-chapter-df.R")
source("R/write-latex.R")

today <- Sys.Date()
this_month <- month(today)
this_year <- year(today)

source_folder(year = this_year, month = this_month) |>
  pages_df() |>
  create_chapter_df() |>
  summarise_chapter_df() |>
  write_latex(output_location = Sys.getenv("OUTPATH"))
