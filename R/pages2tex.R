library(pages2df)

pages2tex <- function(date, output_location) {

  two_digit_year <- substr(lubridate::year(date), 3, 4)
  two_digit_month <- sprintf("%02d", lubridate::month(date))
  
  chapters <- file.path(Sys.getenv("PAGESPATH"), "src", two_digit_year, two_digit_month) |>
    pages_df() |>
    create_chapter_df() |>
    summarise_chapter_df()
  
  purrr::walk(dirname(file.path(output_location, chapters$path)), dir.create, recursive = TRUE, showWarnings = FALSE)
  
  purrr::walk2(.x = chapters$tex_chapter, .y = file.path(output_location, chapters$path), .f = readr::write_file)

}

# Daily update invocation
#
# pages2tex(Sys.Date(), output_location = Sys.getenv("OUTPATH"))
