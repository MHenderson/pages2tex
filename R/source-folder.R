source_folder <- function(year = 2022, month = 9) {
  two_digit_year <- substr(year, 3, 4)
  two_digit_month <- sprintf("%02d", month)
  file.path(Sys.getenv("PAGESPATH"), "src", two_digit_year, two_digit_month)
}