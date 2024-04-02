write_latex <- function(chapters, output_location = '.') {
  purrr::walk(dirname(file.path(output_location, chapters$path)), dir.create, recursive = TRUE, showWarnings = FALSE)
  purrr::walk2(.x = chapters$tex_chapter, .y = file.path(output_location, chapters$path), .f = readr::write_file)
}
