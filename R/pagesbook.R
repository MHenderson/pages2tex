read_pages <- function(path, years = c(2021)) {
  tibble::tibble(
    paths = list.files(
      path = path,
      recursive = TRUE,
      pattern = "[0123456789]{2}_[0123456789]{2}_[0123456789]{2}.md$",
      full.names = TRUE
    )
  ) %>%
  dplyr::mutate(
    text = purrr::map_chr(paths, readr::read_file)
  ) %>%
  dplyr::mutate(
    ymd = lubridate::ymd(gsub(".md", "", basename(paths)))
  ) %>%
  dplyr::filter(lubridate::year(ymd) %in% years)
}

augment_time_label <- function(X) {
  X %>%
    dplyr::mutate(
      ts_s = stringr::str_match(text, "% (.*?) (GMT|BST)")[, 2],
      ts = lubridate::parse_date_time(ts_s, orders = c("a d b H:M:S", "a b d H:M:S"))
    ) %>%
    dplyr::mutate(
      time_label = format(ts, format = "%I:%M %p")
    ) %>%
    dplyr::mutate(
      text = stringr::str_replace(text, "% [A-Za-z]+\\s+\\d+ [A-Za-z]+ \\d{2}:\\d{2}:\\d{2} (GMT|BST) \\d+\n\n", ""),
      text = stringr::str_replace(text, "% [A-Za-z]+\\s+[A-Za-z]+\\s+\\d+ \\d{2}:\\d{2}:\\d{2} (GMT|BST) \\d+\n\n", ""),
    )
}

repair_latex <- function(X) {
  X %>%
    dplyr::mutate(
      tex = stringr::str_replace_all(text, "\\&", "\\\\&"),
      tex = stringr::str_replace_all(tex, "\\_", "\\\\_"),
      tex = stringr::str_replace_all(tex, "\\#", ""),
      # we really only should replace lone dollars, not those that denote equations
      tex = stringr::str_replace_all(tex, "\\$", "\\\\textdollar")
    )
}

add_heading <- function(X) {
  X %>%
   dplyr::mutate(
      heading = paste(
        lubridate::wday(ymd, label = TRUE, abbr = FALSE),
        lubridate::day(ymd),
        lubridate::month(ymd, label = TRUE, abbr = FALSE),
        lubridate::year(ymd)
      )
    ) %>%
    dplyr::mutate(
      tex_w_heading = paste0("\\section{", heading, "}\n\n", "\\hspace*{\\fill}", time_label, "\\vspace{5mm}\n\n", tex)
    )
}

create_chapters <- function(X) {
  X %>%
    dplyr::mutate(month = lubridate::month(ymd)) %>%
    dplyr::group_by(month) %>%
    dplyr::summarise(
      tex_chapter = paste0(tex_w_heading, collapse = "\n")
    ) %>%
    dplyr::mutate(
      tex_chapter = paste0("\\chapter{", lubridate::month(month, label = TRUE, abbr = FALSE), "}", tex_chapter)
    )
}

# replace character in s at position i by x
replace_at_i <- function(s, i, x = "\n") {
  paste(
    stringr::str_sub(s, 1, i - 1),
    stringr::str_sub(s, i + 1, stringr::str_length(s)),
    sep = x
  )
}

# find the index of the nearest space character
# to the left of a given index
locate_last_space_before <- function(s, x) {
  X <- stringr::str_locate_all(s, " ")[[1]]
  X_end <- X[, "end"]
  max(X_end[X_end <= x])
}

# a function that takes a string and returns the same string
# but with some space characters replaced by newlines
# to give the effect of cropping lines at the given number
# of columns
#
# called gqg because gqg in Vim does this
gqg_paragraph <- function(s, textwidth) {
  # we want to put a space every line length characters
  provisional_indices <- seq(textwidth, stringr::str_length(s), textwidth)

  # but if some of those positions are occupied by non-space
  # characters then we have to change plans slightly
  indices <- purrr::map_dbl(provisional_indices, locate_last_space_before, s = s)

  # do the actual changes here
  for(i in indices) {
    s <- replace_at_i(s, i)
  }

  return(s)
}

# this just applies gqg_paragraph to a string containing
# multiple paragraphs
gqg <- function(x, textwidth) {

  paragraphs <- stringr::str_split(x, pattern = "(\n){2,}")[[1]]

  # remove empty paragraphs
  paragraphs <- paragraphs[!(paragraphs == "")]

  too_wide <- function(x) {
    stringr::str_length(x) > textwidth
  }

  # apply gqg_paragraph to those paragraphs wider than textwidth
  fixed_paras <- purrr::map_if(paragraphs, too_wide, gqg_paragraph, textwidth)

  fixed_paras <- as.character(fixed_paras)

  return(paste0(fixed_paras, collapse = "\n\n"))

}

left_align <- function(X, textwidth = 72) {
  X %>%
    mutate(
      tex = purrr::map_chr(tex, gqg, textwidth = textwidth)
    )
}
