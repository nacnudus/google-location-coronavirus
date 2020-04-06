library(tidyverse)
library(xml2)
library(fs)
library(sys)
library(rvest)
library(pdftools)
library(lubridate)

# Utility functions ------------------------------------------------------------

#' Extract URLs from home page
extract_urls <- function(url) {
  url %>%
    read_html() %>%
    html_nodes(".country-data a") %>%
    html_attr("href") %>%
    tibble(url = .) %>%
    mutate(file_path = path("pdf", basename(url)))
}

download_if_not_exists <- function(file_path) {
  if (file_exists(file_path)) return()
  download.file(url, file_path, mode = "wb")
}

#' Convert pdf pages to svg files and return their paths
pdf_to_svg <- function(file_path) {
  cat("Converting to svg: ", file_path, "\n")
  # Create a temporary directory named after the file
  svg_dir <- path_temp(path_ext_remove(path_file(file_path)))
  dir_create(svg_dir)
  command <- c(
    "pdf2svg",
    file_path,
    path(svg_dir, "%d.pdf"),
    "all"
  )
  exec_wait(command)
  dir_ls(svg_dir)
}

#' Extract geometry from svg file
extract_geometry <- function(svg_path) {
  cat("Extracting geometry: ", svg_path, "\n")
  element <-
    svg_path %>%
    read_xml() %>%
    xml_ns_strip() %>%
    xml_find_all("//path[@style='fill:none;stroke-width:2;stroke-linecap:butt;stroke-linejoin:miter;stroke:rgb(25.878906%,52.159119%,95.689392%);stroke-opacity:1;stroke-miterlimit:4;']")
  if (length(element) == 0) return(tibble()) # no graph
  geometry <- xml_attr(element, "d")   # the data points
  panel <-                             # the row and column of the graph
    element %>%
    xml_attr("transform") %>%
    str_sub(8, -2) %>%
    str_split(",") %>%
    map(~ .x[5:6]) %>%                 # x,y of the corner of the graph
    transpose() %>%
    map(unlist) %>%
    map(as.numeric) %>%
    set_names(c("col", "row"))
  # Renumber the row/col from being an x,y coordinate to being the position of
  # the graph on the page, either 3 rows of 1 column, or 4 rows of 3 columns.
  panel$row <- as.integer(as_factor(panel$row))
  panel$col <- as.integer(as_factor(panel$col))
  tibble(geometry, row = panel$row, col = panel$col)
}

#' Convert a string of coordinates to a tibble
parse_geometry <- function(geometry) {
  geometry %>%
    str_replace_all(" Z", "") %>%        # Remove close path commands
    str_replace_all("(?<=[0-9]) (?=[A-Z])", "\n") %>% # Separate into rows
    str_trim() %>%
    map_dfr(read_table2, col_names = c("command", "x", "y"), col_types = "cdd")
}

#' Extract the width of one day on the x-axis from the strokes
extract_day_width <- function(.data) {
  # Width between consecutive points (assume the most common width)
  .data %>%
    group_by(page, row, col) %>%
    arrange(x) %>%
    mutate(diff = x - lag(x)) %>%
    ungroup() %>%
    count(diff, sort = TRUE) %>%
    filter(diff > 0) %>%
    top_n(1, n) %>%
    pull(diff)
}

#' Extract the text from the pdf
extract_text <- function(file_path) {
  cat("Extracting text: ", file_path, "\n")
  file_path %>%
    pdf_data() %>%
    bind_rows(.id = "page") %>%
    mutate(page = as.integer(page))
}

#' Extract the title from the text
extract_title <- function(text) {
  # Read the country name and date
  text %>%
    filter(page == 1, y == 75) %>%
    pull(text)
}

#' Extract the country name from the title
extract_country_name <- function(title) {
  paste(head(title, -3), collapse = " ")
}

#' Extract the report date from the title
extract_report_date <- function(title) {
  title %>%
    tail(3) %>%
    paste(collapse = " ") %>%
    parse_date(format = "%B %d, %Y")
}

#' Extract the country-level text
extract_country_text <- function(text) {
  filter(text, page <= 2)
}

#' Extract the region-level text
extract_region_text <- function(text) {
  filter(text, page >= 3, page < max(page))
}

#' Extract the region names and row/col from the text
extract_region_names <- function(type, text) {
  if (type != "region") {
    return(tibble(row = 1:3, col = 1, region_name = NA_character_))
  }
  text %>%
    filter(y %in% c(36, 363), height == 20) %>%
    arrange(y, x) %>%
    group_by(y, x) %>%
    summarise(region_name = paste(text, collapse = " ")) %>%
    ungroup() %>%
    # Duplicate to two rows per place
    mutate(row = as.integer(factor(y)) * 2L,
           col = as.integer(factor(x))) %>%
    select(row, col, region_name) %>%
    bind_rows(mutate(., row = row - 1L)) %>%
    arrange(row)
}

#' Extract the categories and row/col from the text
extract_categories <- function(type, page, text) {
  text %>%
    filter(
      switch(type,
        country = !(page == 1 & y < 342) & (height == 13),
        region = (y %in% c(82, 220, 409, 547)) & (height == 11)
      )
    ) %>%
    arrange(y, x) %>%
    mutate(x = plyr::round_any(x, 100, f = floor)) %>%
    group_by(y, x) %>%
    summarise(category = paste(text, collapse = " ")) %>%
    ungroup() %>%
    mutate(row = as.integer(factor(y)),
           col = as.integer(factor(x))) %>%
    select(row, col, category)
}

#' Extract the baselines and row/col from the text
extract_baselines <- function(type, text) {
  text %>%
    filter(
      switch(type,
        country = height == 45,
        region = y %in% c(104, 242, 431, 568) & height == 13
      )
    ) %>%
    mutate(baseline = parse_number(text) / 100) %>%
    mutate(row = as.integer(factor(y)),
           col = as.integer(factor(x))) %>%
    select(row, col, baseline)
}

#' Join the separate panels of region names, categories and baselines
join_panels <- function(region_name, category, baseline) {
  region_name %>%
    inner_join(category, by = c("row", "col")) %>%
    inner_join(baseline, by = c("row", "col"))
}

#' Scale y-values by the baseline
scale_y <- function(y, baseline) {
  diff_from_first <- y - first(y)
  scale <- last(diff_from_first) / baseline
  diff_from_first / scale
}

#' Convert x-values to dates
x_to_date <- function(x, report_date) {
  day_diff <- round((x - lag(x)) / day_width)
  day_diff <- replace_na(day_diff, 1)
  report_date - days(rev(cumsum(day_diff)) - 1)
}

# Main script ------------------------------------------------------------------

# The Google page to download each pdf
home_url <- "https://www.google.com/covid19/mobility/"

# A folder to store the PDF files
dir_create("pdf")

# A data frame to collect the data, beginning with the URL of each pdf
df <- extract_urls(home_url)

# Download the pdf files
walk(df$file_path, download_if_not_exists)

# Convert to svg and extract the graphs
df_trends <-
  df %>%
  mutate(svg_path = map(file_path, pdf_to_svg)) %>% # convert pdf pages to svg
  unnest(svg_path) %>%
  group_by(url) %>%
  mutate(page = row_number()) %>%
  ungroup() %>%
  mutate(type = if_else(page <= 2, "country", "region")) %>%
  mutate(geometry = map(svg_path, extract_geometry))  %>% # extract geometry from svg
  unnest(geometry) %>%
  group_by(url, page) %>%
  # mutate(stroke = row_number()) %>%
  mutate(geometry = map(geometry, parse_geometry)) %>% # parse geometry to tibble
  select(-type) %>%
  unnest(geometry) %>%
  mutate(y = -y) %>%                   # Flip y coords to be positive at the top
  group_by(url, page, row, col) %>%
  mutate(group = cumsum(command == "M")) %>% # group sections of strokes
  ungroup() %>%
  select(-file_path, -svg_path, -command)

# Extract the text from the pdf
df_text <-
  df %>%
  mutate(country_code = str_extract(url, "(?<=_)[A-Z]{2}(?=_)")) %>%
  mutate(text = map(file_path, extract_text)) %>%
  mutate(title = map(text, extract_title),
         country_name = map_chr(title, extract_country_name),
         report_date = do.call(c, map(title, extract_report_date))) %>%
  select(-title) %>%
  # Split the text into pages
  mutate(text = map(text, nest_by, page, .key = "text")) %>%
  unnest(text) %>%
  mutate(type = if_else(page <= 2, "country", "region")) %>%
  group_by(url) %>%
  filter(page != max(page)) %>%        # Drop the final page, which is notes
  ungroup() %>%
  # Extract the panel region names, categories and baselines
  rowwise() %>%
  mutate(region_name = list(extract_region_names(type, text))) %>%
  mutate(category = list(extract_categories(type, page, text))) %>%
  mutate(baseline = list(extract_baselines(type, text))) %>%
  mutate(panel = list(join_panels(region_name, category, baseline))) %>%
  ungroup() %>%
  select(url, page, country_code, country_name, report_date, type, panel) %>%
  unnest(panel)

# Guess the x-width of one day between data points
day_width <- extract_day_width(df_trends)

# Pair up the text with the trends
final <-
  inner_join(df_text, df_trends, by = c("url", "page", "row", "col")) %>%
  group_by(url, page, row, col) %>%
  arrange(url, page, row, col, x) %>%
  mutate(
    trend = scale_y(y, baseline),
    date = x_to_date(x, report_date)
  ) %>%
  ungroup() %>%
  select(-x, -y)

write_tsv(final, paste0(max(final$report_date), ".tsv"))
