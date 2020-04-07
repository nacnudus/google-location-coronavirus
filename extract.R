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

download_if_not_exists <- function(url, file_path) {
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

#' Convert a string of coordinates to a tibble
element_to_tibble <- function(element, transform_matrix) {
  element %>%
    xml_attr("d") %>%
    str_replace_all(" Z", "") %>%        # Remove close path commands
    str_replace_all("(?<=[0-9]) (?=[A-Z])", "\n") %>% # Separate into rows
    str_trim() %>%
    map_dfr(
      read_table2,
      col_names = c("command", "x", "y"),
      col_types = "cdd"
    ) %>%
    mutate(xy = map2(x, y, ~ matrix(c(.x, .y, 1), ncol = 1))) %>%
    mutate(xy = map(xy, ~ transform_matrix %*% .x)) %>% # transform
    hoist(xy, x = 1, y = 2) %>%
    select(-xy)
}

#' Extract a transformation matrix from a line element
extract_transform_matrix <- function(line_element) {
  matrix_row_3 <- matrix(c(0, 0, 1), ncol = 3) # 3rd row of svg transform matrix
  line_element %>%
    xml_attr("transform") %>%
    str_sub(8, -2) %>%
    str_split(",") %>%
    map(as.numeric) %>%
    map(matrix, ncol = 3) %>%
    map(rbind, matrix_row_3)
}

#' Extract data points from line element
parse_element <- function(element) {
  line_transform_matrix <- extract_transform_matrix(element)
  map2(
      element,
      line_transform_matrix,
      element_to_tibble
    ) %>%
    bind_rows()
}

#' Extract geometry from svg file
extract_geometry <- function(svg_path) {
  cat("Extracting geometry: ", svg_path, "\n")
  svg <-
    svg_path %>%
    read_xml() %>%
    xml_ns_strip()
  # Parse the trend line
  trend_element <-
    svg %>%
    xml_find_all("//path[@style='fill:none;stroke-width:2;stroke-linecap:butt;stroke-linejoin:miter;stroke:rgb(25.878906%,52.159119%,95.689392%);stroke-opacity:1;stroke-miterlimit:4;']")
  if (length(trend_element) == 0) return(tibble()) # no graph
  trend <- parse_element(trend_element)
  # Parse the baseline
  baseline_element <-
    svg %>%
    xml_find_all("//path[@style='fill:none;stroke-width:0.5;stroke-linecap:butt;stroke-linejoin:miter;stroke:rgb(85.488892%,86.268616%,87.838745%);stroke-opacity:1;stroke-miterlimit:4;']")
  # Keep the middle line of each graph.  There are 5 lines per graph
  lines_per_graph <- 5
  middle_line_index <-
    seq(
      lines_per_graph,
      length(baseline_element),
      by = lines_per_graph
    ) - 2
  baseline_element_80 <- baseline_element[middle_line_index - 2] # 80% line
  baseline_80 <-
    parse_element(baseline_element_80) %>%
    filter(command == "L") %>%     # Keep only the right-end point of each line
    pull(y)
  baseline_element_0 <- baseline_element[middle_line_index]
  baseline <-
    parse_element(baseline_element_0) %>%
    filter(command == "L") %>%     # Keep only the right-end point of each line
    rename(baseline_x = x, baseline = y) %>%
    select(-command)
  baseline$baseline_80 <- baseline_80
  # Assign trend lines to baselines
  unique_baselines <- sort(unique(round(baseline$baseline, -2)))
  # Midpoints between baselines https://stackoverflow.com/a/54147509/937932
  midpoints <-
    unique_baselines[-length(unique_baselines)] +
    diff(unique_baselines) / 2
  y_cutpoints <- sort(c(0, Inf, midpoints))
  x_cutpoints <- sort(c(0, unique(baseline$baseline_x) + 1)) # rounding margin
  trend <-
    trend %>%
    mutate(row = findInterval(y, y_cutpoints),
           col = findInterval(x, x_cutpoints))
  baseline <-
    baseline %>%
    mutate(row = findInterval(baseline, y_cutpoints),
           col = findInterval(baseline_x, x_cutpoints))
  inner_join(baseline, trend, by = c("col", "row"))
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
    return(tibble(row = 1:3, region_name = NA_character_))
  }
  text %>%
    filter(y %in% c(36, 363), height == 20) %>%
    arrange(y, x) %>%
    group_by(y) %>%
    summarise(region_name = paste(text, collapse = " ")) %>%
    ungroup() %>%
    # Duplicate to two rows per place
    mutate(row = as.integer(factor(y)) * 2L) %>%
    select(row, region_name) %>%
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
extract_baseline_comparisons <- function(type, text) {
  text %>%
    filter(
      switch(type,
        country = height == 45,
        region = y %in% c(104, 242, 431, 568) & height == 13
      )
    ) %>%
    mutate(baseline_comparison = parse_number(text) / 100) %>%
    mutate(row = as.integer(factor(y)),
           col = as.integer(factor(x))) %>%
    select(row, col, baseline_comparison)
}

#' Join the separate panels of region names, categories and baselines
join_panels <- function(region_name, category, baseline_comparison) {
  region_name %>%
    inner_join(category, by = "row") %>%
    inner_join(baseline_comparison, by = c("row", "col"))
}

#' Scale y-values by the baseline
scale_y <- function(y, baseline, baseline_80) {
  baseline <- baseline[1]
  baseline_80 <- baseline_80[1]
  diff_from_baseline <- y - baseline
  scale <- (baseline_80 - baseline) / -0.8
  diff_from_baseline / scale
}

#' Convert x-values to dates
x_to_date <- function(x, baseline_x, report_date) {
  report_date - days(round((baseline_x - x) / day_width))
}

# Main script ------------------------------------------------------------------

# The Google page to download each pdf
home_url <- "https://www.google.com/covid19/mobility/"

# A folder to store the PDF files
dir_create("pdf")

# A data frame to collect the data, beginning with the URL of each pdf
df <- extract_urls(home_url)

# Download the pdf files
walk2(df$url, df$file_path, download_if_not_exists)

# Convert to svg and extract the graphs
df_trends <-
  df %>%
  mutate(svg_path = map(file_path, pdf_to_svg)) %>% # convert pdf pages to svg
  unnest(svg_path) %>%
  mutate(page = as.integer(str_extract(svg_path, "[0-9]+(?=\\.pdf$)"))) %>%
  mutate(type = if_else(page <= 2, "country", "region")) %>%
  mutate(geometry = map(svg_path, extract_geometry)) %>% # extract geometry from svg
  unnest(geometry) %>%
  group_by(url, page, row, col) %>%
  mutate(group = cumsum(command == "M")) %>% # group sections of strokes
  ungroup() %>%
  select(-file_path, -svg_path, -command, -type)

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
  mutate(
    region_name = list(extract_region_names(type, text)),
    category = list(extract_categories(type, page, text)),
    baseline_comparison = list(extract_baseline_comparisons(type, text)),
    panel = list(join_panels(region_name, category, baseline_comparison))
  ) %>%
  ungroup() %>%
  select(url, page, country_code, country_name, report_date, type, panel) %>%
  unnest(panel) %>%
  select(url, page, country_code, country_name, report_date, type, row, col,
         region_name, category, baseline_comparison)

# Guess the x-width of one day between data points
day_width <- extract_day_width(df_trends)

# Pair up the text with the trends
final <-
  inner_join(df_text, df_trends, by = c("url", "page", "row", "col")) %>%
  group_by(url, page, row, col) %>%
  arrange(url, page, row, col, x) %>%
  mutate(
    trend = scale_y(y, baseline, baseline_80),
    date = x_to_date(x, baseline_x, report_date)
  ) %>%
  ungroup() %>%
  select(-x, -y, -baseline, -baseline_80, -baseline_x)

write_tsv(final, paste0(max(final$report_date), ".tsv"))
