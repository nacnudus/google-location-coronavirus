# Collate the latest data per data point, because Google's reports are a
# sliding window, so some dates are reported every week for a few weeks.

library(tidyverse)
library(fs)
library(vroom)

all_country_data <-
  dir_ls(glob = "*-country.tsv") %>%
  map_dfr(vroom, delim = "\t", col_types = "cdccDcddccdddD")

latest_country_data <-
  all_country_data %>%
  group_by(country_code, region_name, category, date) %>%
  top_n(1, report_date) %>%
  ungroup()

write_tsv(latest_country_data, "country.tsv")

all_region_data <-
  dir_ls(glob = "*-region.tsv") %>%
  map_dfr(vroom, delim = "\t", col_types = "cdccDcddccdddD")

latest_region_data <-
  all_region_data %>%
  group_by(country_code, region_name, sub_region_name, category, date) %>%
  top_n(1, report_date) %>%
  ungroup()

write_tsv(latest_region_data, "region.tsv")
