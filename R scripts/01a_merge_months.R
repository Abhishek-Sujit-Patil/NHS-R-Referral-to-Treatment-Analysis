
# Merges all CSVs in data/raw/ into one big file
# -----------------------------------------------

library(tidyverse)
library(readr)
library(janitor)

# Step 1: List all CSVs in data/raw
file_list <- list.files("data/raw/", pattern = "*.csv", full.names = TRUE)

# Step 2: Read and combine
rtt_all <- file_list %>%
  map_dfr(read_csv, show_col_types = FALSE) %>%
  clean_names()

# Step 3: Save cleaned full file
dir.create("data/processed", showWarnings = FALSE)
write_csv(rtt_all, "data/processed/rtt_all_months.csv")

# Preview
glimpse(rtt_all)