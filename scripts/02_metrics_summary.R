# Step 1: Load libraries
library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)

# Step 2: Load combined dataset
rtt <- read_csv("output/rtt_combined_all_categories.csv", show_col_types = FALSE) %>%
  clean_names()

# Step 3: Identify wait-band columns
wait_band_cols <- names(rtt)[str_detect(names(rtt), "gt_\\d+_to_\\d+_weeks_sum_1|gt_104_weeks_sum_1")]

# Extract starting week number from column name (e.g., "gt_18_to_19_weeks_sum_1" → 18)
starting_week <- str_extract(wait_band_cols, "\\d+") %>% as.integer()

# Columns for >=18 weeks and >=52 weeks
over_18_cols <- wait_band_cols[starting_week >= 18]
over_52_cols <- wait_band_cols[starting_week >= 52]

# Step 4: Calculate row-level metrics
rtt <- rtt %>%
  mutate(
    total_patients = select(., all_of(wait_band_cols)) %>% rowSums(na.rm = TRUE),
    over_18_weeks = select(., all_of(over_18_cols)) %>% rowSums(na.rm = TRUE),
    over_52_weeks = select(., all_of(over_52_cols)) %>% rowSums(na.rm = TRUE)
  )

# Step 5: Group and summarise monthly metrics
monthly_summary <- rtt %>%
  group_by(period, category) %>%
  summarise(
    total_patients = sum(total_patients, na.rm = TRUE),
    over_18_weeks = sum(over_18_weeks, na.rm = TRUE),
    over_52_weeks = sum(over_52_weeks, na.rm = TRUE)
  ) %>%
  mutate(
    percent_over_18 = round(100 * over_18_weeks / total_patients, 1),
    percent_over_52 = if_else(
      category == "Completed Pathways (Admitted)", NA_real_,
      round(100 * over_52_weeks / total_patients, 1)
    )
  ) %>%
  ungroup()

# Step 6: Save output
write_csv(monthly_summary, "output/monthly_summary.csv")

# Step 7: Preview result
print(monthly_summary, n = Inf)