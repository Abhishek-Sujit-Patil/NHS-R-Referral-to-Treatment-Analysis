
library(tidyverse)
library(janitor)
library(stringr)

# Step 1: Load Files
trust_clusters <- read_csv("output/clustering/trust_clusters.csv", show_col_types = FALSE)
rtt_all_months <- read_csv("data/processed/rtt_all_months.csv", show_col_types = FALSE) %>%
  clean_names()

# Step 2: Filter only Acute NHS Trusts
trust_codes <- trust_clusters$provider_org_code

rtt_filtered <- rtt_all_months %>%
  filter(provider_org_code %in% trust_codes)

# Step 3: Define Week Columns
week_cols_all <- names(rtt_filtered)[str_detect(names(rtt_filtered), "^gt_.*")]
week_cols_18  <- names(rtt_filtered)[str_detect(names(rtt_filtered), "^gt_1[89]_to_.*|^gt_[2-9][0-9]_to_.*|^gt_1[0-9]{2}_to_.*|^gt_104_weeks_sum_1$")]

# Step 4: Aggregate Final Trust-Level Metrics
trust_summary_typology <- rtt_filtered %>%
  group_by(provider_org_code, provider_org_name) %>%
  summarise(
    total_patients = sum(across(all_of(week_cols_all)), na.rm = TRUE),
    patients_over_18 = sum(across(all_of(week_cols_18)), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_over_18 = if_else(total_patients == 0, NA_real_, round(patients_over_18 / total_patients * 100, 1))
  )

# Step 5: Save Output
write_csv(trust_summary_typology, "output/trust_typology_base.csv")

# Step 6: Preview
print(trust_summary_typology, n = 20)