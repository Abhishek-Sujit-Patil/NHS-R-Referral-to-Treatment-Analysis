library(tidyverse)
library(janitor)
library(readxl)

# Load Acute NHS Trust codes
ods <- read_excel("data/ODS_acute_nhs_trusts.xlsx") %>%
  clean_names()
acute_trust_codes <- ods$code

# Load full 11-month RTT dataset
rtt <- read_csv("data/processed/rtt_all_months.csv", show_col_types = FALSE) %>%
  clean_names()

# Filter only acute NHS trusts
rtt_filtered <- rtt %>%
  filter(provider_org_code %in% acute_trust_codes)

# Define weekly column ranges using consistent naming pattern
week_cols_all <- names(rtt_filtered)[str_detect(names(rtt_filtered), "^gt_.*")]
week_cols_18 <- names(rtt_filtered)[which(names(rtt_filtered) == "gt_18_to_19_weeks_sum_1"):which(names(rtt_filtered) == "gt_104_weeks_sum_1")]
week_cols_52 <- names(rtt_filtered)[which(names(rtt_filtered) == "gt_52_to_53_weeks_sum_1"):which(names(rtt_filtered) == "gt_104_weeks_sum_1")]

# Group by trust and calculate true totals
trust_summary <- rtt_filtered %>%
  group_by(provider_org_code, provider_org_name) %>%
  summarise(
    total_patients = sum(across(all_of(week_cols_all)), na.rm = TRUE),
    over_18_weeks = sum(across(all_of(week_cols_18)), na.rm = TRUE),
    over_52_weeks = sum(across(all_of(week_cols_52)), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    percent_over_18 = round(over_18_weeks / total_patients * 100, 2),
    percent_over_52 = round(over_52_weeks / total_patients * 100, 2)
  )

# Save and preview
write_csv(trust_summary, "output/clustering/trust_delay_summary_FINAL.csv")
print(trust_summary, n = 112)
