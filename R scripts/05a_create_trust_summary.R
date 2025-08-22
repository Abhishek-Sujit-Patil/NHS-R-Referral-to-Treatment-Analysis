# ---------------------------------------------------------------
# scripts/05a_create_trust_summary.R
# Recalculate breach percentages and summarise per NHS trust
# ---------------------------------------------------------------

library(dplyr)
library(readr)
library(janitor)

# Step 1: Load the full RTT dataset (11 months)
rtt_all <- read_csv("data/processed/rtt_all_months.csv", show_col_types = FALSE) %>%
  clean_names()

# Step 2: Recalculate % Over 18 and % Over 52 Weeks
rtt_all <- rtt_all %>%
  mutate(
    over_18_weeks = rowSums(select(., starts_with("gt_18_to_")), na.rm = TRUE),
    over_52_weeks = rowSums(select(., starts_with("gt_52_to_")), na.rm = TRUE) +
      coalesce(gt_104_weeks_sum_1, 0),
    
    percent_over_18 = if_else(total_all > 0, over_18_weeks / total_all * 100, NA_real_),
    percent_over_52 = if_else(total_all > 0, over_52_weeks / total_all * 100, NA_real_)
  )

# Step 3: Summarise metrics per provider trust
trust_summary <- rtt_all %>%
  group_by(provider_org_code, provider_org_name) %>%
  summarise(
    avg_total_patients = mean(total_all, na.rm = TRUE),
    sd_total_patients = sd(total_all, na.rm = TRUE),
    
    avg_percent_over_18 = mean(percent_over_18, na.rm = TRUE),
    sd_percent_over_18 = sd(percent_over_18, na.rm = TRUE),
    
    avg_percent_over_52 = mean(percent_over_52, na.rm = TRUE),
    sd_percent_over_52 = sd(percent_over_52, na.rm = TRUE),
    
    .groups = "drop"
  )

# Step 4: Save summary file
dir.create("output/clustering", showWarnings = FALSE)
write_csv(trust_summary, "output/clustering/trust_summary_all.csv")

# Step 5: Preview
print(trust_summary, n = Inf)

