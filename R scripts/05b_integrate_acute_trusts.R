# ---------------------------------------------------------------
# scripts/05b_integrate_acute_trusts.R
# Filter trust summary to include only valid Acute NHS trusts
# ---------------------------------------------------------------

library(readxl)
library(dplyr)
library(janitor)
library(readr)

# Step 1: Load ODS acute trust list and extract codes
acute_codes <- read_excel("data/ODS_acute_nhs_trusts.xlsx") %>%
  clean_names() %>%
  transmute(provider_org_code = code) %>%
  mutate(provider_org_code = trimws(provider_org_code))

# Step 2: Load full trust-level summary
trust_summary_all <- read_csv("output/clustering/trust_summary_all.csv", show_col_types = FALSE) %>%
  mutate(provider_org_code = trimws(provider_org_code))

# Step 3: Keep only trusts matching ODS acute codes
trust_summary_acute <- trust_summary_all %>%
  semi_join(acute_codes, by = "provider_org_code")

# Step 4: Save acute-only file
write_csv(trust_summary_acute, "output/clustering/trust_summary_acute_only.csv")

# Step 5: Preview
print(trust_summary_acute, n = 10)