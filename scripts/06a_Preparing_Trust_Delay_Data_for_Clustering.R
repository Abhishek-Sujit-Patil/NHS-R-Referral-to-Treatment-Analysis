library(tidyverse)
library(janitor)

# Load cleaned trust-level summary
trust_data <- read_csv("output/clustering/trust_delay_summary_FINAL.csv", show_col_types = FALSE) %>%
  clean_names()

# Prepare clustering input
trust_for_clustering <- trust_data %>%
  select(provider_org_code, provider_org_name, percent_over_18, percent_over_52) %>%
  drop_na()

trust_scaled <- trust_for_clustering %>%
  mutate(
    scaled_18 = scale(percent_over_18)[, 1],
    scaled_52 = scale(percent_over_52)[, 1]
  )

write_csv(trust_scaled, "output/clustering/trust_clustering_input.csv")