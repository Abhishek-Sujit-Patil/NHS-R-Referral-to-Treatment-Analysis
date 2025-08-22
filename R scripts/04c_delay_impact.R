# -----------------------------------------------
# Module 4C: Delay Impact Analysis
# -----------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)

# Load monthly summary
monthly <- read_csv("output/monthly_summary.csv", show_col_types = FALSE) %>%
  clean_names()

# Get backlog + over18/52 for Incomplete + DTA
delay_df <- monthly %>%
  filter(category %in% c("Incomplete Pathways", "Incomplete Pathways with DTA")) %>%
  group_by(period) %>%
  summarise(
    total_backlog = sum(total_patients, na.rm = TRUE),
    patients_over_18 = sum(over_18_weeks, na.rm = TRUE),
    patients_over_52 = sum(over_52_weeks, na.rm = TRUE)
  ) %>%
  mutate(
    percent_over_18 = round(patients_over_18 / total_backlog * 100, 1),
    percent_over_52 = round(patients_over_52 / total_backlog * 100, 1)
  )

# Save output
write_csv(delay_df, "output/delay_impact.csv")

# Preview
print(delay_df, n = Inf)