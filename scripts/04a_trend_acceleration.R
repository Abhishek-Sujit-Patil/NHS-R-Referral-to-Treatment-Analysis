#Module 4A: Month-on-Month Change in RTT Delays
# -----------------------------------------------

library(tidyverse)
library(lubridate)
library(janitor)

# Load data
monthly <- read_csv("output/monthly_summary.csv", show_col_types = FALSE) %>%
  clean_names()

# Arrange and calculate change per category
trend_change <- monthly %>%
  arrange(category, period) %>%
  group_by(category) %>%
  mutate(
    change_percent_over_18 = round(percent_over_18 - lag(percent_over_18), 2),
    change_percent_over_52 = round(percent_over_52 - lag(percent_over_52), 2)
  ) %>%
  ungroup()

# Save result
write_csv(trend_change, "output/trend_change.csv")

# Preview full result
print(trend_change, n = Inf)