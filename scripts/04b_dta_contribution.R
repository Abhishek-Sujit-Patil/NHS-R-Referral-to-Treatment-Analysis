# -----------------------------------------------
# Module 4B: DTA Contribution to Backlog
# -----------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)

# Load summary data
monthly <- read_csv("output/monthly_summary.csv", show_col_types = FALSE) %>%
  clean_names()

# Step 1: Filter only backlog-related categories
backlog_df <- monthly %>%
  filter(category %in% c("Incomplete Pathways", "Incomplete Pathways with DTA")) %>%
  select(period, category, total_patients) %>%
  pivot_wider(names_from = category, values_from = total_patients) %>%
  rename(
    incomplete = `Incomplete Pathways`,
    dta = `Incomplete Pathways with DTA`
  ) %>%
  mutate(
    total_backlog = incomplete + dta,
    dta_share = round(dta / total_backlog * 100, 1),
    incomplete_share = round(incomplete / total_backlog * 100, 1)
  )

# Step 2: Save the output
write_csv(backlog_df, "output/dta_backlog_contribution.csv")

# Step 3: Preview
print(backlog_df, n = Inf)

library(ggplot2)
library(scales)
library(readr)

# Load prepared DTA share data
backlog_df <- read_csv("output/dta_backlog_contribution.csv", show_col_types = FALSE)

# Plot DTA Share Over Time
plot_dta_share <- ggplot(backlog_df, aes(x = period, y = dta_share)) +
  geom_line(color = "#d62728", size = 0.8) +
  geom_point(color = "#d62728", size = 2) +
  scale_x_date(date_labels = "%b '%y", date_breaks = "1 month") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(
    title = "DTA Share of Total RTT Backlog",
    subtitle = "Proportion of patients waiting after decision to treat",
    x = "Month",
    y = "DTA Share (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Show plot
print(plot_dta_share)