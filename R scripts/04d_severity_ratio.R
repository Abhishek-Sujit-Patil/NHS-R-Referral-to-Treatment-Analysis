# -----------------------------------------------
# Module 4D: Severity Ratio (52+ / 18+)
# -----------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)

# Load monthly summary
monthly <- read_csv("output/monthly_summary.csv", show_col_types = FALSE) %>%
  clean_names()

# Filter backlog categories
severity_df <- monthly %>%
  filter(category %in% c("Incomplete Pathways", "Incomplete Pathways with DTA")) %>%
  group_by(period) %>%
  summarise(
    patients_over_18 = sum(over_18_weeks, na.rm = TRUE),
    patients_over_52 = sum(over_52_weeks, na.rm = TRUE)
  ) %>%
  mutate(
    severity_ratio = round(patients_over_52 / patients_over_18, 3)
  )

# Save output
write_csv(severity_df, "output/severity_ratio.csv")

# Preview
print(severity_df, n = Inf)

# Load severity data
severity_df <- read_csv("output/severity_ratio.csv", show_col_types = FALSE)

# Plot severity ratio trend
plot_severity_ratio <- ggplot(severity_df, aes(x = period, y = severity_ratio)) +
  geom_line(color = "#9467bd", size = 0.8) +
  geom_point(color = "#9467bd", size = 2) +
  scale_x_date(date_labels = "%b '%y", date_breaks = "1 month") +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, 0.14, by = 0.02),
    limits = c(0, 0.14)
  ) +
  labs(
    title = "Severity of Delay: % of Over-18-Week Patients Waiting Over 52 Weeks",
    x = "Month",
    y = "Severity Ratio (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Print plot
print(plot_severity_ratio)