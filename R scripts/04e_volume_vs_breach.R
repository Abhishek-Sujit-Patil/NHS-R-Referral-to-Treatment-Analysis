# -----------------------------------------------
# scripts/04e_volume_vs_breach.R
# -----------------------------------------------

library(tidyverse)
library(janitor)
library(scales)
library(lubridate)

# Load data
monthly <- read_csv("output/monthly_summary.csv", show_col_types = FALSE) %>%
  clean_names()

# Latest mont
latest_month <- max(monthly$period)

snapshot <- monthly %>%
  filter(period == latest_month) %>%
  mutate(
    label_pos = if_else(row_number() %% 2 == 0, -1.3, 1.3),
    percent_label = paste0(percent_over_18, "%")
  )

# Plot
plot_volume_breach <- ggplot(snapshot, aes(
  x = total_patients,
  y = percent_over_18,
  color = category
)) +
  geom_point(size = 4, alpha = 0.9) +
  geom_text(aes(label = percent_label, vjust = label_pos), size = 3) +
  geom_hline(yintercept = 92, linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text",
           x = max(snapshot$total_patients) * 0.9,
           y = 91,
           label = "92% Target",
           color = "black",
           size = 3,
           fontface = "italic") +
  scale_x_continuous(
    labels = label_number(scale = 1e-6, suffix = "M"),
    limits = c(0, max(snapshot$total_patients) * 1.1)
  ) +
  scale_y_continuous(
    labels = percent_format(scale = 1),
    limits = c(0, 100)
  ) +
  labs(
    title = paste0("Overview ", format(latest_month, "%b %Y")),
    subtitle = NULL,
    x = "Total Patients (Millions)",
    y = "% Waiting Over 18 Weeks",
    color = "RTT Category"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
    legend.position = "bottom",
    legend.direction = "horizontal",  # ← stack legend items
    legend.box = "vertical",
    legend.text = element_text(size = 9),
    axis.text.x = element_text(angle = 0, hjust = 1)
  )

# Show plot
print(plot_volume_breach)
