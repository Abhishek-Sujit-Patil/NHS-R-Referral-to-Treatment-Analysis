# NHS RTT: Backlog vs Completed Pathways Ratio
# ------------------------------------------

# Step 1: Load libraries
library(tidyverse)
library(janitor)
library(lubridate)

# Step 2: Load data
monthly <- read_csv("output/monthly_summary.csv", show_col_types = FALSE)

# Step 3: Prepare backlog (Incomplete + DTA)
backlog_df <- monthly %>%
  filter(category %in% c("Incomplete Pathways", "Incomplete Pathways with DTA")) %>%
  group_by(period) %>%
  summarise(
    backlog = sum(total_patients, na.rm = TRUE)
  )

# Step 4: Prepare completions (Admitted only)
treated_df <- monthly %>%
  filter(category == "Completed Pathways (Admitted)") %>%
  select(period, treated = total_patients)

# Step 5: Join & calculate ratio
bt_ratio <- backlog_df %>%
  left_join(treated_df, by = "period") %>%
  mutate(
    backlog_to_treatment_ratio = round(backlog / treated, 2)
  )

# Step 6: Save output
write_csv(bt_ratio, "output/backlog_to_treatment.csv")

# Step 7: Preview
print(bt_ratio, n = Inf)


# Load required packages
library(ggplot2)
library(scales)

# Plot backlog-to-treatment ratio over time
plot_bt_ratio <- ggplot(bt_ratio, aes(x = period, y = backlog_to_treatment_ratio)) +
  geom_line(color = "#1f77b4", size = 0.8) +
  geom_point(color = "#1f77b4", size = 2) +
  labs(
    title = "Backlog-to-Treatment Ratio Over Time",
    subtitle = "Total patients waiting (Incomplete + DTA) per 1 treated (Admitted)",
    x = "Month",
    y = "Backlog-to-Treatment Ratio"
  ) +
  scale_x_date(date_labels = "%b '%y", date_breaks = "1 month") +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Show the plot
print(plot_bt_ratio)