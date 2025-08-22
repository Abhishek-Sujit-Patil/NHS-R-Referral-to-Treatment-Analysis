# Step 1: Load libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

# Step 2: Load summary dataset
monthly <- read_csv("output/monthly_summary.csv", show_col_types = FALSE)

# Step 3: Convert period to proper date (if not already)
monthly <- monthly %>%
  mutate(period = as.Date(period))

# Step 4: Plot % Over 18 Weeks
plot_18 <- monthly %>%
  ggplot(aes(x = period, y = percent_over_18, color = category)) +
  geom_line(size = 0.8) +
  geom_point() +
  labs(
    title = "% of Patients Waiting Over 18 Weeks",
    x = "Month",
    y = "% Over 18 Weeks",
    color = "RTT Category"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# Step 5: Plot % Over 52 Weeks (exclude completed)
plot_52 <- monthly %>%
  filter(!str_detect(category, "Completed")) %>%
  ggplot(aes(x = period, y = percent_over_52, color = category)) +
  geom_line(size = 0.8) +
  geom_point() +
  labs(
    title = "% of Patients Waiting Over 52 Weeks",
    x = "Month",
    y = "% Over 52 Weeks",
    color = "RTT Category"
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# Step 6: Plot total patients by category (line plot)
plot_total_trend <- ggplot(monthly, aes(x = period, y = total_patients, color = category)) +
  geom_line(size = 0.8) +
  geom_point() +
  scale_x_date(date_labels = "%b '%y", date_breaks = "1 month") +
  scale_y_continuous(labels = label_number(scale_cut = cut_si("M"))) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Monthly Total Patients by RTT Category",
    x = "Month", y = "Total Patients", color = "Category"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

# Step 7: Print all plots
print(plot_18)
print(plot_52)
print(plot_total_trend)

