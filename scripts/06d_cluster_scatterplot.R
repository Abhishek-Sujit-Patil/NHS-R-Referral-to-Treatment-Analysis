library(tidyverse)
library(janitor)
library(readr)
library(stringr)
library(plotly)

# Load data
trust_clusters <- read_csv("output/clustering/trust_clusters.csv", show_col_types = FALSE)

rtt_all_months <- read_csv("data/processed/rtt_all_months.csv", show_col_types = FALSE) %>%
  clean_names()

# Filter acute trusts only
rtt_filtered <- rtt_all_months %>%
  filter(provider_org_code %in% trust_clusters$provider_org_code)

# Week columns for total patient calculation
week_cols_all <- names(rtt_filtered)[str_detect(names(rtt_filtered), "^gt_.*")]

# Calculate total patients per trust
trust_totals <- rtt_filtered %>%
  group_by(provider_org_code) %>%
  summarise(
    total_patients = sum(across(all_of(week_cols_all)), na.rm = TRUE),
    .groups = "drop"
  )

# Merge and prepare tooltip text
trust_plot_data <- trust_clusters %>%
  left_join(trust_totals, by = "provider_org_code") %>%
  mutate(
    trust_label = str_to_title(provider_org_name),
    hover_text = paste0(
      trust_label, "<br>",
      "Over 18w Delays: ", round(percent_over_18, 1), "%<br>",
      "Over 52w Delays: ", round(percent_over_52, 1), "%<br>",
      "Total Patients: ", scales::comma(total_patients)
    )
  )

# Interactive Plot
plot_ly(
  data = trust_plot_data,
  x = ~percent_over_18,
  y = ~percent_over_52,
  type = "scatter",
  mode = "markers",
  text = ~hover_text,
  hoverinfo = "text",
  color = ~as.factor(cluster),
  colors = "Set2",
  marker = list(size = 8, opacity = 0.8),
  showlegend = FALSE
) %>%
  layout(
    title = "NHS Trust Clustering by RTT Delays (April 2024 – Feb 2025)",
    xaxis = list(title = "% Over 18 Weeks", tickformat = ".0f"),
    yaxis = list(title = "% Over 52 Weeks", tickformat = ".0f"),
    margin = list(l = 60, r = 50, b = 60, t = 60)
  )