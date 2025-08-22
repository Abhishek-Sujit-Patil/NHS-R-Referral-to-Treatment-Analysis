# -----------------------------------------------
# Trust Typology with High/Medium/Low Volume and Delay
# -----------------------------------------------

library(tidyverse)

# Load trust delay summary
trust_delay <- read_csv("output/trust_typology_base.csv", show_col_types = FALSE)

# Step 1: Define volume categories
trust_delay <- trust_delay %>%
  mutate(
    volume_category = case_when(
      total_patients >= 1500000 ~ "High Volume",
      total_patients >= 700000 ~ "Medium Volume",
      TRUE ~ "Low Volume"
    )
  )

# Step 2: Define delay categories
trust_delay <- trust_delay %>%
  mutate(
    delay_category = case_when(
      percent_over_18 >= 50 ~ "High Delay",
      percent_over_18 >= 30 ~ "Medium Delay",
      TRUE ~ "Low Delay"
    )
  )

# Step 3: Assign creative typology labels
trust_delay <- trust_delay %>%
  mutate(
    typology_label = case_when(
      volume_category == "High Volume" & delay_category == "High Delay" ~ "Large Trusts Struggling to Manage Delays",
      volume_category == "High Volume" & delay_category == "Medium Delay" ~ "Large Trusts Managing Backlogs Steadily",
      volume_category == "High Volume" & delay_category == "Low Delay" ~ "Leading Large Performers",
      
      volume_category == "Medium Volume" & delay_category == "High Delay" ~ "Mid-Sized Trusts Facing Challenges",
      volume_category == "Medium Volume" & delay_category == "Medium Delay" ~ "Mid-Sized Steady Performers",
      volume_category == "Medium Volume" & delay_category == "Low Delay" ~ "High Performing Mid-Sized Trusts",
      
      volume_category == "Low Volume" & delay_category == "High Delay" ~ "Specialist or Small Trusts Under Pressure",
      volume_category == "Low Volume" & delay_category == "Medium Delay" ~ "Small Trusts Managing Well",
      volume_category == "Low Volume" & delay_category == "Low Delay" ~ "Top Performing Small Trusts",
      
      TRUE ~ "Unclassified"
    )
  )

# Step 4: Save final output
write_csv(trust_delay, "output/trust_typology_final.csv")

# Quick view
print(trust_delay, n = Inf)

