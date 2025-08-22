# Step 1: Install required packages (if not already installed)
required_packages <- c("tidyverse", "janitor", "lubridate")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

# Step 2: Load libraries
library(tidyverse)
library(janitor)
library(lubridate)

# Step 3: List all CSV files from data/raw/
file_paths <- list.files("data/raw/", pattern = "\\.csv$", full.names = TRUE)

# Step 4: Extract month from filename (e.g., RTT-April-2024 → 2024-04-01)
get_month_from_filename <- function(path) {
  file <- basename(path)
  str_extract(file, "(?i)(January|February|March|April|May|June|July|August|September|October|November|December)-20\\d{2}") %>%
    parse_date(format = "%B-%Y")
}

# Step 5: Read, clean, and combine all files
rtt_data <- map_dfr(file_paths, ~ {
  read_csv(.x, show_col_types = FALSE) %>%
    clean_names() %>%
    mutate(period = get_month_from_filename(.x))
})

# Step 6: Normalize rtt_part_description to lowercase for filtering
rtt_data <- rtt_data %>%
  mutate(rtt_part_description = str_to_lower(rtt_part_description))

# Step 7: Filter for the 3 relevant categories
rtt_filtered <- rtt_data %>%
  filter(rtt_part_description %in% c(
    "incomplete pathways",
    "incomplete pathways with dta",
    "completed pathways for admitted patients"
  )) %>%
  mutate(
    category = case_when(
      rtt_part_description == "incomplete pathways" ~ "Incomplete Pathways",
      rtt_part_description == "incomplete pathways with dta" ~ "Incomplete Pathways with DTA",
      rtt_part_description == "completed pathways for admitted patients" ~ "Completed Pathways (Admitted)"
    )
  )

# Step 8: Save combined and individual outputs
write_csv(rtt_filtered, "output/rtt_combined_all_categories.csv")
write_csv(filter(rtt_filtered, category == "Incomplete Pathways"), "output/rtt_incomplete.csv")
write_csv(filter(rtt_filtered, category == "Incomplete Pathways with DTA"), "output/rtt_dta.csv")
write_csv(filter(rtt_filtered, category == "Completed Pathways (Admitted)"), "output/rtt_completed.csv")

# Step 9: Preview structure
print(glimpse(rtt_filtered))

