library(readr)
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
DcPractice <- read_csv("~/R/Projects/Thesis/data/DcPractice.csv")
View(DcPractice)

str(DcPractice)


# clean rows so we are only looking at the nest lays
clean_df <- DcPractice %>%
  mutate(results_clean = str_to_lower(str_trim(Result))) %>%
  filter(results_clean %in% c("relocate", "lay", "probable lay", "in situ")) %>%
  select(-results_clean)


#separate dates into individual columns

DcDates <- clean_df %>%
  mutate(DateOfActivity = mdy(DateOfActivity),  # convert to Date type
         month = month(DateOfActivity),
         day   = day(DateOfActivity),
         year  = year(DateOfActivity))


#reproductive longevity

library(dplyr)
library(lubridate)

# Per-turtle longevity
turtle_longevity <- DcDates %>%
  group_by(OriginalTagID) %>%
  summarise(
    first_year = min(year(DateOfActivity), na.rm = TRUE),
    last_year  = max(year(DateOfActivity), na.rm = TRUE),
    longevity  = last_year - first_year,
    .groups = "drop"
  )

# Overall summary
longevity_summary <- turtle_longevity %>%
  summarise(
    mean_longevity   = mean(longevity, na.rm = TRUE),
    median_longevity = median(longevity, na.rm = TRUE),
    max_longevity    = max(longevity, na.rm = TRUE)
  )



# Overall summary (excluding turtles with longevity = 0)
newlongevity_summary <- turtle_longevity %>%
  filter(longevity > 0) %>%
  summarise(
    mean_longevity   = mean(longevity, na.rm = TRUE),
    median_longevity = median(longevity, na.rm = TRUE),
    max_longevity    = max(longevity, na.rm = TRUE)
  )



#sample size without 0

NoZero <- turtle_longevity %>%
  filter(longevity > 0)






# Find each turtle's first nesting year
first_nesting <- DcDates %>%
  group_by(OriginalTagID) %>%
  summarise(first_year = min(year(DateOfActivity), na.rm = TRUE),
            .groups = "drop")

# Keep only rows from the first nesting year
first_year_data <- DcDates %>%
  mutate(Year = year(DateOfActivity)) %>%
  inner_join(first_nesting, by = "OriginalTagID") %>%
  filter(Year == first_year)

# Now calculate size at sexual maturity
size_summary <- first_year_data %>%
  summarise(
    min_CCL  = min(CurvedCarapaceLength_NtoN, na.rm = TRUE),
    mean_CCL = mean(CurvedCarapaceLength_NtoN, na.rm = TRUE),
    max_CCL  = max(CurvedCarapaceLength_NtoN, na.rm = TRUE),
    
    min_CCW  = min(CurvedCarapaceWidth, na.rm = TRUE),
    mean_CCW = mean(CurvedCarapaceWidth, na.rm = TRUE),
    max_CCW  = max(CurvedCarapaceWidth, na.rm = TRUE)
  )



