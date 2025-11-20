library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(stringr)

DcDataRemoved <- read_csv("~/R/Projects/Thesis/data/DcDataRemoved.csv")
View(DcDataRemoved)

# clean rows so we are only looking at the nest lays
clean_df <- DcDataRemoved %>%
  mutate(results_clean = str_to_lower(str_trim(Result))) %>%
  filter(results_clean %in% c("relocate", "lay", "probable lay", "in situ")) %>%
  select(-results_clean)


#separate dates into individual columns

DcDates <- clean_df %>%
  mutate(DateOfActivity = mdy(DateOfActivity),  # convert to Date type
         month = month(DateOfActivity),
         day   = day(DateOfActivity),
         year  = year(DateOfActivity))


#remove values +/- 1 cm different in same yr

filtered_df <- DcDates %>%
  mutate(Year = year(DateOfActivity)) %>%
  group_by(OriginalTagID, Year) %>%
  arrange(DateOfActivity) %>%
  mutate(first_CCL = first(CurvedCarapaceLength_NtoN)) %>%
  filter(abs(CurvedCarapaceLength_NtoN - first_CCL) <= 1) %>%
  ungroup()








# Per-turtle longevity
turtle_longevity <- filtered_df %>%
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

#
#use the data from newlongevity_summary :)