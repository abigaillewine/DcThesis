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



# Find each turtle's first nesting year
first_nesting <- filtered_df %>%
  group_by(OriginalTagID) %>%
  summarise(first_year = min(year(DateOfActivity), na.rm = TRUE),
            .groups = "drop")


# Keep only rows from the first nesting year
first_year_data <- filtered_df %>%
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



#plot size at sexual maturity across years
ggplot(first_year_data, aes(x = first_year, y = first_CCL)) +geom_jitter()+
  labs(
    x = "Year",
    y = "Size at Sexual Maturity",
    title = "Change in Size at Sexual Maturity across years"
  ) +
  theme_minimal()

#linear regression, this doesn't seem to be woth much
model <- lm(first_year ~ first_CCL, data=first_year_data)

summary(model)


#check assumptions

par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

#