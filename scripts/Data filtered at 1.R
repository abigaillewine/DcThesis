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







####################################################


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



# only turtles that have nested 10 years or more


OverTen <- filtered_df %>%
  group_by(OriginalTagID) %>%
  filter(max(year(DateOfActivity), na.rm = TRUE) - 
           min(year(DateOfActivity), na.rm = TRUE) >= 10) %>%
  ungroup()



# Calculate growth rates
growth_rates <- OverTen %>%
  arrange(OriginalTagID, DateOfActivity) %>%
  group_by(OriginalTagID) %>%
  mutate(
    prev_CCL  = lag(CurvedCarapaceLength_NtoN),
    prev_date = lag(DateOfActivity),
    days_elapsed = as.numeric(difftime(DateOfActivity, prev_date, units = "days")),
    growth_rate = (CurvedCarapaceLength_NtoN - prev_CCL) / days_elapsed * 365.25,
    years_since_first = as.numeric(difftime(DateOfActivity, min(DateOfActivity, na.rm = TRUE), units = "days")) / 365.25
  ) %>%
  filter(!is.na(growth_rate) & days_elapsed > 0) %>%  # remove 0-day intervals
  ungroup()


# Summarize growth rates
growth_summary <- growth_rates %>%
  summarise(
    min_growth    = min(growth_rate, na.rm = TRUE),
    mean_growth   = mean(growth_rate, na.rm = TRUE),
    median_growth = median(growth_rate, na.rm = TRUE),
    max_growth    = max(growth_rate, na.rm = TRUE)
  )


sample_size <- filtered_df %>%
  group_by(OriginalTagID) %>%
  summarise(n_observations = n(), .groups = "drop")


#bins for each decade
growth_rates_bins <- growth_rates %>%
  mutate(
    growth_phase = case_when(
      years_since_first >= 1  & years_since_first <= 10 ~ "Years 1–10",
      years_since_first >= 11 & years_since_first <= 20 ~ "Years 11–20",
      years_since_first >= 21 & years_since_first <= 31 ~ "Years 21–31",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(growth_phase))


#summarize by bins
growth_by_phase <- growth_rates_bins %>%
  group_by(growth_phase) %>%
  summarise(
    mean_growth   = mean(growth_rate, na.rm = TRUE),
    median_growth = median(growth_rate, na.rm = TRUE),
    sd_growth     = sd(growth_rate, na.rm = TRUE),
    n             = n(),
    .groups = "drop"
  )



#compare bins, not significant
kruskal.test(growth_rate ~ growth_phase, data = growth_rates_bins)



ggplot(growth_rates_bins, aes(x = growth_phase, y = growth_rate, fill = growth_phase)) +geom_jitter()+
  geom_boxplot() +
  labs(
    x = "Nesting year phase",
    y = "Growth rate (cm yr⁻¹)",
    title = "Change in growth rate across nesting years"
  ) +
  theme_minimal()




##########################

#re-try growth, same measurements

growth_rates <- OverTen %>%
  arrange(OriginalTagID, DateOfActivity) %>%
  group_by(OriginalTagID) %>%
  mutate(
    prev_CCL  = lag(CurvedCarapaceLength_NtoN),
    prev_date = lag(DateOfActivity),
    
    # Calculate years between nesting events directly
    years_elapsed = as.numeric(difftime(DateOfActivity, prev_date, units = "days")) / 365.25,
    
    # Calculate annualized growth rate (cm per year)
    growth_rate = (CurvedCarapaceLength_NtoN - prev_CCL) / years_elapsed
  ) %>%
  filter(!is.na(growth_rate) & years_elapsed > 0) %>%
  ungroup()


growth_summary_pop1 <- growth_rates %>%
  summarise(
    n_observations = n(),
    mean_growth   = mean(growth_rate, na.rm = TRUE),
    median_growth = median(growth_rate, na.rm = TRUE),
    sd_growth     = sd(growth_rate, na.rm = TRUE),
    min_growth    = min(growth_rate, na.rm = TRUE),
    max_growth    = max(growth_rate, na.rm = TRUE)
  )

growth_summary_pop1



#remove all negative values

growth_summary_pop2 <- growth_rates %>%
  filter(growth_rate > 0) %>%  # only positive growth
  summarise(
    n_observations = n(),
    mean_growth   = mean(growth_rate, na.rm = TRUE),
    median_growth = median(growth_rate, na.rm = TRUE),
    sd_growth     = sd(growth_rate, na.rm = TRUE),
    min_growth    = min(growth_rate, na.rm = TRUE),
    max_growth    = max(growth_rate, na.rm = TRUE)
  )

##########################################
# keep only realistic growth

growth_summary_pop3 <- growth_rates %>%
  filter(growth_rate > -1) %>%  # keep only realistic growth
  summarise(
    n_observations = n(),
    mean_growth   = mean(growth_rate, na.rm = TRUE),
    median_growth = median(growth_rate, na.rm = TRUE),
    sd_growth     = sd(growth_rate, na.rm = TRUE),
    min_growth    = min(growth_rate, na.rm = TRUE),
    max_growth    = max(growth_rate, na.rm = TRUE)
  )



######################################################################



#growth rates for decades without values <-1

growth_rates <- OverTen %>%
  arrange(OriginalTagID, DateOfActivity) %>%
  group_by(OriginalTagID) %>%
  mutate(
    prev_CCL  = lag(CurvedCarapaceLength_NtoN),
    prev_date = lag(DateOfActivity),
    days_elapsed = as.numeric(difftime(DateOfActivity, prev_date, units = "days")),
    growth_rate = (CurvedCarapaceLength_NtoN - prev_CCL) / days_elapsed * 365.25,
    years_since_first = as.numeric(difftime(DateOfActivity, min(DateOfActivity, na.rm = TRUE), units = "days")) / 365.25
  ) %>%
  filter(!is.na(growth_rate) & days_elapsed > 0) %>%  # remove 0-day intervals
  ungroup()

growth <- growth_rates %>%
  filter(growth_rate > -1)  # keep only realistic growth
  

growth_rates_bins_cleaned <- growth %>%
  mutate(
    growth_phase = case_when(
      years_since_first >= 1  & years_since_first <= 10 ~ "Years 1–10",
      years_since_first >= 11 & years_since_first <= 20 ~ "Years 11–20",
      years_since_first >= 21 & years_since_first <= 31 ~ "Years 21–31",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(growth_phase))

#compare bins, not significant
kruskal.test(growth_rate ~ growth_phase, data = growth_rates_bins_cleaned)

#plot (boxplot)
ggplot(growth_rates_bins_cleaned, aes(x = growth_phase, y = growth_rate, fill = growth_phase)) +geom_jitter()+
  geom_boxplot() +
  labs(
    x = "Nesting year phase",
    y = "Growth rate (cm yr⁻¹)",
    title = "Change in growth rate across nesting years"
  ) +
  theme_minimal()



growth_summary_phase <- growth_rates_bins_cleaned %>%
  group_by(growth_phase) %>%
  summarise(
    mean_growth   = mean(growth_rate, na.rm = TRUE),
    median_growth = median(growth_rate, na.rm = TRUE),
    sd_growth     = sd(growth_rate, na.rm = TRUE),
    min_growth    = min(growth_rate, na.rm = TRUE),
    max_growth    = max(growth_rate, na.rm = TRUE),
    n_observations = n()
  )

#plot (column)
ggplot(growth_summary_phase,
       aes(x = growth_phase, y = mean_growth, fill = growth_phase)) +
  geom_col() +
  geom_errorbar(
    aes(ymin = mean_growth - sd_growth,
        ymax = mean_growth + sd_growth),
    width = 0.2,
    linewidth = 0.7
  ) +
  labs(
    x = "Nesting year phase",
    y = "Mean growth rate (cm yr⁻¹)",
    title = "Mean growth rate by nesting year phase"
  ) +
  theme_minimal() +
  theme(legend.position = "none")



##########


#size at sexual maturity across years
ggplot(first_year_data, aes(x = first_year, y = first_CCL)) +geom_jitter()+
  labs(
    x = "Year",
    y = "Size at Sexual Maturity",
    title = "Change in Size at Sexual Maturity across years"
  ) +
  theme_minimal()


model <- lm(first_year ~ first_CCL, data=first_year_data)

summary(model)