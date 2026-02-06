#Fixed growth rates

library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(stringr)

DcDataRemoved <- read_csv("~/R/Projects/Thesis/data/DcDataRemoved.csv")



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



#growth rate



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




#binned growth rate

OverTen <- filtered_df %>%
  group_by(OriginalTagID) %>%
  filter(max(year(DateOfActivity), na.rm = TRUE) - 
           min(year(DateOfActivity), na.rm = TRUE) >= 10) %>%
  ungroup()






# 1. Keep valid measurements (modify column name if needed)
valid <- OverTen %>%
  filter(!is.na(CurvedCarapaceLength_NtoN))

# 2. Mean size per turtle per nesting season
season_means <- valid %>%
  group_by(OriginalTagID, Year) %>%
  summarise(
    mean_size = mean(CurvedCarapaceLength_NtoN, na.rm = TRUE),
    n_meas = n(),
    .groups = "drop"
  ) %>%
  # keep only seasons with at least 2 measurements
  filter(n_meas >= 2)

# 3. Compare each season with previous season
growth_rates <- season_means %>%
  arrange(OriginalTagID, Year) %>%
  group_by(OriginalTagID) %>%
  mutate(
    prev_year = lag(Year),
    prev_size = lag(mean_size),
    remigration_interval = Year - prev_year,
    growth = mean_size - prev_size,
    growth_rate = growth / remigration_interval
  ) %>%
  filter(!is.na(growth_rate)) %>%
  ungroup()

growth_rates


growth_summary <- growth_rates %>%
  summarise(
    min_growth    = min(growth_rate, na.rm = TRUE),
    mean_growth   = mean(growth_rate, na.rm = TRUE),
    median_growth = median(growth_rate, na.rm = TRUE),
    max_growth    = max(growth_rate, na.rm = TRUE)
  )




hist(growth_rates$growth_rate[!is.na(growth_rates$growth_rate)],
     main = "Frequency Distribution of Leatherback Growth Rates",
     xlab = "Post Maturation Growth rate (cm yr⁻¹)",
     ylab = "Turtles",
     col = "lightpink",
     border = "black")





#growth bins


repro_years <- filtered_df %>%
  group_by(OriginalTagID) %>%
  summarise(
    first_year = min(Year, na.rm = TRUE),
    last_year  = max(Year, na.rm = TRUE),
    years_reproducing = last_year - first_year + 1,
    .groups = "drop"
  )



repro_years <- repro_years %>%
  mutate(
    longevity_group = case_when(
      years_reproducing <= 10 ~ "1-10",
      years_reproducing <= 20 ~ "10-20",
      years_reproducing <= 30 ~ "20-30",
      TRUE ~ "30+"
    )
  )

growth_grouped <- growth_rates %>%
  left_join(repro_years %>% select(OriginalTagID, longevity_group),
            by = "OriginalTagID")

growth_summary <- growth_grouped %>%
  group_by(longevity_group) %>%
  summarise(
    n = n(),
    mean_growth = mean(growth_rate, na.rm = TRUE),
    sd_growth = sd(growth_rate, na.rm = TRUE),
    .groups = "drop"
  )

growth_summary



#ANOVA

by(growth_grouped$growth_rate,
   growth_grouped$longevity_group,
   shapiro.test)

library(car)

leveneTest(growth_rate ~ longevity_group, data = growth_grouped)


anova_model <- aov(growth_rate ~ longevity_group, data = growth_grouped)
summary(anova_model)
TukeyHSD(anova_model)

ggplot(growth_grouped,
       aes(x = longevity_group, y = growth_rate)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1) +
  labs(title = "Growth Rates by Reproductive Longevity Group",
       x = "Years Reproducing",
       y = "Growth rate (cm/year)") +
  theme_minimal()




ggplot(growth_grouped,
       aes(x = longevity_group,
           y = growth_rate,
           fill = longevity_group)) +
  
  geom_violin(trim = FALSE, alpha = 0.5, color = "black") +
  
  # show all observations
  geom_jitter(width = 0.15,
              alpha = 0.6,
              size = 1.8,
              aes(color = longevity_group),
              show.legend = FALSE) +
  
  # mean point
  stat_summary(fun = mean,
               geom = "point",
               size = 3,
               color = "black") +
  
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  
  labs(title = "Growth Rate Across Nesting Years",
       x = "Nesting Year Phase",
       y = "Growth rate (cm yr⁻¹)") +
  
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

