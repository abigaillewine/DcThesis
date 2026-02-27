#Growth limear mixed model


library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(stringr)
library(car)
library(FSA)
library(multcompView)
library(mgcv)
library(ggplot2)
library(gratia)
library(lme4)

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



season_means <- filtered_df %>%
  group_by(OriginalTagID, Year) %>%
  summarise(
    mean_size = mean(CurvedCarapaceLength_NtoN, na.rm = TRUE),
    n_meas = n(),
    .groups = "drop"
  ) %>%
  filter(n_meas >= 2)

# ---------- 3. Calculate growth rates ----------
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


# calculate reproductive years (one row per turtle-year)
repro_years <- filtered_df %>%
  group_by(OriginalTagID) %>%
  mutate(
    first_nesting_year = min(Year, na.rm = TRUE),
    years_reproducing = Year - first_nesting_year
  ) %>%
  ungroup() %>%
  distinct(OriginalTagID, Year, years_reproducing)


# join correctly by turtle AND year
growth_grouped <- growth_rates %>%
  left_join(repro_years,
            by = c("OriginalTagID", "Year"))



lmm_longevity <- lmer(
  growth_rate ~ years_reproducing +
    (1 | OriginalTagID),
  data = growth_grouped
)

summary(lmm_longevity)

isSingular(lmm_longevity)





lm_longevity <- lm(growth_rate ~ years_reproducing,
                   data = growth_grouped)
summary(lm_longevity)





lm_longevity  <- lm(growth_rate ~ years_reproducing,
                    data = growth_grouped)

gam_longevity <- gam(growth_rate ~ s(years_reproducing, k = 5),
                     data = growth_grouped,
                     method = "REML")
summary(gam_longevity)

AIC(lm_longevity, gam_longevity)





ggplot(growth_grouped, aes(x = years_reproducing, y = growth_rate)) +
  
  # individual observations
  geom_point(alpha = 0.35, size = 2, color = "darkblue") +
  
  # GAM smooth
  geom_smooth(method = "gam",
              formula = y ~ s(x, k = 5),
              color = "black",
              fill = "gray",
              size = 1.3,
              se = TRUE) +
  
  # labels
  labs(x = "Years Reproductively Active",
       y = "Growth Rate (cm yr⁻¹)") +
  
  # presentation theme
  theme_classic(base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.margin = margin(10, 15, 10, 10)
  )






gam_remigration <- gam(growth_rate ~ s(remigration_interval, k = 5),
                     data = growth_grouped,
                     method = "REML")
summary(gam_remigration)



ggplot(growth_grouped, aes(x = remigration_interval, y = growth_rate)) +
  
  # individual observations
  geom_point(alpha = 0.35, size = 2, color = "darkblue") +
  
  # GAM smooth
  geom_smooth(method = "gam",
              formula = y ~ s(x, k = 5),
              color = "black",
              fill = "gray",
              size = 1.3,
              se = TRUE) +
  
  # labels
  labs(x = "Remigration Interval",
       y = "Growth Rate (cm yr⁻¹)") +
  
  # presentation theme
  theme_classic(base_size = 16) +
  theme(
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    plot.margin = margin(10, 15, 10, 10)
  )

