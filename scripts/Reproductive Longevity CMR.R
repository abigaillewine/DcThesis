#Reproductive Longevity CMR



#A capture–mark–recapture (CMR) survival model estimates apparent survival (Φ) while accounting for detection probability (p).
#For nesting beach data, the standard starting model is the Cormack–Jolly–Seber (CJS) model.

library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(stringr)
library(writexl)
library(RMark)

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



years <- seq(min(filtered_df$Year), max(filtered_df$Year))

#-----------------------------
# 2. Build capture histories
#-----------------------------
annual_detect <- filtered_df %>%
  distinct(OriginalTagID, Year) %>%
  mutate(obs = 1)

capture_histories <- annual_detect %>%
  complete(OriginalTagID, Year = years, fill = list(obs = 0)) %>%
  arrange(OriginalTagID, Year) %>%
  group_by(OriginalTagID) %>%
  summarise(ch = paste(obs, collapse = ""), .groups = "drop")

#-----------------------------
# 3. Run CJS model
#-----------------------------
cmr_data <- process.data(capture_histories, model = "CJS")
cmr_ddl  <- make.design.data(cmr_data)

# Fit constant survival
cjs_model <- mark(
  data = cmr_data,
  ddl  = cmr_ddl,
  model.parameters = list(
    Phi = list(formula = ~1),
    p   = list(formula = ~1)
  )
)

summary(cjs_model)

#-----------------------------
# 4. Compute reproductive longevity
#-----------------------------
# Phi = annual survival probability
phi <- cjs_model$results$real$estimate[1]

# Expected reproductive longevity (years) = 1 / (1 - survival)
# This assumes reproduction continues until death
repro_longevity <- 1 / (1 - phi)
cat("Estimated reproductive longevity (years):", round(repro_longevity, 1), "\n")

#-----------------------------
# 5. Make a figure
#-----------------------------
# If you want to show "per year" estimate across the study
phi_time <- data.frame(
  Year = years,
  ReproLongevity = rep(repro_longevity, length(years)),
  lcl = rep(1 / (1 - cjs_model$results$real$ucl[1]), length(years)),  # conservative lower bound
  ucl = rep(1 / (1 - cjs_model$results$real$lcl[1]), length(years))   # conservative upper bound
)

ggplot(phi_time, aes(x = Year, y = ReproLongevity)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.3, fill = "green") +
  ylab("Estimated Reproductive Longevity (years)") +
  xlab("Year") +
  theme_minimal(base_size = 14)

