#Reproductive longevity Kaplain
library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(stringr)
library(writexl)
library(survival)

DcDataRemoved <- read_csv("~/R/Projects/Thesis/data/DcDataRemoved.csv")




# ---- Clean nest lays ----
clean_df <- DcDataRemoved %>%
  mutate(results_clean = str_to_lower(str_trim(Result))) %>%
  filter(results_clean %in% c("relocate", "lay", "probable lay", "in situ")) %>%
  select(-results_clean)

# ---- Separate dates ----
DcDates <- clean_df %>%
  mutate(DateOfActivity = mdy(DateOfActivity),  # convert to Date type
         month = month(DateOfActivity),
         day   = day(DateOfActivity),
         year  = year(DateOfActivity))

# ---- Remove +/- 1 cm difference in same year ----
filtered_df <- DcDates %>%
  group_by(OriginalTagID, year) %>%
  arrange(DateOfActivity) %>%
  mutate(first_CCL = first(CurvedCarapaceLength_NtoN)) %>%
  filter(abs(CurvedCarapaceLength_NtoN - first_CCL) <= 1) %>%
  ungroup()

# ---- Calculate reproductive longevity ----
turtle_longevity <- filtered_df %>%
  group_by(OriginalTagID) %>%
  summarise(
    first_year = min(year(DateOfActivity), na.rm = TRUE),
    last_year  = max(year(DateOfActivity), na.rm = TRUE),
    longevity  = last_year - first_year,
    .groups = "drop"
  )

# ---- Prepare reproductive span ----
reproductive_span <- turtle_longevity %>%
  rename(individual_id = OriginalTagID,
         first_nest = first_year,
         last_nest  = last_year,
         span       = longevity) %>%
  mutate(
    n_seasons = last_nest - first_nest + 1,
    # Event = 1 if reproduction ended, 0 if censored (still potentially reproducing)
    event = ifelse(last_nest == max(DcDates$year, na.rm = TRUE), 0, 1)
  )

# ---- Kaplan-Meier survival model ----
surv_model <- survfit(Surv(span, event) ~ 1, data = reproductive_span)

# ---- Summary ----
summary(surv_model)

# ---- Plot ----
plot(surv_model,
     xlab = "Years of Reproduction",
     ylab = "Probability of Continuing Reproduction",
     main = "Kaplan–Meier Estimate of Reproductive Longevity",
     col = "steelblue",
     lwd = 2)
grid()



library(survival)
library(survminer)

ggsurvplot(
  surv_model,
  xlab = "Years since first nesting",
  ylab = "Probability of remaining reproductively active",
  conf.int = TRUE
)


# Restricted mean reproductive longevity up to max observed time
rmst <- surv_model$time %*% surv_model$surv / sum(diff(c(0, surv_model$time)))
rmst

# Using survival package
mean_longevity <- summary(surv_model)$table["*rmean"]
mean_longevity

# surv_model is your KM fit
times <- surv_model$time       # the observed spans
surv_prob <- surv_model$surv   # corresponding survival probabilities

# Calculate approximate mean (area under the KM curve)
# Use the trapezoidal rule
delta_time <- c(diff(c(0, times)))  # time intervals
mean_longevity <- sum(surv_prob * delta_time)
mean_longevity












############## WITH TURTLES NESTING ONLY 1 YEAR EXCLUDED###################



# ---- Reproductive span dataset ----
reproductive_span <- turtle_longevity %>%
  rename(individual_id = OriginalTagID,
         first_nest = first_year,
         last_nest  = last_year,
         span       = longevity) %>%
  mutate(
    n_seasons = last_nest - first_nest + 1,
    event = ifelse(last_nest == max(DcDates$year, na.rm = TRUE), 0, 1)  # 1 = event, 0 = censored
  )

# ---- Kaplan–Meier model ----
surv_model <- survfit(Surv(span, event) ~ 1, data = reproductive_span)

# ---- Median reproductive longevity ----
median_longevity <- summary(surv_model)$table["median"]

# ---- Restricted mean reproductive longevity (area under curve) ----
times <- surv_model$time
surv_prob <- surv_model$surv
delta_time <- c(times[1], diff(times))
mean_longevity <- sum(surv_prob * delta_time)

median_longevity
mean_longevity



repeat_nesters <- reproductive_span %>%
  filter(n_seasons > 1)
surv_model_repeat <- survfit(Surv(span, event) ~ 1, data = repeat_nesters)
plot(surv_model_repeat,
     xlab = "Years of Reproduction",
     ylab = "Probability of Continuing Reproduction",
     main = "Kaplan–Meier Estimate of Reproductive Longevity",
     col = "steelblue",
     lwd = 2)

median_repeat <- summary(surv_model_repeat)$table["median"]
median_repeat

times_repeat <- surv_model_repeat$time
surv_prob_repeat <- surv_model_repeat$surv

delta_time_repeat <- c(times_repeat[1], diff(times_repeat))

mean_repeat <- sum(surv_prob_repeat * delta_time_repeat)
mean_repeat

