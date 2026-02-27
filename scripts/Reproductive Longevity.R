library(dplyr)
library(lubridate)
library(readr)
library(tidyverse)
library(stringr)
library(writexl)

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


# filtered dataset as an excel
write_xlsx(filtered_df, "~/R/Projects/Thesis/data/Filtered_Dc.xlsx")





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




#histogram of the results

hist(turtle_longevity$longevity,
     main = "Frequency Distribution of Reproductive Longevity",
     xlab = "Reproductive Longevity",
     ylab = "Turtles",
     col = "lightblue",
     border = "black"
)


#histogram without zeros
turtle_longevity_no_zero <- turtle_longevity %>%
  dplyr::filter(longevity != 0)

hist(turtle_longevity_no_zero$longevity,
     main = "",
     xlab = "Reproductive Longevity",
     ylab = "Turtles",
     col = "lightblue",
     border = "black"
)



#Exclude turtles with a longevity less than 3

# Filter turtles with longevity >= 3
turtle_longevity_overthree <- turtle_longevity %>%
  dplyr::filter(longevity >= 3)

# Summary statistics
newlongevity_summary_overthree <- turtle_longevity_overthree %>%
  summarise(
    mean_longevity   = mean(longevity, na.rm = TRUE),
    sd_longevity     = sd(longevity, na.rm = TRUE),
    median_longevity = median(longevity, na.rm = TRUE),
    max_longevity    = max(longevity, na.rm = TRUE),
    n                = n()
  )

# Histogram
hist(turtle_longevity_overthree$longevity,
     main = "",
     xlab = "Reproductive Longevity (years)",
     ylab = "Turtles",
     col = "lightblue",
     border = "black"
)
mean_longevity <- newlongevity_summary_overthree$mean_longevity
abline(v = mean_longevity, lwd = 2, lty = 2)




# Check minimum value
min(turtle_longevity_overthree$longevity)


#check for stat differences
wilcox.test(turtle_longevity_overthree$longevity, mu = 8, alternative = "two.sided", exact = NULL, conf.int = FALSE)




#1985 onward


# Per-turtle longevity (all turtles first)
turtle_longevity_85 <- filtered_df %>%
  group_by(OriginalTagID) %>%
  summarise(
    first_year = min(year(DateOfActivity), na.rm = TRUE),
    last_year  = max(year(DateOfActivity), na.rm = TRUE),
    longevity  = last_year - first_year,
    .groups = "drop"
  )

# Keep only turtles that began nesting in 1985 onward and longevity over or = to 3
turtle_longevity_1985on <- turtle_longevity_85 %>%
  dplyr::filter(first_year >= 1985 & longevity >= 3)


# Summary statistics for those turtles
longevity_summary_1985on <- turtle_longevity_1985on %>%
  summarise(
    mean_longevity   = mean(longevity, na.rm = TRUE),
    sd_longevity     = sd(longevity, na.rm = TRUE),
    median_longevity = median(longevity, na.rm = TRUE),
    max_longevity    = max(longevity, na.rm = TRUE),
    n_turtles        = n()
  )

hist(turtle_longevity_1985on$longevity,
     main = "",
     xlab = "Reproductive Longevity (years)",
     ylab = "Turtles",
     col = "lightblue",
     border = "black"
)


mean_longevity85 <- longevity_summary_1985on$mean_longevity
abline(v = mean_longevity85, lwd = 2, lty = 2)


#1992 onward

# Per-turtle longevity (all turtles first)
turtle_longevity_92 <- filtered_df %>%
  group_by(OriginalTagID) %>%
  summarise(
    first_year = min(year(DateOfActivity), na.rm = TRUE),
    last_year  = max(year(DateOfActivity), na.rm = TRUE),
    longevity  = last_year - first_year,
    .groups = "drop"
  )

# Keep only turtles that began nesting in 1992 onward and longevity over or = to 3
turtle_longevity_1992on <- turtle_longevity_92 %>%
  dplyr::filter(first_year >= 1992 & longevity >= 3)


# Summary statistics for those turtles
longevity_summary_1992on <- turtle_longevity_1992on %>%
  summarise(
    mean_longevity   = mean(longevity, na.rm = TRUE),
    sd_longevity     = sd(longevity, na.rm = TRUE),
    median_longevity = median(longevity, na.rm = TRUE),
    max_longevity    = max(longevity, na.rm = TRUE),
    n_turtles        = n()
  )

hist(turtle_longevity_1992on$longevity,
     main = "",
     xlab = "Reproductive Longevity (years)",
     ylab = "Turtles",
     col = "lightblue",
     border = "black"
)

mean_longevity92 <- longevity_summary_1992on$mean_longevity
abline(v = mean_longevity92, lwd = 2, lty = 2)
############################################################################

# t-test
# need to redo and check for normality



t_test_result <- t.test(turtle_longevity_overthree$longevity, mu = 8)

# Print results
print(t_test_result)

shapiro.test(turtle_longevity_overthree$longevity) # p > 0.05 suggests normality [2]


