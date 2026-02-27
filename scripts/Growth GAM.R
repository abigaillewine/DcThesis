#Growth GAMs


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


valid <- filtered_df %>%
  filter(!is.na(CurvedCarapaceLength_NtoN)) %>%
  mutate(Year = year(DateOfActivity))

# ---------- 2. Mean size per turtle per season ----------
season_means <- valid %>%
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




growth_summary_full <- growth_rates %>%
  summarise(
    min_growth    = min(growth_rate, na.rm = TRUE),
    mean_growth   = mean(growth_rate, na.rm = TRUE),
    sd_growth =sd(growth_rate, na.rm = TRUE),
    median_growth = median(growth_rate, na.rm = TRUE),
    max_growth    = max(growth_rate, na.rm = TRUE)
  )




hist(growth_rates$growth_rate[!is.na(growth_rates$growth_rate)],
     main = "Frequency Distribution of Leatherback Growth Rates",
     xlab = "Post Maturation Growth rate (cm yr⁻¹)",
     ylab = "Turtles",
     col = "lightpink",
     border = "black")


# ---------- 4. Calculate reproductive longevity ----------
repro_years <- filtered_df %>%
  group_by(OriginalTagID) %>%
  summarise(
    years_reproducing =
      max(year(DateOfActivity), na.rm = TRUE) -
      min(year(DateOfActivity), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    longevity_group = case_when(
      years_reproducing <= 10 ~ "1-10",
      years_reproducing > 10 & years_reproducing <= 20 ~ "11-20",
      years_reproducing > 20 ~ "20+"
    )
  )

# ---------- 5. Attach longevity group to growth data ----------
growth_grouped <- growth_rates %>%
  left_join(repro_years %>% select(OriginalTagID, longevity_group),
            by = "OriginalTagID")

# ensure plotting order
growth_grouped$longevity_group <- factor(
  growth_grouped$longevity_group,
  levels = c("1-10", "11-20", "20+")
)


growth_summary <- growth_grouped %>%
  group_by(longevity_group) %>%
  summarise(
    n = n(),
    mean_growth = mean(growth_rate, na.rm = TRUE),
    sd_growth = sd(growth_rate, na.rm = TRUE),
    .groups = "drop"
  )

growth_summary



#GAMS
growth_gam_data <- growth_rates %>%
  left_join(repro_years %>% select(OriginalTagID, years_reproducing),
            by = "OriginalTagID") %>%
  filter(!is.na(prev_size),
         !is.na(remigration_interval),
         !is.na(years_reproducing))

#Growth vs Size 
gam_size <- gam(growth_rate ~ s(prev_size, k = 5),
                data = growth_gam_data,
                method = "REML")
summary(gam_size)


#growth vs remigration
gam_interval <- gam(growth_rate ~ s(remigration_interval, k = 5),
                    data = growth_gam_data,
                    method = "REML")
summary(gam_interval)


#Growth vs Repro long
gam_longevity <- gam(growth_rate ~ s(years_reproducing, k = 5),
                     data = growth_gam_data,
                     method = "REML")
summary(gam_longevity)


plot_gam <- function(model, varname, xlab){
  
  newdat <- data.frame(
    x = seq(min(model$model[[varname]], na.rm=TRUE),
            max(model$model[[varname]], na.rm=TRUE),
            length.out = 200)
  )
  
  names(newdat) <- varname
  
  preds <- predict(model, newdat, se.fit = TRUE)
  
  newdat$fit <- preds$fit
  newdat$upper <- preds$fit + 2*preds$se.fit
  newdat$lower <- preds$fit - 2*preds$se.fit
  
  ggplot(newdat, aes_string(x = varname, y = "fit")) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25) +
    labs(x = xlab,
         y = "Growth rate (cm yr⁻¹)") +
    theme_classic(base_size = 14)
}



plot_gam(gam_size, "prev_size", "Curved Carapace Length (cm)")
plot_gam(gam_interval, "remigration_interval", "Remigration interval (years)")
plot_gam(gam_longevity, "years_reproducing", "Reproductive longevity (years)")




library(ggplot2)
library(gratia)

# Draw GAM smooth for years_reproducing only
gam_longevity_fig <- draw(gam_longevity,
                          select = "s(years_reproducing)",  # only this smooth
                          residuals = FALSE,
                          rug = TRUE,
                          scales = "free",
                          colour = "#7570b3",  # purple
                          size = 1.5           # thicker line
) +
  labs(
    title = "",
    y = "Reproductive longevity (years)",
    x = "Years reproducing"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

gam_longevity_fig


library(gratia)
library(ggplot2)

# Extract smooth data for 'years_reproducing'
smooth_data <- smooth_estimates(
  gam_longevity,
  select = "years_reproducing",
  partial_match = TRUE
)

# Compute confidence intervals manually
smooth_data <- smooth_data %>%
  mutate(
    lower = est - 2 * se,
    upper = est + 2 * se
  )

names(smooth_data)



# Plot
gam_longevity_fig <- ggplot(smooth_data, aes(x = years_reproducing, y = est)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lavender", alpha = 0.3) +  # CI band
  geom_line(color = "#7570b3", size = 1.5) +  # smooth line
  geom_rug(data = gam_longevity$model, aes(x = years_reproducing), inherit.aes = FALSE, alpha = 0.2) +  # data points
  labs(
    x = "Years reproducing",
    y = "Reproductive longevity (years)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

gam_longevity_fig
###################################################################3
#Growth vs Repro long

# Get smooth estimates from the GAM
smooth_data <- smooth_estimates(
  gam_longevity,
  select = "years_reproducing",
  partial_match = TRUE
)

# Compute confidence intervals
smooth_data <- smooth_data %>%
  mutate(
    lower = .estimate - 2 * .se,
    upper = .estimate + 2 * .se
  )

# Plot
gam_longevity_fig <- ggplot(smooth_data, aes(x = years_reproducing, y = .estimate)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lavender", alpha = 0.3) +  # CI band
  geom_line(color = "#7570b3", size = 1.5) +  # smooth line
  geom_point(data = gam_longevity$model,
             aes(x = years_reproducing, y = growth_rate),
             inherit.aes = FALSE, color = "darkred", alpha = 0.6, size = 2) +  # individual points
  labs(
    x = "Reproductive Longevity (year)",
    y = "Post Maturation Growth rate (cm yr⁻¹)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

gam_longevity_fig

################################
# Residuals normality
resid_gam <- residuals(gam_longevity)

# Histogram
hist(resid_gam, main = "Residuals of GAM", xlab = "Residuals", col = "lightblue", border = "black")

# Q-Q plot
qqnorm(resid_gam)
qqline(resid_gam, col = "red")

# Shapiro-Wilk test (formal test)
shapiro.test(resid_gam)

#Homoscedasticity
# Residuals vs fitted
plot(gam_longevity, residuals = TRUE, pch = 16, cex = 0.7)

# Histogram
hist(resid_gam, col = "lightblue", main = "Residuals", xlab = "Residual value")

# Q-Q plot
qqnorm(resid_gam)
qqline(resid_gam, col = "red")

###################################################################3
####################################################################

#Combined GAM

gam_full <- gam(growth_rate ~
                  s(prev_size, k=5) +
                  s(remigration_interval, k=5) +
                  s(years_reproducing, k=5),
                data = growth_gam_data,
                method = "REML")

summary(gam_full)

gam_fig <- draw(gam_full,
                residuals = FALSE,
                ncol = 3) +
  labs(y = "Effect on growth rate (cm yr⁻¹)") +
  theme_classic(base_size = 14)

gam_fig




library(ggplot2)
library(gratia)

# Draw GAM smooths
gam_fig <- draw(gam_full,
                residuals = FALSE,   # Hide residuals
                ncol = 3,            # Arrange in 3 columns
                rug = TRUE,           # optional: show data points
                scales = "free") +    # allow axes to adjust automatically
  labs(
    y = "Effect on growth rate (cm yr⁻¹)",
    x = NULL               # remove x-axis label to avoid clutter
  ) +
  theme_classic(base_size = 14) + # clean background
  theme(
    strip.background = element_blank(),  # remove gray behind facet labels
    strip.text = element_text(face = "bold", size = 12),  # bold facet labels
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "gray90"),  # subtle grid lines
    panel.grid.minor = element_blank()
  )

# Show figure
gam_fig


library(gratia)
library(ggplot2)

# Draw GAM smooths with thicker lines and a single color palette
gam_fig2 <- draw(gam_full,
                residuals = FALSE,
                ncol = 3,
                rug = TRUE,
                scales = "free",
                colour = "#1b9e77",  # teal color for all smooths
                size = 1.5            # thicker lines
) +
  labs(
    y = "Effect on growth rate (cm yr⁻¹)",
    x = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12),
    axis.title.x = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )

gam_fig2





#linear model

# Fit a linear model instead of a GAM
lm_longevity <- lm(growth_rate ~ years_reproducing, data = growth_gam_data)

# View summary
summary(lm_longevity)


# Fit the linear model (if not already done)
lm_longevity <- lm(growth_rate ~ years_reproducing, data = growth_gam_data)

# 1. Residuals vs Fitted (linearity + homoscedasticity)
plot(lm_longevity, which = 1)

# 2. Normal Q-Q plot (normality of residuals)
plot(lm_longevity, which = 2)

# 3. Scale-Location plot (spread of residuals vs fitted, homoscedasticity)
plot(lm_longevity, which = 3)

# 4. Residuals vs Leverage (influential points)
plot(lm_longevity, which = 5)


library(lmtest)
bptest(lm_longevity)

shapiro.test(resid(lm_longevity))




#######################################################
#
#linear mixed effects model

library(lme4)

lmm_longevity <- lmer(
  growth_rate ~ years_reproducing +
    (1 | OriginalTagID),
  data = growth_gam_data
)

summary(lmm_longevity)




resid_vals  <- resid(lmm_longevity)
fitted_vals <- fitted(lmm_longevity)


