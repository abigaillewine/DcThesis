

#SPNWR_ID is the ID for the individual event
#Original Tag ID is the ID for the individual turtle


library(readr)
library(tidyverse)
library(writexl)
library(lmtest)


Excavation_2011 <- read_csv("~/R/Projects/Thesis/data/ExcavationData_2011_2025.csv")
TurtleID_2011 <- read_csv("~/R/Projects/Thesis/data/TurtleID_2011_2025.csv")
Excavation_2010 <- read_csv("~/R/Projects/Thesis/data/Excavation_2010.csv")
DcDataRemoved <- read_csv("~/R/Projects/Thesis/data/DcDataRemoved.csv")

problems(Excavation_2010)

Full_Clutch_2011 <- full_join(Excavation_2011, TurtleID_2011, by = "SPNWR_ID")

str(Full_Clutch_2011)


#keep only columns you need

clean_clutch_2011 <- select(Full_Clutch_2011,SPNWR_ID, 'Total unhatched.x', 'Total eggs.x', 'Hatch success (%).x','OriginalTagID','Date')

clean_clutch_2010 <- select(Excavation_2010,SPNWR_ID, 'Total unhatched', 'Total eggs', 'Hatch success (%)','OriginalTagID','DateOfActivity')


#rename columns

clutch_2011_renamed <- clean_clutch_2011 %>%
  rename(Unhatched = 'Total unhatched.x',
         TotalEggs = 'Total eggs.x',
         HatchSuccess ='Hatch success (%).x',
         Date = 'Date')


clutch_2010_renamed <- clean_clutch_2010 %>%
  rename(Unhatched = 'Total unhatched',
         TotalEggs = 'Total eggs',
         HatchSuccess ='Hatch success (%)',
         Date = 'DateOfActivity')

#combine into one data frame
Total_Clutch_NA <- rbind(clutch_2010_renamed, clutch_2011_renamed)%>%  mutate(Date = mdy(Date),  # convert to Date type
         month = month(Date),
         day   = day(Date),
         year  = year(Date))



Total_Clutch <- na.omit(Total_Clutch_NA)


#save as an excel to then double check and fix IDs
write_xlsx(Total_Clutch, "~/R/Projects/Thesis/data/Total_Clutch.xlsx")




#combined dc and clutch data using both SPNWR ID and Tag ID???

#try using Tag ID and Year

#body size
  #match nest to body size of the turtle at that nesting event






# clean rows so we are only looking at the nest lays
clean_df <- DcDataRemoved %>%
  mutate(results_clean = str_to_lower(str_trim(Result))) %>%
  filter(results_clean %in% c("relocate", "lay", "probable lay", "in situ")) %>%
  select(-results_clean) %>% rename(Date = DateOfActivity)


#separate dates into individual columns

DcDates <- clean_df %>%
  mutate(Date = mdy(Date),  # convert to Date type
         month = month(Date),
         day   = day(Date),
         year  = year(Date))


#remove values +/- 1 cm different in same yr

filtered_df <- DcDates %>%
  mutate(Year = year(Date)) %>%
  group_by(OriginalTagID, Year) %>%
  arrange(Date) %>%
  mutate(first_CCL = first(CurvedCarapaceLength_NtoN)) %>%
  filter(abs(CurvedCarapaceLength_NtoN - first_CCL) <= 1) %>%
  ungroup()


# join dataframes

FullData <- Total_Clutch %>%
  left_join(filtered_df %>% 
              select(OriginalTagID, SPNWR_ID, CurvedCarapaceLength_NtoN),
            by = c("OriginalTagID", "SPNWR_ID"))



#linear model body+hatch


lm_hatch_bodysize <- lm(HatchSuccess ~ CurvedCarapaceLength_NtoN, data = FullData)

# View summary
summary(lm_hatch_bodysize)


# 1. Residuals vs Fitted (linearity + homoscedasticity)
plot(lm_hatch_bodysize, which = 1)

# 2. Normal Q-Q plot (normality of residuals)
plot(lm_hatch_bodysize, which = 2)

# 3. Scale-Location plot (spread of residuals vs fitted, homoscedasticity)
plot(lm_hatch_bodysize, which = 3)

# 4. Residuals vs Leverage (influential points)
plot(lm_hatch_bodysize, which = 5)



bptest(lm_hatch_bodysize)

shapiro.test(resid(lm_hatch_bodysize))

lm_hatch_bodysize <- lm(HatchSuccess ~ CurvedCarapaceLength_NtoN, 
                        data = FullData)

ggplot(FullData, aes(x = CurvedCarapaceLength_NtoN, 
                     y = HatchSuccess)) +
  
  geom_point(alpha = 0.5) +
  
  geom_smooth(method = "lm",
              color = "black",
              fill = "gray",
              linewidth = 1.3,
              se = TRUE) +
  
  labs(x = "Curved Carapace Length (cm)",
       y = "Hatch Success") +
  
  theme_classic()



#linear model body+clutch


lm_clutch_bodysize <- lm(TotalEggs ~ CurvedCarapaceLength_NtoN, data = FullData)

# View summary
summary(lm_clutch_bodysize)


# 1. Residuals vs Fitted (linearity + homoscedasticity)
plot(lm_clutch_bodysize, which = 1)

# 2. Normal Q-Q plot (normality of residuals)
plot(lm_clutch_bodysize, which = 2)

# 3. Scale-Location plot (spread of residuals vs fitted, homoscedasticity)
plot(lm_clutch_bodysize, which = 3)

# 4. Residuals vs Leverage (influential points)
plot(lm_clutch_bodysize, which = 5)



bptest(lm_clutch_bodysize)

shapiro.test(resid(lm_clutch_bodysize))


ggplot(FullData, aes(x = CurvedCarapaceLength_NtoN, 
                     y = TotalEggs)) +
  
  geom_point(alpha = 0.5) +
  
  geom_smooth(method = "lm",
              color = "black",
              fill = "gray",
              linewidth = 1.3,
              se = TRUE) +
  
  labs(x = "Curved Carapace Length (cm)",
       y = "Clutch Size") +
  
  theme_classic()


###########################################

#remigration interval

remigration_full <- FullData %>%  arrange(OriginalTagID, year) %>% group_by(OriginalTagID) %>%
  mutate(
    prev_year = lag(year),
    remigration_interval = year - prev_year
  ) %>% na.omit



#linear model body+hatch


lm_hatch_remi <- lm(HatchSuccess ~ remigration_interval, data = remigration_full)

# View summary
summary(lm_hatch_remi)


# 1. Residuals vs Fitted (linearity + homoscedasticity)
plot(lm_hatch_remi, which = 1)

# 2. Normal Q-Q plot (normality of residuals)
plot(lm_hatch_remi, which = 2)

# 3. Scale-Location plot (spread of residuals vs fitted, homoscedasticity)
plot(lm_hatch_remi, which = 3)

# 4. Residuals vs Leverage (influential points)
plot(lm_hatch_remi, which = 5)



bptest(lm_hatch_remi)

shapiro.test(resid(lm_hatch_remi))


ggplot(remigration_full, aes(x = remigration_interval, 
                     y = HatchSuccess)) +
  
  geom_point(alpha = 0.5) +
  
  geom_smooth(method = "lm",
              color = "black",
              fill = "gray",
              linewidth = 1.3,
              se = TRUE) +
  
  labs(x = "remigration_interval",
       y = "Hatch Success") +
  
  theme_classic()



#linear model body+clutch


lm_clutch_remi <- lm(TotalEggs ~ remigration_interval, data = remigration_full)

# View summary
summary(lm_clutch_remi)


# 1. Residuals vs Fitted (linearity + homoscedasticity)
plot(lm_clutch_remi, which = 1)

# 2. Normal Q-Q plot (normality of residuals)
plot(lm_clutch_remi, which = 2)

# 3. Scale-Location plot (spread of residuals vs fitted, homoscedasticity)
plot(lm_clutch_remi, which = 3)

# 4. Residuals vs Leverage (influential points)
plot(lm_clutch_remi, which = 5)



bptest(lm_clutch_remi)

shapiro.test(resid(lm_clutch_remi))


ggplot(remigration_full, aes(x = remigration_interval, 
                     y = TotalEggs)) +
  
  geom_point(alpha = 0.5) +
  
  geom_smooth(method = "lm",
              color = "black",
              fill = "gray",
              linewidth = 1.3,
              se = TRUE) +
  
  labs(x = "remigration_interval",
       y = "Clutch Size") +
  
  theme_classic()



