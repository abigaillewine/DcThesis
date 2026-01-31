

#SPNWR_ID is the ID for the individual event
#Original Tag ID is the ID for the individual turtle


library(readr)
library(tidyverse)


Excavation_2011 <- read_csv("~/R/Projects/Thesis/data/ExcavationData_2011_2025.csv")
TurtleID_2011 <- read_csv("~/R/Projects/Thesis/data/TurtleID_2011_2025.csv")


Full_Clutch_2011 <- full_join(Excavation_2011, TurtleID_2011, by = "SPNWR_ID")

str(Full_Clutch_2011)


#keep only columns you need

clean_clutch_2011 <- select(Full_Clutch_2011,SPNWR_ID, 'Total unhatched.x', 'Total eggs.x', 'Hatch success (%).x','OriginalTagID')

clean_clutch_2010 <- select(Excavation_2010,SPNWR_ID, 'Total unhatched', 'Total eggs', 'Hatch success (%)','OriginalTagID')


#rename columns

clutch_2011_renamed <- clean_clutch_2011 %>%
  rename(Unhatched = 'Total unhatched.x',
         TotalEggs = 'Total eggs.x',
         HatchSuccess ='Hatch success (%).x')


clutch_2010_renamed <- clean_clutch_2010 %>%
  rename(Unhatched = 'Total unhatched',
         TotalEggs = 'Total eggs',
         HatchSuccess ='Hatch success (%)')

#combine into one data frame
Total_Clutch_NA <- rbind(clutch_2010_renamed, clutch_2011_renamed)

Total_Clutch <- na.omit(Total_Clutch_NA)


#save as an excel to then double check and fix IDs





#body size
  #match nest to body size of the turtle at that nesting event

#remigration interval
