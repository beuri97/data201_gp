# Load packages such as ggplot2, dplyr, tidyr, and readr to be able to use specialised functions for creating
# visualisations, reading, writing, and manipulating data.
library(tidyverse)

# Load the readxl package to be able to use a function to read Excel files.
library(readxl)

# Load skimr package to be able to use a function to understand the structure of the dataframe we will analyse
library(skimr)

# Load the knitr package to be able to use a function for presenting information in a tidy format.
library(knitr)

# Load the visdat package to be able to use a function for visualisation of the data. 
library(visdat)

# Load the lubridate package to be able to use function(s) for manipulating datetime data type.
library(lubridate)

# Read the groundwq_2004-2020.xlsx and store it as groundwq for analysis.
groundwq <- "groundwq_2004-2020.xlsx" %>% 
  read_excel()

# Gives an overview of groundwq such as columns, data types, the possible values, number of rows and columns.
groundwq %>% 
  glimpse()

# Reads the entirety of groundwq and creates a plot to check if it contains missing data (NA).
groundwq %>% 
  vis_miss(warn_large_data = FALSE)

# Takes the groundwq modify and rename CensoredValue, Date, and Units, select the relevant columns 
# and rows, group the rows by Region, Indicator, and Year then create an new data frame containing the sum of
# the CensoredValue rounded off to 2.
new_groundwq <- groundwq %>% 
  mutate(CensoredValue = ifelse(is.na(CensoredValue), NA_integer_, CensoredValue),
         Year = year(Date),
         Units = case_when(Indicator == "E.coli" ~ "cfu/100ml", TRUE ~ "g/m3"),
         Region = case_when(Region == "Hawkes Bay" ~ "Hawke's Bay", TRUE ~ Region)) %>% 
  select(Region, Indicator, Units, Year, CensoredValue) %>% 
  filter(Indicator %in% c("E.coli", "Nitrate nitrogen"), Year >= 2002, Year <= 2019) %>% 
  group_by(Region, Indicator, Year) %>% 
  na.omit() %>% 
  summarise(Total_MedVal = round(mean(CensoredValue), 2)) %>% 
  distinct()

# Takes the new_groundwq then convert it to wide format.
groundwater_quality <- new_groundwq %>% 
  spread(key = Indicator,
         value = Total_MedVal) %>% 
  mutate(`E.coli (cfu/100ml)` = E.coli, `Nitrate nitrogen (g/m3)` = `Nitrate nitrogen`) %>% 
  select(-c("E.coli", "Nitrate nitrogen"))
groundwater_quality

# Load 'new_river_ecoli.csv' data
river_ecoli <- "new_river_ecoli.csv" %>% 
  read_csv()

# Gives an overview of river_ecoli such as columns, data types, the possible values, number of rows and columns.
river_ecoli %>% 
  glimpse()

# Reads the entirety of river_ecoli and creates a plot to check if it contains missing data (NA).
river_ecoli %>% 
  vis_miss()

# Select and rename the columns we need.
river_ecoli <- river_ecoli %>% 
  rename(Region = state, Year = end_year, Indicator = measure, Median = median, 
         Units = units) %>% 
  select(Region, Year, Indicator, Median, Units)

# Check for missing data again (NA)
river_ecoli %>% 
  vis_miss()

# Takes the river_ecoli group the rows by Region, Indicator, and Year then create an new data frame containing the 
# sum of the Median rounded off to 2.
new_riverecoli <- river_ecoli %>%
  filter(Year >= 2002, Year <= 2019) %>% 
  group_by(Region, Year, Indicator) %>% 
  summarise(Total_MedVal = round(mean(Median), 2)) %>% 
  unique()

# Wide format data set spread by Region as key
riverecoli_wide <- new_riverecoli %>% 
  spread(key = Indicator,
         value = Total_MedVal) %>% 
  mutate(`E.coli (cfu/100ml)` = `E. coli`) %>% 
  select(-"E. coli")

# Read the new_river_nitrogen.csv and store it as river_nitrogen for analysis.
river_nitrogen <- "new_river_nitrogen.csv" %>% 
  read_csv()

# Gives an overview of river_nitrogen such as columns, data types, the possible values, number of rows and columns.
# This allow us to select which relevant columns to select.
river_nitrogen %>% 
  glimpse()

# Select the relevant columns for analysis.
river_nitrogen <- river_nitrogen %>% 
  select(measure, units, median, end_year, state)

# Reads the entirety of river_nitrogen and creates a plot to check if it contains missing data (NA). 
river_nitrogen %>% 
  vis_miss()

# Takes the river_nitrogen data frame then rename the columns, select the necessary rows, group them
# by Region, Indicator, and Year. Lastly, summarise them by getting the sum of the median values rounded
# off by 2 s.f.
new_rivernitrogen <- river_nitrogen %>% 
  rename(Region = state, Indicator = measure, Units = units, Med_Value = median,
         Year = end_year) %>% 
  filter(Indicator %in% c("Ammoniacal nitrogen", "Nitrate-nitrite nitrogen"),
         Year >= 2002, Year <= 2019) %>% 
  group_by(Region, Indicator, Year) %>% 
  summarise(Total_MedVal = round(mean(Med_Value), 2)) %>% 
  distinct()

# Takes the new_rivernitrogen then convert it to wide format.
rivernitrogen_wide <- new_rivernitrogen %>% 
  spread(key = Indicator,
         value = Total_MedVal) %>% 
  mutate(`Ammoniacal nitrogen (g/m3)` = `Ammoniacal nitrogen`,
         `Nitrate-nitrite nitrogen (g/m3)` = `Nitrate-nitrite nitrogen`) %>% 
  select(-c(`Ammoniacal nitrogen`, `Nitrate-nitrite nitrogen`))
rivernitrogen_wide

# Join new_rivernitrogen and new_riverecoli to create river_quality dataframe.
river_quality <- rivernitrogen_wide %>% 
  full_join(riverecoli_wide)

# Generate csv file
write_csv(river_quality, "river_quality.csv")
write_csv(groundwater_quality, "groundwater_quality.csv")


# Prototype Line Plot
my_df <- river_quality %>% 
  group_by(Year) %>% 
  summarise(`NO3-N (g/m3)` = mean(`Nitrate-nitrite nitrogen (g/m3)`), 
            `NH3-N (g/m3)` = mean(`Ammoniacal nitrogen (g/m3)` %>% na.omit()),
            `E.coli (cfu/100ml)` = mean(`E.coli (cfu/100ml)` %>% na.omit()))

NO3<-ggplot(my_df %>% select(Year, `NO3-N (g/m3)`), aes(x = Year, y = `NO3-N (g/m3)`)) +
  geom_line()+
  geom_point()

NO3
NH3 <- ggplot(my_df %>% select(Year, `NH3-N (g/m3)`), aes(x = Year, y = `NH3-N (g/m3)`)) +
  geom_line()+
  geom_point()
NH3

E_coli <- ggplot(my_df %>% select(Year, `E.coli (cfu/100ml)`), aes(x = Year, y = `E.coli (cfu/100ml)`)) +
  geom_line()+
  geom_point()
E_coli
