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
         Region = case_when(Region == "Hawkes Bay" ~ "Hawke's Bay",
                            Region == "Manawatu-Whanganui" ~ "Manawatū-Whanganui",
                            TRUE ~ Region)) %>% 
  select(Region, Indicator, Units, Year, CensoredValue) %>% 
  filter(Indicator %in% c("E.coli", "Nitrate nitrogen"), Year >= 2002, Year <= 2019) %>% 
  group_by(Region, Indicator, Year) %>% 
  na.omit() %>% 
  summarise(Total_MedVal = round(sum(CensoredValue), 2), Units) %>% 
  distinct()

# Takes the new_groundwq then convert it to wide format.
groundwq_wide <- new_groundwq %>% 
  spread(key = Region,
         value = Total_MedVal)
groundwq_wide

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
  summarise(Total_MedVal = round(sum(Median), 2), Units) %>% 
  unique()

# Wide format data set spread by Region as key
new_riverecoli %>% 
  spread(key = Region,
         value = Total_MedVal)

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
  summarise(Total_MedVal = round(sum(Med_Value), 2), Units) %>% 
  distinct()

# Takes the new_rivernitrogen then convert it to wide format.
rivernitrogen_wide <- new_rivernitrogen %>% 
  spread(key = Indicator,
         value = Total_MedVal)
rivernitrogen_wide

# Join new_rivernitrogen and new_riverecoli to create river_quality dataframe.
river_quality <- new_rivernitrogen %>% 
  full_join(new_riverecoli)

# Merge the tibble of categories with the existing groundwq and river_quality dataframes.
groundwq_categ <- tibble(Water_Categ = rep(c("Groundwater Quality"), each = nrow(new_groundwq)))
river_categ <- tibble(Water_Categ = rep(c("River Quality"), each = nrow(river_quality)))

final_groundwq <- cbind(new_groundwq, groundwq_categ)
final_riverq <- cbind(river_quality, river_categ)

# Join the groundwq and river_quality to create water quality dataframe.
# Normalise the indicator for all E.coli observation.
water_quality <- final_groundwq %>% 
  full_join(final_riverq) %>% 
  mutate(Indicator = case_when(Indicator == "E. coli" ~ "E.coli", 
                               Indicator == "Nitrate nitrogen" ~ "Nitrate-nitrite nitrogen",
                               TRUE ~ Indicator))

water_quality %>% 
  vis_miss()

write_csv(water_quality, "water_quality.csv")
