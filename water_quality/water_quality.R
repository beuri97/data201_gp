# Load packages such as ggplot2, dplyr, tidyr, and readr to be able to use specialised functions for creating
# visualisations, reading, writing, and manipulating data.
library(tidyverse)

# Load the tidygeocoder package to be able to use a function to convert the given latitude and longitude
# to address.
library(tidygeocoder)

# Load the readxl package to be able to use a function to read Excel files.
library(readxl)

# Load skimr package to be able to use a function to understand the structure of the dataframe we will analyse
library(skimr)

# Load the knitr package to be able to use a function for presenting information in a tidy format.
library(knitr)

# Load the visdat  
library(visdat)

library(lubridate)

# Read the groundwq_2004-2020.xlsx and store it as groundwq for analysis.
groundwq <- "groundwq_2004-2020.xlsx" %>% 
  read_excel()

# Gives an overview of groundwq such as columns, data types, the possible values, number of rows and columns.
groundwq %>% 
  glimpse()

# The following lines of code show the entirety of the summary statistics of the whole groundwq.
# The skim() provides a detailed overview of the dataframe.
# The select() choose the columns specified.
# The kable() creates and presents the overview of the dataframe in a table format.
groundwq %>% 
  skim() %>% 
  select(1:7) %>% 
  kable()

groundwq %>% 
  skim() %>% 
  select(8:14) %>% 
  kable()

groundwq %>% 
  skim() %>% 
  select(15:21) %>% 
  kable()

# Reads the entirety of groundwq and creates a plot to check if it contains missing data (NA).
groundwq %>% 
  vis_miss(warn_large_data = FALSE)

# Takes the groundwq modify CensoredValue and create a new variable called Year, select the relevant columns 
# and rows, group the rows by Region, Indicator, and Year then create an new data frame containing the sum of
# the CensoredValue rounded off to 2.

new_groundwq <- groundwq %>% 
  mutate(CensoredValue = ifelse(is.na(CensoredValue), NA_integer_, CensoredValue),
         Year = year(Date),
         Units = case_when(Indicator == "E.coli" ~ "cfu/100ml", TRUE ~ "g/m3")) %>% 
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

# Reads the entirety of river_ecoli and creates a plot to check if it contains missing data (NA).
river_ecoli %>% 
  vis_miss()

# Select columns we need and discard rest of them.
new_river_ecoli <- river_ecoli %>% 
  select(Region = state, Year = end_year, Measure = measure, Median = median, 
         Units = units)

# Check any NAs present in this data set.
new_river_ecoli %>% 
  vis_miss()

# Wide format data set spread by Region as key
new_riverecoli <- new_river_ecoli %>% 
  group_by(Region, Year) %>% 
  summarise(Indicator = Measure, Total_MedVal = sum(round(Median,2)), Units) %>% 
  unique()

new_riverecoli %>% 
  spread(key = Region,
         value = Total_MedVal)

# Wide format data set spread by Indicator as key
new2 <- new_river_ecoli %>% 
  group_by(Region, Year) %>% 
  summarise(Indicator = Measure, Value = sum(round(Median,2))) %>% 
  unique() %>% 
  filter(Year >= 2002, Year <= 2019) %>% 
  spread(key = Indicator,
         value = Value)


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

# The following lines of code show the entirety of the summary statistics of the whole river_nitrogen.
# The skim() provides a detailed overview of the dataframe.
# The select() choose the columns specified.
# The kable() creates and presents the overview of the dataframe in a table format.
river_nitrogen %>% 
  skim() %>% 
  select(1:7) %>% 
  kable()

river_nitrogen %>% 
  skim() %>% 
  select(8:14) %>% 
  kable()

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

add_col <- function(base_data, new_data) {
  new_df <- cbind(base_data, new_data)
  return(new_df)
}

river_quality <- new_rivernitrogen %>% 
  full_join(new_riverecoli)

groundwq_categ <- tibble(Water_Categ = rep(c("Groundwater Quality"), each = nrow(new_groundwq)))
river_categ <- tibble(Water_Categ = rep(c("River Quality"), each = nrow(river_quality)))

groundwq_with_categ <- add_col(new_groundwq, groundwq_categ)
riverq_with_categ <- add_col(river_quality, river_categ)

water_quality <- groundwq_with_categ %>% 
  full_join(riverq_with_categ) %>% 
  mutate(Indicator = case_when(Indicator == "E. coli" ~ "E.coli", TRUE ~ Indicator))

# # Takes the river_ecoli dataset then take the lat and long variables 
# # to get the full address. Then save it as new_riverecoli.
# new_riverecoli <- river_ecoli %>% 
#   reverse_geocode(lat = lat, long = long,
#                   method = "osm", full_results = TRUE)
# 
# # Takes the river_ecoli dataset then take the lat and long variables 
# # to get the full address. Then save it as new_rivernitrogen.
# new_rivernitrogen <- river_nitrogen %>% 
#   reverse_geocode(lat = lat, long = long,
#                   method = "osm", full_results = TRUE)
# 
# # Write the new dataset as CSVs for use.
# write_csv(new_riverecoli, "new_river_ecoli.csv")
# write_csv(new_rivernitrogen, "new_river_nitrogen.csv")

water_quality %>% 
  filter(Indicator != "E.coli", Water_Categ == "Groundwater Quality", Region == "Auckland") %>% 
  ggplot(aes(x = Year, y = Total_MedVal)) +
  geom_line() +
  xlim(2007, 2019) +
  theme_bw()


water_quality %>% 
  filter(Indicator %in% c("Nitrate nitrogen")) %>% 
  ggplot(mapping=aes(x = Region, y = Total_MedVal)) + 
  geom_boxplot()

df <- groundwq %>% 
  filter(Indicator == "E.coli", Region == "Bay of Plenty")

df1 <- water_quality %>% 
  filter(Indicator == "E.coli", Water_Categ == "Groundwater Quality")
  

