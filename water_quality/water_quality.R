# Load packages such as ggplot2, dplyr, tidyr, and readr to be able to use specialised functions for creating
# visualisations, reading, writing, and manipulating data.
library(tidyverse)

# Load the tidygeocoder package to be able to use a function to convert the given latitude and longitude
# to address.
library(tidygeocoder)

# Load the readxl package to be able to use a function to read Excel files.
library(readxl)

library(skimr)

library(knitr)

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
# the kable() creates and presents the overview of the dataframe in a table format.
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
         Year = year(Date)) %>% 
  select(Region, Indicator, Units, Year, CensoredValue) %>% 
  filter(Indicator %in% c("E.coli", "Nitrate nitrogen")) %>% 
  group_by(Region, Indicator, Year) %>% 
  summarise(Value = sum(round(CensoredValue,2)))

# Takes the new_groundwq then convert it to wide format.
groundwq_wide <- new_groundwq %>% 
  spread(key = Indicator,
         value = Value)
groundwq_wide


river_ecoli <- read_csv("new_river_ecoli.csv")

# Read the new_river_nitrogen.csv and store it as river_nitrogen for analysis.
river_nitrogen <- "new_river_nitrogen.csv" %>% 
  read_csv()

# # takes the river_ecoli dataset then take the lat and long variables 
# # to get the full address. Then save it as new_riverecoli.
# new_riverecoli <- river_ecoli %>% 
#   reverse_geocode(lat = lat, long = long,
#                   method = "osm", full_results = TRUE)
# 
# # takes the river_ecoli dataset then take the lat and long variables 
# # to get the full address. Then save it as new_rivernitrogen.
# new_rivernitrogen <- river_nitrogen %>% 
#   reverse_geocode(lat = lat, long = long,
#                   method = "osm", full_results = TRUE)
# 
# # write the new dataset as CSVs for use
# write_csv(new_riverecoli, "new_river_ecoli.csv")
# write_csv(new_rivernitrogen, "new_river_nitrogen.csv")