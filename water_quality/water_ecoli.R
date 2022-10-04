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

# Load 'new_river_ecoli.csv' data
river_ecoli <- read_csv("new_river_ecoli.csv")

river_ecoli %>% 
  vis_miss()

# Select columns we need and discard rest of them.
new_river_ecoli <- river_ecoli %>% 
  select(Region = state, Year = end_year, Measure = measure, Median = median, 
         Units = units)

# Check any NAs present in this data set.
new_river_ecoli %>% 
  vis_miss()

new <- new_river_ecoli %>% 
  group_by(Region, Year) %>% 
  summarise(Indicator = Measure, Value = sum(round(Median,2))) %>% 
  unique() %>% 
  filter(Year >= 2002, Year <= 2019) %>% 
  spread(key = Region,
         value = Value)

new2 <- new_river_ecoli %>% 
  group_by(Region, Year) %>% 
  summarise(Indicator = Measure, Value = sum(round(Median,2))) %>% 
  unique() %>% 
  filter(Year >= 2002, Year <= 2019) %>% 
  spread(key = Indicator,
         value = Value)
