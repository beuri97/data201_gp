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

# read the three datasets for use
groundwq <- "groundwq_2004-2020.xlsx" %>% 
  read_excel()

groundwq %>% 
  glimpse()

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

groundwq %>% 
  vis_miss(warn_large_data = FALSE)

groundwq %>% 
  mutate(CensoredValue = ifelse(is.na(CensoredValue), NA_integer_, CensoredValue),
         Date = year(Date)) %>% 
  select(Region, Indicator, Units, Date, CensoredValue) %>% 
  filter(Indicator %in% c("E.coli", "Nitrate nitrogen"))

river_ecoli <- read_csv("new_river_ecoli.csv")
river_nitrogen <- read_csv("new_river_nitrogen.csv")


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

groundwater_mod <- groundwater %>% 
  select(region, measure, median, units, lower_confidence_level, upper_confidence_level, direction_confidence_lawa) %>% 
  filter(measure %in% c("Nitrate nitrogen", "E.coli")) %>% 
  group_by(region, measure) %>% 
  #summarise(Median = sum(median), Units = units, Lower_CI = lower_confidence_level, Upper_CI = upper_confidence_level, Direction_confidence = direction_confidence_lawa)
  summarise(Median = sum(median), Units = units) %>% 
  unique()
groundwater_mod

groundwater_mod_1 <- groundwater_mod %>% 
  spread(key = region,
         value = Median)
groundwater_mod_1
