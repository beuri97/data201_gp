# Load packages such as dplyr, tidyr, and readr to be able to use specialised functions for creating
# reading, writing, and manipulating data.
library(tidyverse)

# Load the tidygeocoder package to be able to use a function to convert the given latitude and longitude
# to address.
library(tidygeocoder)

# Takes the river_ecoli dataset then take the lat and long variables
# to get the full address. Then save it as new_riverecoli.
new_riverecoli <- river_ecoli %>%
  reverse_geocode(lat = lat, long = long,
                  method = "osm", full_results = TRUE)

# Takes the river_ecoli dataset then take the lat and long variables
# to get the full address. Then save it as new_rivernitrogen.
new_rivernitrogen <- river_nitrogen %>%
  reverse_geocode(lat = lat, long = long,
                  method = "osm", full_results = TRUE)

# Write the new dataset as CSVs for use.
write_csv(new_riverecoli, "new_river_ecoli.csv")
write_csv(new_rivernitrogen, "new_river_nitrogen.csv")