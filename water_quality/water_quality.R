library(tidygeocoder)
library(tidyverse)

# read the three datasets for use
groundwater <- read_csv("groundwater_quality.csv")
river_ecoli <- read_csv("river_ecoli.csv")
river_nitrogen <- read_csv("river_nitrogen.csv")

# takes the river_ecoli dataset then take the lat and long variables 
# to get the full address. Then save it as new_riverecoli.
new_riverecoli <- river_ecoli %>% 
  reverse_geocode(lat = lat, long = long,
                  method = "osm", full_results = TRUE)

# takes the river_ecoli dataset then take the lat and long variables 
# to get the full address. Then save it as new_rivernitrogen.
new_rivernitrogen <- river_nitrogen %>% 
  reverse_geocode(lat = lat, long = long,
                  method = "osm", full_results = TRUE)

# write the new dataset as CSVs for use
write_csv(new_riverecoli, "new_river_ecoli.csv")
write_csv(new_rivernitrogen, "new_river_nitrogen.csv")
