library(tidygeocoder)
library(tidyverse)

groundwater <- read_csv("groundwater_quality.csv")
river_ecoli <- read_csv("river_ecoli.csv")
river_nitrogen <- read_csv("river_nitrogen.csv")

new_riverecoli <- river_ecoli %>% 
  reverse_geocode(lat = lat, long = long,
                  method = "osm", full_results = TRUE)

new_riverecoli <- river_ecoli %>% 
  reverse_geocode(lat = lat, long = long,
                  method = "osm", full_results = TRUE)
  
new_rivernitrogen <- river_nitrogen %>% 
  reverse_geocode(lat = lat, long = long,
                  method = "osm", full_results = TRUE)

