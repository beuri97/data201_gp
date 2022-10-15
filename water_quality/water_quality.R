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

library(highcharter)

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

# new_groundwq <- groundwq %>% 
#   mutate(CensoredValue = ifelse(is.na(CensoredValue), NA_integer_, CensoredValue),
#          Year = year(Date),
#          Units = case_when(Indicator == "E.coli" ~ "cfu/100ml", TRUE ~ "g/m3"),
#          Region = case_when(Region == "Hawkes Bay" ~ "Hawke's Bay", TRUE ~ Region)) %>% 
#   select(Region, Indicator, Units, Year, CensoredValue) %>% 
#   filter(Indicator %in% c("E.coli", "Nitrate nitrogen"), Year >= 2002, Year <= 2019) %>% 
#   group_by(Region, Indicator, Year) %>% 
#   na.omit() %>% 
#   summarise(Mean_MedVal = round(mean(CensoredValue), 2))

# Takes the groundwq modify the values and rename some columns, select the relevant columns 
# and rows.
new_groundwq <- groundwq %>% 
  mutate(CensoredValue = ifelse(is.na(CensoredValue), NA_integer_, CensoredValue),
         Year = year(Date),
         Indicator = case_when(Indicator == "E.coli" ~ "E.coli cfu/100ml", TRUE ~ "Nitrate nitrogen g/m3"),
         Region = case_when(Region == "Hawkes Bay" ~ "Hawke's Bay",
                            Region == "Manawatu-Whanganui" ~ "Manawat??-Whanganui", TRUE ~ Region),
         WellName = LAWAWellName) %>% 
  select(Region, WellName, Latitude, Longitude, Indicator, Year, CensoredValue) %>% 
  filter(Indicator %in% c("E.coli cfu/100ml", "Nitrate nitrogen g/m3"), Year >= 2002, Year <= 2019)

# Take the new_groundwq to create a data frame about the quality of the groundwater.
# groundwater_quality <- new_groundwq %>%
#   select(Region, Year, Indicator, CensoredValue) %>% 
#   group_by(Region, Year, Indicator) %>% 
#   na.omit() %>% 
#   summarise(MeanVal = round(mean(CensoredValue), 2))

sites_quality <- new_groundwq %>% 
  select(Region, Year, WellName, CensoredValue, Indicator) %>% 
  group_by(Region, Year, WellName, Indicator) %>% 
  summarise(MeanVal = mean(CensoredValue))

sites <- new_groundwq %>% 
  select(Region, WellName, Latitude, Longitude) %>% 
  distinct

sites_quality_wide <- sites_quality %>% 
  spread(key = Indicator,
         value = MeanVal)

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
new_river_ecoli <- river_ecoli %>% 
  rename(Region = state, Year = end_year, Indicator = measure, Median = median, Units = units, S_ID = s_id,
         Latitude = lat, Longitude = long) %>% 
  mutate(Indicator = "E.coli cfu/100ml") %>% 
  select(Region, Year, S_ID, Median, Indicator, Latitude, Longitude) %>% 
  filter(Year >= 2002, Year <= 2019)

# Check for missing data again (NA)
river_ecoli %>% 
  vis_miss()

# # Takes the new_river_ecoli group the rows by Region, Indicator, and Year then create an new data frame containing the 
# # mean of the Median rounded off to 2.
# riverecoli <- new_river_ecoli %>%
#   select(Region, Year, Indicator, Median) %>% 
#   group_by(Region, Year, Indicator) %>% 
#   summarise(MeanVal = round(mean(Median), 2))
# 
# # Wide format data set spread by Indicator as key
# riverecoli_wide <- riverecoli %>% 
#   spread(key = Indicator,
#          value = MeanVal)

river_src_quality_ecoli <- new_river_ecoli %>% 
  select(Region, Year, S_ID, Median, Indicator) %>% 
  group_by(Region, Year, S_ID, Indicator) %>% 
  summarise(MeanVal = mean(Median))

river_src_ecoli <- new_river_ecoli %>% 
  select(Region, S_ID, Latitude, Longitude) %>% 
  distinct()

# Read the new_river_nitrogen.csv and store it as river_nitrogen for analysis.
river_nitrogen <- "new_river_nitrogen.csv" %>% 
  read_csv()

# Gives an overview of river_nitrogen such as columns, data types, the possible values, number of rows and columns.
# This allow us to select which relevant columns to select.
river_nitrogen %>% 
  glimpse()

# Select the relevant columns for analysis.
new_river_nitrogen <- river_nitrogen %>% 
  rename(Region = state, Year = end_year, Indicator = measure, Median = median, Units = units, S_ID = s_id,
         Latitude = lat, Longitude = long) %>% 
  filter(Year >= 2002, Year <= 2019, Indicator %in% c("Ammoniacal nitrogen", "Nitrate-nitrite nitrogen")) %>%
  mutate(Indicator = case_when(Indicator == "Ammoniacal nitrogen" ~ "Ammoniacal nitrogen g/m3",
                               TRUE ~ "Nitrate-nitrite nitrogen g/m3")) %>%
  select(Region, Year, S_ID, Median, Indicator, Latitude, Longitude)
  
# Reads the entirety of new_river_nitrogen and creates a plot to check if it contains missing data (NA). 
new_river_nitrogen %>% 
  vis_miss()

# # Takes the new_river_nitrogen data frame then select the necessary rows, group them
# # by Region, Indicator, and Year. Lastly, summarise them by getting the mean of the median values rounded
# # off by 2 s.f.
# rivernitrogen <- new_river_nitrogen %>% 
#   select(Region, Year, Indicator, Median) %>% 
#   group_by(Region, Year, Indicator) %>% 
#   summarise(MeanVal = round(mean(Median), 2))
# 
# # Takes the rivernitrogen then convert it to wide format.
# rivernitrogen_wide <- rivernitrogen %>% 
#   spread(key = Indicator,
#          value = MeanVal) 

river_src_quality_nitrogen <- new_river_nitrogen %>% 
  select(Region, Year, S_ID, Median, Indicator) %>% 
  group_by(Region, Year, S_ID, Indicator) %>% 
  summarise(MeanVal = mean(Median))

# Takes the new-river_nitrogen data frame to create a data frame containing the information about the sites where the measurements are taken
river_src_nitrogen <- new_river_nitrogen %>%
  select(Region, S_ID, Latitude, Longitude) %>%
  distinct()

# Join new_rivernitrogen and new_riverecoli to create river_quality dataframe.
river_quality <- river_src_quality_ecoli %>% 
  full_join(river_src_quality_nitrogen)

# river_src <- new_river_ecoli %>% 
#   full_join(new_river_nitrogen) %>% 
#   select(-c(Median, Indicator, Year)) %>% 
#   distinct()

river_src <- river_src_ecoli %>% 
  full_join(river_src_nitrogen) %>% 
  distinct()

# Generate csv file
write_csv(sites_quality, "groundwater_quality.csv")
write_csv(sites, "groundwater_sites.csv")

write_csv(river_quality, "river_quality.csv")
write_csv(river_src, "river_srcs.csv")

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


 # <- gwq_sites %>% 
 #  group_by(Region, Year, WellName, Indicator) %>% 
 #  summarise(Mean = mean(CensoredValue %>% na.omit()))


mean_by_well <- gwq_sites %>% 
  group_by(Region, WellName, Indicator) %>% 
  summarise(mean(CensoredValue))

gwq <- sites_quality_wide %>%
  na.omit() %>% 
  group_by(Year) %>% 
  summarise(MeanEcoli = mean(`E.coli cfu/100ml`), MeanNitrogen = mean(`Nitrate nitrogen g/m3`))

rq <- river_quality %>% 
  spread(key = Indicator,
         value = MeanVal) %>% 
  na.omit() %>% 
  group_by(Year) %>% 
  summarise(MeanEcoli = mean(`E.coli cfu/100ml`),
            MeanNitrogen = mean(`Nitrate-nitrite nitrogen g/m3`),
            MeanAmmoniacal = mean(`Ammoniacal nitrogen g/m3`))

overall_gwq <- highchart() %>% 
  hc_yAxis_multiples(
    list(lineWidth = 3, lineColor='blue', title=list(text="E.coli cfu/100ml")),
    list(lineWidth = 3, lineColor="green", title=list(text="Nitrate nitrogen g/m3"))
  ) %>% 
  hc_add_series(data = gwq$MeanEcoli, color='blue', name = "E.coli") %>% 
  hc_add_series(data = gwq$MeanNitrogen, color='green', name = "Nitrate nitrogen", yAxis = 1) %>%
  hc_xAxis(categories = gwq$Year, title = list(text = "Year")) %>% 
  hc_title(text = "Average E. coli Count and Nitrate Nitrogen Amount in NZ (2004 - 2019)")
overall_gwq

overall_rq <- highchart() %>% 
  hc_yAxis_multiples(
    list(lineWidth = 3, lineColor='blue', title=list(text="E.coli cfu/100ml")),
    list(lineWidth = 3, lineColor="green", title=list(text="Nitrogen g/m3"))
  ) %>% 
  hc_add_series(data = rq$MeanEcoli, color='blue', name = "E.coli") %>% 
  hc_add_series(data = rq$MeanNitrogen, color='green', name = "Nitrate-nitrite nitrogen", yAxis = 1) %>%
  hc_add_series(data = rq$MeanAmmoniacal, color='yellow', name = "Ammoniacal nitrogen", yAxis = 1) %>% 
  hc_xAxis(categories = rq$Year, title = list(text = "Year"))%>% 
  hc_title(text = "Average E. coli Count, Nitrate-Nitrite and Ammonical Nitrogen Amount in NZ (2002 - 2019)")
overall_rq

sitepropecoli <- river_quality %>% 
  filter(Indicator == "E.coli cfu/100ml") %>% 
  group_by(S_ID) %>% 
  summarise(m = mean(MeanVal)) %>% 
  mutate(Status = case_when(m <= 130 ~ "Excellent",
                            m > 130 & m <= 260 ~ "Good",
                            m > 260 & m <= 540 ~ "Fair",
                            TRUE ~ "Poor")) %>% 
  group_by(Status) %>% 
  summarise(counts = n()) %>% 
  mutate(prop = counts/sum(counts),
         Indicator = rep(c("E.coli (cfu/100ml)"), 4))

sitepropnitrate <- river_quality %>% 
  filter(Indicator == "Nitrate-nitrite nitrogen g/m3") %>% 
  group_by(S_ID) %>% 
  summarise(m = mean(MeanVal)) %>% 
  mutate(Status = case_when(m >= 0 & m <= 1 ~ "Excellent", 
                            m > 1 & m <= 5.65 ~ "Good",
                            m > 5.65 & m <= 11.3 ~ "Fair",
                            TRUE ~ "Poor")) %>% 
  group_by(Status) %>% 
  summarise(counts = n()) %>% 
  mutate(prop = counts/sum(counts),
         Indicator = rep(c("Nitrate-nitrite nitrogen (g/m3)"), 4))

siteammoniacal <- river_quality %>% 
  filter(Indicator == "Ammoniacal nitrogen g/m3") %>% 
  group_by(S_ID) %>% 
  summarise(m = mean(MeanVal)) %>% 
  mutate(Status = case_when(m >= 0 & m <= 1 ~ "Excellent", 
                            m > 1 & m <= 5.65 ~ "Good",
                            m > 5.65 & m <= 11.3 ~ "Fair",
                            TRUE ~ "Poor")) %>% 
  group_by(Status) %>% 
  summarise(counts = n()) %>% 
  mutate(prop = counts/sum(counts),
         Indicator = rep(c("Ammoniacal nitrogen (g/m3)"), 1))

sitepropnitrogen <- sitepropnitrate %>% 
  full_join(siteammoniacal)

river <- sitepropecoli %>% 
  full_join(sitepropnitrogen)

river$Status <- factor(river$Status, levels = c("Excellent", "Good", "Fair", "Poor"))

river_condition_plot <- river %>%
  ggplot(aes(x = Indicator, y = prop*100, fill = Status)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#00ABFD", "#00B81F",
                            "#85AD00", "#CF9400")) + 
  ylab("Percentage of sites") +
  ggtitle("River Quality (2002 - 2019)") +
  ylim(0, 100)

# Proportion of Nitrate nitrogen in groundwater exceeded over 50
nitro_threshold <- sites_quality %>% 
  filter(Indicator == "Nitrate nitrogen g/m3") %>% 
  group_by(Region, Year) %>% 
  count(MeanVal >= 50)

groundwater_nitro_prop <- sites_quality %>% 
  filter(Indicator == "Nitrate nitrogen g/m3") %>% 
  group_by(Region) %>% 
  count(MeanVal >= 50) %>% 
  spread(key = `MeanVal >= 50`,
         value = n) %>% 
  mutate(`FALSE` = if_else(is.na(as.double(`FALSE`)), 0, as.double(`FALSE`)),
         Total = `FALSE` + `TRUE`) %>% 
  group_by(Region) %>% 
  summarise(prop = `TRUE`/ Total)


# Groundwater condition by E.coli
groundwater_ecoli_condition <- sites_quality %>% 
  filter(Indicator == "E.coli cfu/100ml") %>% 
  group_by(WellName) %>% 
  summarise(m = mean(MeanVal)) %>% 
  mutate(Status = case_when(m <= 130 ~ "Excellent",
                            m > 130 & m <= 260 ~ "Good",
                            m > 260 & m <= 540 ~ "Fair",
                            TRUE ~ "Poor")) %>% 
  group_by(Status) %>% 
  summarize(Count = n()) %>% 
  mutate(prop=Count/sum(Count), Indicator="E.coli (cfu/100ml)")

ecoli_risk <- groundwater_ecoli_condition %>% 
  ggplot(aes(x =  Status, y = Count, fill=Status))+
  geom_bar(stat="identity")+
  geom_text(aes(label = Count), vjust=1.5)
ecoli_risk


# Groundwater condition by Nitrate nitrogen 
groundwater_nitro_condition <- sites_quality %>% 
  filter(Indicator == "Nitrate nitrogen g/m3") %>% 
  group_by(WellName) %>% 
  summarise(m = mean(MeanVal)) %>% 
  mutate(Status = case_when(m >= 0 & m <= 1 ~ "Excellent", 
                            m > 1 & m <= 5.65 ~ "Good",
                            m > 5.65 & m <= 11.3 ~ "Fair",
                            TRUE ~ "Poor")) %>% 
  group_by(Status) %>% 
  summarize(Count = n()) %>% 
  mutate(prop=Count/sum(Count), Indicator = "Nitrate nitrogen (g/m3)")

groundwater_nitro_condtion_plot <- df103_1 %>% 
  ggplot(aes(x =  Status, y = Count, fill=Status))+
  geom_bar(stat="identity")+
  geom_text(aes(label = Count), vjust=1.5)
groundwater_nitro_condtion_plot


# Overall condition of groundwater
ground_water_condition <- groundwater_ecoli_condition %>% 
  full_join(groundwater_nitro_condition)

ground_water_condition$Status <- factor(ground_water_condition$Status, levels = c("Excellent", "Good", "Fair", "Poor"))

groundwater_condition_plot <- ground_water_condition %>% 
  ggplot(aes(x = Indicator, y = prop*100, fill = Status)) +
  geom_bar(position = "stack", stat="identity") +
  scale_fill_manual(values = c("#00ABFD", "#00B81F",
                               "#85AD00", "#CF9400")) + 
  ylab("Percentage of sites") +
  ggtitle("Groundwater Quality (2004 - 2019)") +
  ylim(0, 100)
groundwater_condition_plot

ggsave(groundwater_condition_plot, file = "groundwater_condition.png", width = 7, height = 6)
ggsave(river_condition_plot, file = "river_condition.png", width = 7, height = 6)

