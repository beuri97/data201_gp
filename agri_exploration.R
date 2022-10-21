#------------ Examining and understanding the datas that were gathered online
library(tidyverse)
library(visdat)
library(readr)
library(skimr)

getwd() 

# Reading all the dataset in csv format
horti_info <- read_csv("https://raw.githubusercontent.com/beuri97/data201_gp/agri-activity/data/hort_data_info.csv")

horti_pop <- read_csv("https://raw.githubusercontent.com/beuri97/data201_gp/agri-activity/data/hort_pop_regional.csv")

livestock_info <- read_csv("https://raw.githubusercontent.com/beuri97/data201_gp/agri-activity/data/livestock_data_info.csv")

livestock_pop <- read_csv("https://raw.githubusercontent.com/beuri97/data201_gp/agri-activity/data/livestock_pop_regional.csv")

# Visualization of any missing values in our datasets
vis_miss(horti_info)
vis_dat(horti_info) # No NAs

vis_miss(horti_pop) 
vis_dat(horti_pop) # Few NA Values

vis_miss(livestock_info)
vis_dat(livestock_info) # No NAs

vis_miss(livestock_pop)
vis_dat(livestock_pop) # Few NA Values

#--------------------------- Removing those identified NAs
#--- For horti_pop Dataset
new_horti_pop <- horti_pop %>%
  drop_na(Value)                     #Dropping all NAs in the column Value from "horti_pop" dataset

vis_miss(new_horti_pop)
vis_dat(new_horti_pop)

latest_horti_pop <- subset(new_horti_pop, select = -Flags)  #Removing the column "FLags" as it only contains NA values

vis_miss(latest_horti_pop)
vis_dat(latest_horti_pop) # No NAs

#--- For livestock_pop Dataset
new_livestock_pop <- livestock_pop %>%
  drop_na(Value)                     #Dropping all NAs in the column Value from "livestock_pop" dataset

vis_miss(new_livestock_pop)
vis_dat(new_livestock_pop)

latest_livestock_pop <- subset(new_livestock_pop, select = -Flags)  #Removing the column "FLags" as it only contains NA values

vis_miss(latest_livestock_pop)
vis_dat(latest_livestock_pop) # No NAs

#------------------ Manipulating the format of the Dates, Renaming few variables, Filtering specific variables, etc.
#--- For horti_pop Dataset
latest_horti_pop$Year <- gsub("As at ", "", as.character(latest_horti_pop$Year))   # This removes the wording in our dataset Years Column

# Standardizing dates
test <- latest_horti_pop %>% 
  filter(!startsWith(latest_horti_pop$Year, "June 30")) %>%  # This is to check that all dates start with 30th of June
  head()

# All observations were made as at June 30 of each year. Therefore, month and day is irrelevant and can be removed.
latest_horti_pop$Year <- gsub("June 30 ", "", as.character(latest_horti_pop$Year))

latest_horti_pop$Year <- as.numeric(as.character(latest_horti_pop$Year)) # Turning the Year from character to numeric

latest_horti_pop <- latest_horti_pop %>% 
  filter(Area != "Total New Zealand") %>%  # Removing irrelevant data
  filter(Area != "Total North Island") %>%
  filter(Area != "Total South Island")

latest_horti_pop <- latest_horti_pop %>%
  rename(Activity = "Horticulture")                   # Renames the column Value to Total_Livestock


#--- For livestock_pop Dataset
latest_livestock_pop$Year <- gsub("As at June 30", "", as.character(latest_livestock_pop$Year))   # This removes the wording in our dataset Years Column

latest_livestock_pop$Year <- as.numeric(as.character(latest_livestock_pop$Year)) # Turning the Year from character to numeric

latest_livestock_pop <- latest_livestock_pop %>% filter(Area != "Total New Zealand") %>%  # Removing irrelevant data
  filter(Area != "Total North Island") %>%
  filter(Area != "Total South Island")

# Removing unnecessary data 
latest_livestock_pop_remove <- latest_livestock_pop %>% # defining unnecessary data to be removed
  filter(Livestock != "Total dairy cattle") %>%
  filter(Livestock != "Total beef cattle") %>% 
  filter(Livestock != "Total sheep") %>% 
  filter(Livestock != "Total deer")

# Extracting the necessary datas/variables we want
latest_livestock_pop <- latest_livestock_pop[!(latest_livestock_pop$Livestock %in% latest_livestock_pop_remove$Livestock),]               

# Renaming Variables.
latest_livestock_pop <- latest_livestock_pop %>%
  rename(Activity = "Livestock")                   # Renames Livestock to Activity 

latest_livestock_pop$Activity <- gsub("Total dairy cattle", "Dairy Cattle", as.character(latest_livestock_pop$Activity))

latest_livestock_pop$Activity <- gsub("Total beef cattle", "Beef Cattle", as.character(latest_livestock_pop$Activity))

latest_livestock_pop$Activity <- gsub("Total sheep", "Sheep", as.character(latest_livestock_pop$Activity))

latest_livestock_pop$Activity <- gsub("Total deer", "Deer", as.character(latest_livestock_pop$Activity))


#----- Creating unique keys (interaction between Area and Years) for both datasets
latest_livestock_pop$key <- interaction(latest_livestock_pop$Area, latest_livestock_pop$Year,  sep = " ") 

latest_horti_pop$key <- interaction(latest_horti_pop$Area, latest_horti_pop$Year, sep = " ")


# Transforming combined datasets to long and wide data format
activity_long <- latest_horti_pop %>% full_join(latest_livestock_pop) # full_join function returns all of the records in a new table


activity_wide <- activity_long %>% spread(key = Activity, value = Value)  # transform the combined dataset into long to wide format


# Renaming regions to match format of the other table
# For column Area
activity_wide$Area <- gsub("Hawkes", "Hawke's", as.character(activity_wide$Area))

activity_wide$Area <- gsub("Manawatu-Wanganui", "Manawatu-Whanganui", as.character(activity_wide$Area))

# For column key
activity_wide$key <- gsub("Hawkes", "Hawke's", as.character(activity_wide$key))

activity_wide$key <- gsub("Manawatu-Wanganui", "Manawatu-Whanganui", as.character(activity_wide$key))




#--- For visualizing (understand what those datas are telling us)
activity_long %>% ggplot(aes(x = Year, y = Value, colour = Area)) + geom_point() + facet_wrap(~ Activity)



#--- For visualizing (understand what those datas are telling us)
activity_long %>% ggplot(aes(x = Year, y = Value)) + geom_line(lwd = 1) +
  facet_grid(cols = vars(Activity),
    rows = vars(Area))


#----------------------------------- Farm Area Dataset
# reading the dataset 
data <- read_csv("https://raw.githubusercontent.com/beuri97/data201_gp/agri-activity/data/data.csv")

# creating farm area table
farm_area <- data %>%       # Cleaning the farm area data 
  filter(unit == 'Hectares') %>% 
  select(-variable) %>% 
  select(-unit)

farm_area

farm_area$region <- gsub(" Region", "", as.character(farm_area$region))  # removing the region from Region

farm_area %>% arrange(desc(year)) # Arranging form highest to lowest order

farm_area <- farm_area %>% filter(!region == "New Zealand")   # Removing all rows that is New Zealand

#------------ Joining Farm Area Dataset and Farm Activity (Wide) Dataset
farm_area <- farm_area %>%  # renaming
  rename(Area = "region")       

farm_area <- farm_area %>%  # renaming
  rename(Year = "year")      
 
farm_area <- farm_area %>%   # renaming
  rename(Total_Hectares = "value")      


farm_area$key <- interaction(farm_area$Area, farm_area$Year,  sep = " ") # Created a column of keys

activity_wide2 <- farm_area %>% full_join(activity_wide) # Joining farm area data with farm activity data 

activity_wide2 <- activity_wide2[, c(1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 2)] # Rearranging columns to be a more logical order


activity_wide2 %>% ggplot(aes(x = Year, y = `Beef Cattle`, colour = Area)) + geom_point() 
activity_wide2 %>% ggplot(aes(x = Year, y = `Dairy Cattle`, colour = Area)) + geom_point() 
activity_wide2 %>% ggplot(aes(x = Year, y = `Deer`, colour = Area)) + geom_point() 
activity_wide2 %>% ggplot(aes(x = Year, y = `Sheep`, colour = Area)) + geom_point() 
activity_wide2 %>% ggplot(aes(x = Year, y = `Total apples (hectares)`, colour = Area)) + geom_point() 








# Getting the Density of Beef Cattle per Hectare
test2 <-activity_wide2 %>% mutate(pop_density = `Beef Cattle` / Total_Hectares)
test2

# Plotting Density of Beef Cattle per Hectare
test2 %>% filter(Area == "Canterbury" | Area == "Auckland") %>%
  ggplot(aes(x = Year, y = pop_density, colour = Area)) + geom_bar(stat = 'identity') +
  facet_grid(cols = vars(Area))






write_csv(activity_long, "agriculture_activity_long.csv") 
