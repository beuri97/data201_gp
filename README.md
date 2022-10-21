# Agricultural Activity vs Water Quality

This contains datasets about agricultural activity and water quality (river and groundwater) across New Zealand.

### Agricultural Activity
The agricultural activity dataset contains information on the agricultural activities carried out within the boundaries of regional councils from the year 1994 to 2021. This includes:

- The population of livestock by different species, 
- the area extent dedicated to a range of horticultural activities, 
- the area extent dedicated to livestock.

This is presented in a wide format. This means that each single row corresponds to a year (Year) and region of observation (Area). Reading across the columns gives different variables for that Year in that Area. The breakdown on livestock population and horticultural activity was sourced from Stats NZ. The breakdown of area extent for agricultural activity was sourced from the Ministry for the Environment. This dataset is related to the water quality dataset via the year of observation (Year) and the region of observation (Area). 
**Note**: The temporal resolution for this data varies across variables. So, NAs appear for variables that weren’t recorded in a given year.

#### Variables
**Area [Key]** - The region in which the observation was made.
**Year [Key]** - Year from when the observation was made 
**Beef Cattle** - The population of beef cattle taken at a given observation
**Dairy Cattle** - The population of dairy cattle taken at a given observation
**Deer** - The total population of Deer taken at a given observation
**Sheep** - The total population of Sheep taken at a given observation
**Total apples (hectares)** - The total area dedicated to apple growing for given observation, in hectares
**Total avocados (hectares)** - The total area dedicated to avocados growing for a given observation, in hectares
**Total kiwifruit (hectares)** - The total area dedicated to kiwifruit growing for a given observation, in hectares
**Total olives (hectares)** - The total area dedicated to olives growing for a given observation, in hectares
**Total onions (hectares)** - The total area dedicated to onions growing for a given observation, in hectares
**Total potatoes (hectares)** - The total area dedicated to potatoes growing for a given observation, in hectares
**Total squash (hectares)** - The total area dedicated to squash growing for a given observation, in hectares
**Total wine grapes (hectares)** - The total area dedicated to wine grape growing for a given observation, in hectares
**Total_Hectares** - The total area dedicated to agriculture for a given observation, in hectares
**Dairy area (ha)** - The total area dedicated to dairy farming for a given observation, in hectares
**Floriculture area (ha)** - The total area dedicated to floriculture for a given observation, in hectares
**Forestry area (ha)** - The total area dedicated to forestry area for a given observation, in hectares
**Fruit and berry area (ha)** - The total area dedicated to fruit and berry for a given observation, in hectares
**Grain growing area (ha)** - The total area dedicated to grain growing for a given observation, in hectares
**Nursery and turf area (ha)** - The total area allocated to plant nurseries for a given observation, in hectares
**Other area (ha)** - The total area dedicated to unspecified agricultural activity for a given observation, in hectares
**Other Livestock area (ha)** - The total area dedicated to unspecified livestock (e.g: deer) for a given observation, in hectares
**Sheep and Beef area (ha)** - The total area dedicated to both sheep and beef livestock for a given observation, in hectares
**Vegetable growing area (ha)** - The total area dedicated to vegetable growing for a given observation, in hectares
**Note**: combination of Area and Year uniquely identifies each observation.

### Water Quality
The water quality dataset is split into four datasets containing information about the monitored well sites (2004 to 2019) and river sites (2002 to 2019) across New Zealand. This includes:

- The measurements of the water quality attribute (E. coli and nitrogen) that affect the sites' suitability for various use cases,
- the name and coordinates of the monitored sites.

The data are gathered from different government agencies such as Tatauranga Aotearoa (StatsNZ) and the Ministry for the Environment which are wrangled and standardised. 
#### Water Quality Dataset Schema
![]()

#### Variables

##### Groundwater Sites Quality
**Region [Key]** - The region where the well is located.
**WellName [Key]** - The name of the well where the measurements are taken.
**Year** - The year the measurements are taken/recorded.
**MeanVal** - The mean measurement of the water quality attribute for specific year.
**Indicator** - The water quality attribute and its unit of measurement.
##### Groundwater Sites 
**Region [Key]** - The region where the well is located.
**WellName [Key]** - The name of the well where the measurements are taken.
**Latitude** - The latitude where the well is located.
**Longitude** - The latitude where the well is located.

##### River Sites Quality
**Region** - The region where the river site is located.
**S_ID** - The river site ID.
**MeanVal** - The mean measurement of the water quality attribute for specific year.
**Indicator** - The water quality attribute and its unit of measurement.
##### River Sites
**Region** - The region where the river site is located.
**S_ID** - The river site ID.
**Latitude** - The latitude where the river site is located.
**Longitude** - The longitude where the river site is

#### Authors
Henri Liswoyo, Moses Velano, Alzen Punio, and HanByeol Yang
