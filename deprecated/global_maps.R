# Maps

# source of water scarcity data:
# https://waterfootprint.org/media/downloads/WS_blue_monthly_rasters.zip (30x30 arc minute)
# https://waterfootprint.org/media/downloads/Report53-BlueWaterScarcity-ArcGIS-ShapeFile.zip (by basin)

library(raster)
library(sf)
library(tidyverse)
library(RStoolbox)

# country boundaries in global equal area projection
countries <- st_read('data/raw_data/landuse/ecoregions/countries_global_equalarea.gpkg')

# Water scarcity raster
avg_ws <- raster('data/raw_data/water_x_food/water_scarcity/WSbl_monthly_30m/ws_avg')

# Food waste index 
flw_total <- read_csv('data/raw_data/water_x_food/fbs_flw_total.csv')

# Project water raster into the equal area projection
avg_ws_proj <- projectRaster(avg_ws, crs = CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

# Make a map.
# Global water scarcity, taken directly from Mekonnen and Hoekstra 2016.
ggplot() +
  geom_raster(aes(x = x, y = y, fill = ws_avg), data = avg_ws_proj, na.rm = TRUE) +
  geom_sf(data = countries, fill = 'transparent') +
  scale_fill_viridis_c(na.value = 'transparent') +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_line(color = 'gray75'),
        axis.title = element_blank())

# Crop production: crop dominance layer
crop_dom <- raster('data/raw_data/landuse/global_aglands/cropdominance_equalarea.tif')
# Crop types: reclassify ocean and no crop as missing
crop_dom[crop_dom == 0 | crop_dom == 9] <- NA

ggplot() +
  geom_raster(aes(x = x, y = y, fill = factor(layer)), data = crop_dom, na.rm = TRUE) +
  geom_sf(data = countries, fill = 'transparent') +
  scale_fill_brewer(name = 'Crop', palette = 'Set1') +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_line(color = 'gray75'),
        axis.title = element_blank())


# food waste overall
# Country names
setdiff(flw_total$area, countries$NAME)
# Can be joined with a minimum of effort
wrong_names <- setdiff(flw_total$area, countries$NAME)
# Old one was for the overall_waste DF.
#right_names <- c("Bosnia and Herz.", "Moldova", "Macedonia", "Russia", "South Korea", "Bolivia", "Dominican Rep.", "Venezuela", "Syria", "Iran", "Vietnam", "Central African Rep.", "Eq. Guinea", "eSwatini", "Tanzania", "Dem. Rep. Congo")
right_names <- c("Bosnia and Herz.", "Moldova", "Macedonia", "Russia", "China", "Hong Kong", "South Korea", "Macao", "Taiwan", "Bolivia", "Dominican Rep.", "Venezuela", "Iran", "North Korea", "Laos", "Vietnam", "Central African Rep.", "Côte d'Ivoire", "São Tomé and Principe", "eSwatini", "Tanzania")

flw_total$correct_name <- flw_total$area
for (i in 1:length(wrong_names)) flw_total$correct_name[flw_total$correct_name == wrong_names[i]] <- right_names[i]

# Convert to food waste decile
flw_total <- flw_total %>%
  ungroup %>%
  mutate(food_waste_decile = Hmisc::cut2(flw_kg_per_capita, g = 10) %>% factor(labels = c('1st', '2nd', '3rd', paste0(4:10, 'th'))))

countries <- left_join(countries, flw_total, by = c('NAME' = 'correct_name'))

ggplot() +
  geom_sf(data = countries, aes(fill = food_waste_decile)) +
  scale_fill_viridis_d(na.value = 'gray80', name = 'Food waste decile') +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_line(color = 'gray75'),
        axis.title = element_blank())

