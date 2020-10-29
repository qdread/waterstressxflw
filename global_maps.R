# Maps

# source of water scarcity data:
# https://waterfootprint.org/media/downloads/WS_blue_monthly_rasters.zip (30x30 arc minute)
# https://waterfootprint.org/media/downloads/Report53-BlueWaterScarcity-ArcGIS-ShapeFile.zip (by basin)

library(raster)
library(sf)

# country boundaries in global equal area projection
countries <- st_read('data/raw_data/landuse/ecoregions/countries_global_equalarea.gpkg')

# Water scarcity raster
avg_ws <- raster('data/raw_data/water_x_food/water_scarcity/WSbl_monthly_30m/ws_avg')

# Project water raster into the equal area projection
avg_ws_proj <- projectRaster(avg_ws, crs = CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

avg_ws_proj_dat <- fortify(avg_ws_proj)

# Make a map.
# Global water scarcity, taken directly from Mekonnen and Hoekstra 2016.
ggplot() +
  geom_raster(aes(x = x, y = y, fill = ws_avg), data = avg_ws_proj, na.rm = TRUE) +
  geom_sf(data = countries, fill = 'transparent') +
  scale_fill_viridis_c(na.value = 'transparent') +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_line(color = 'gray75'))

# Crop production and livestock production. Total produced in tonnes?

# food waste overall
# Country names
setdiff(overall_waste$area, countries$NAME)
# Can be joined with a minimum of effort
wrong_names <- setdiff(overall_waste$area, countries$NAME)
right_names <- c("Bosnia and Herz.", "Moldova", "Macedonia", "Russia", "South Korea", "Bolivia", "Dominican Rep.", "Venezuela", "Syria", "Iran", "Vietnam", "Central African Rep.", "Eq. Guinea", "eSwatini", "Tanzania", "Dem. Rep. Congo")

overall_waste$correct_name <- overall_waste$area
for (i in 1:length(wrong_names)) overall_waste$correct_name[overall_waste$correct_name == wrong_names[i]] <- right_names[i]

countries <- left_join(countries, overall_waste, by = c('NAME' = 'correct_name'))

ggplot() +
  geom_sf(data = countries, aes(fill = total_waste_per_capita)) +
  scale_fill_viridis_c(na.value = 'gray80') +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_line(color = 'gray75'))

