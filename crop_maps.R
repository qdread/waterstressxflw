# Crop map processing script
# Initial processing done in GDAL

library(tidyverse)
library(sf)
library(raster)
library(RStoolbox)

# country boundaries in global equal area projection
countries <- st_read('data/raw_data/landuse/ecoregions/countries_global_equalarea.gpkg')

crop_table <- read_csv('data/raw_data/landuse/global_aglands/HarvestedAreaYield175Crops_Geotiff/earthstat_crops.csv')
crop_categories <- unique(crop_table$GROUP)

# Sum the rasters within each group.
crop_group_rasters <- map(set_names(crop_categories), ~ brick(paste0('data/raw_data/landuse/global_aglands/HarvestedAreaYield175Crops_Geotiff/', ., '.vrt')))
crop_sum_rasters <- map(crop_group_rasters, ~ calc(., function(x) sum(x, na.rm = TRUE)))

# Combine them, finding which is the maximum.
#crop_sum_brick <- brick(crop_sum_rasters[which(!names(crop_sum_rasters) %in% c('Fiber', 'Forage'))])
crop_sum_brick <- brick(crop_sum_rasters)
crop_dominant_raster <- calc(crop_sum_brick, function(x) {
  max_x <- max(x, na.rm = TRUE)
  max_idx <- which(x == max_x)
  if (length(max_idx) > 1 & max_x > 0) return(12) # Return 12 if multiple crops are tied for maximum.
  if (length(max_idx) == 0 | max_x == 0) return(NA) # Missing data.
  return(max_idx)
})

# Project raster to Mollweide with nearest neighbor method.
crop_dominant_proj <- projectRaster(crop_dominant_raster, crs = CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"), method = 'ngb')

# Mask by the country polygons.
crop_dominant_mask <- mask(crop_dominant_proj, countries)

# Reclassify, based on FAO scheme
# fruits and vegetables / oilseeds, nuts, and pulses / cereals / roots and tubers / other + sugar
# Commented version uses fiber and forage
rcl_mat <- cbind(from = 1:11, to = c(6, 6, 2, 5, 1, 1, 2, 3, 4, 2, 5))
#rcl_mat <- cbind(from = 1:9, to = c(2, 5, 1, 1, 2, 3, 4, 2, 5))
crop_dominant_reclass <- reclassify(crop_dominant_mask, rcl = rcl_mat)

group_labels <- c('Fruits and vegetables', 'Oilseeds, nuts, and pulses', 'Cereals', 'Roots and tubers', 'Sugar and other food crops', 'Fiber and forage crops')

ggplot() +
  geom_raster(aes(x = x, y = y, fill = factor(layer, labels = group_labels)), data = crop_dominant_reclass) +
  geom_sf(data = countries, fill = 'transparent') +
  scale_fill_brewer(name = 'Crop', palette = 'Set1', na.value = 'transparent', na.translate = FALSE) +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_line(color = 'gray75'),
        axis.title = element_blank())


# Food waste map, v2 ------------------------------------------------------

# For each pixel, take a weighted average of food waste rate, where the weights are the production, in tons, of each of the crops grown there.
production_group_rasters <- map(set_names(crop_categories), ~ brick(paste0('data/raw_data/landuse/global_aglands/HarvestedAreaYield175Crops_Geotiff/production_', ., '.vrt')))
production_sum_rasters <- map(production_group_rasters, ~ calc(., function(x) sum(x, na.rm = TRUE)))
production_brick <- brick(production_sum_rasters)

# Project brick to Mollweide with nearest neighbor method.
production_proj <- projectRaster(production_brick, crs = CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"), method = 'bilinear')

# Mask by the country polygons.
production_mask <- mask(production_proj, countries)

# Lookup table for countries
country_filled <- read_csv('data/crossreference_tables/fbs_country_lookup_filled.csv')

# Food loss worldwide dataset
fao_flw_world <- read_csv('data/raw_data/FAOSTAT/fao_world_flw_clean.csv')

flw_crops <- fao_flw_world %>% 
  filter(category_number %in% c(1,2,4,5)) %>%
  mutate(loss_cumulative = 1 - (1-loss_agricultural_production)*(1-loss_handling_storage)*(1-loss_processing_packaging)*(1-loss_distribution)*(1-loss_consumption))

flw_crops_average <- flw_crops %>%
  group_by(continent) %>%
  summarize_at(vars(starts_with('loss')), mean) %>%
  mutate(category_number = 6)

flw_crops <- bind_rows(flw_crops, flw_crops_average)

flw_crops$category[is.na(flw_crops$category)] <- 'Other'

# category mapping: order in production_brick mapped to category_number
category_mapping <- c(NA, NA, 4, 6, 5, 5, 4, 1, 2, 4, 6)

production_reclass_rasters <- list(Cereals = production_mask$Cereals,
                                   Roots.Tubers = production_mask$Roots.Tubers,
                                   Oilseeds.Pulses = production_mask$Oilcrops + production_mask$Pulses + production_mask$Treenuts,
                                   Fruits.Vegetables = production_mask$Fruit + production_mask$Vegetables.Melons,
                                   Other = production_mask$OtherCrops + production_mask$SugarCrops)

production_reclass_brick <- brick(production_reclass_rasters)

# Now we need to do a calculation based on the food loss rates of the country in which each pixel is found.
# Match country names in continent lookup table with those in the countries raster

wrong_names <- setdiff(country_filled$area, countries$NAME)
right_names <- c("Antigua and Barb.", 
                 "Bolivia", 
                 "Bosnia and Herz.", 
                 "Brunei",
                 "Central African Rep.",
                 "Hong Kong",
                 "Macao",
                 "China",
                 "Taiwan",
                 "Côte d'Ivoire",
                 "North Korea",
                 "Dominican Rep.",
                 "eSwatini",
                 "Fr. Polynesia",
                 "Iran",
                 "Laos",
                 "Macedonia",
                 "South Korea",
                 "Moldova", 
                 "Russia",
                 "St. Kitts and Nevis",
                 NA,
                 "São Tomé and Principe",
                 "Solomon Is.",
                 "Tanzania",
                 "Venezuela",
                 "Vietnam")

cbind(wrong_names[1:27], right_names)
country_filled$correct_name <- country_filled$area
for (i in 1:length(right_names)) country_filled$correct_name[country_filled$correct_name == wrong_names[i]] <- right_names[i]

full_join(country_filled, flw_crops, by = c('region' = 'continent'))

countries <- left_join(countries, country_filled, by = c('NAME' = 'correct_name'))

# Add in some regions if they aren't given a continent.
# do manually
#countries_extract <- countries %>% dplyr::select(NAME, area_code, area, region) %>% st_drop_geometry()
#write_csv(countries_extract, 'data/raw_data/water_x_food/world_region_lookup_for_map.csv')

continent_lookup <- read_csv('data/raw_data/water_x_food/world_region_lookup_for_map_filled.csv')

# Pare down countries to sf object with just name and continent, then rasterize.
countries_regions <- countries %>% dplyr::select(NAME) %>% cbind(continent_lookup[,-1])

library(fasterize)
library(biscale)
library(cowplot)

countries_regions_raster <- fasterize(countries_regions, production_mask[[1]], by = 'region')
region_code_raster <- calc(countries_regions_raster, function(x) which(x == 1)[1])

# Stack the reclassified production with the region code
production_and_region <- stack(production_reclass_brick, region_code_raster)

# Function to calculate the total food loss and waste of each pixel, based on multiplying the production times the cumulative food loss rate for its continent

# Lookup of continents
continent_number_lookup <- data.frame(continent = c("Europe", "North America and Oceania", "Industrialized Asia", 
                                                    "Sub-Saharan Africa", "North Africa, West and Central Asia", 
                                                    "South and Southeast Asia", "Latin America"),
                                      number = c(5, 8, 11, 1, 2, 3, 4))



waste_rates_list <- map(continent_number_lookup$continent, ~ flw_crops[flw_crops$continent == ., c('category', 'loss_cumulative')])

waste_total_fn <- function(vec, na.rm) {
  if (!vec[6] %in% c(1:5,8,11)) return(NA)
  the_continent <- continent_number_lookup$continent[which(continent_number_lookup$number == as.integer(vec[6]))]
  waste_rates <- as.numeric(waste_rates_list[[which(continent_number_lookup$continent == the_continent)]]$loss_cumulative)
  sum(vec[1:5] * waste_rates)
}

waste_total_raster <- calc(production_and_region, waste_total_fn)

waste_wtdmeanrate_fn <- function(vec, na.rm) {
  if (!vec[6] %in% c(1:5,8,11)) return(NA)
  the_continent <- continent_number_lookup$continent[which(continent_number_lookup$number == as.integer(vec[6]))]
  waste_rates <- as.numeric(waste_rates_list[[which(continent_number_lookup$continent == the_continent)]]$loss_cumulative)
  weighted.mean(waste_rates, vec[1:5])
}

waste_wtdmeanrate_raster <- calc(production_and_region, waste_wtdmeanrate_fn)

ggplot() +
  geom_raster(aes(x = x, y = y, fill = layer), data = waste_total_raster) +
  geom_sf(data = countries, fill = 'transparent') +
  scale_fill_viridis_c(name = 'Total crop wasted', na.value = 'transparent') +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_line(color = 'gray75'),
        axis.title = element_blank())

ggplot() +
  geom_raster(aes(x = x, y = y, fill = layer), data = waste_wtdmeanrate_raster) +
  geom_sf(data = countries, fill = 'transparent') +
  scale_fill_viridis_c(name = 'Food waste rate across all crops', na.value = 'transparent') +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_line(color = 'gray75'),
        axis.title = element_blank())


# Four color map water scarcity x food waste ------------------------------

# 0.4 is about halfway in flw, and 5 is halfway in the food waste data.

avg_ws_downscale <- projectRaster(avg_ws_proj, waste_wtdmeanrate_raster, method = 'bilinear')
flw_water_raster <- stack(avg_ws_downscale, waste_wtdmeanrate_raster)

# 1 high high, 2 high low, 3 low high, 4 low low (water stress, food waste)
flw_water_fourcat <- calc(flw_water_raster, function(x, na.rm) {
  if (any(is.na(x))) return(NA)
  if (x[1] > 5) {
    if (x[2] > 0.4) 1 else 2
  } else {
    if (x[2] > 0.4) 3 else 4
  }
})

ggplot() +
  geom_raster(aes(x = x, y = y, fill = factor(layer)), data = flw_water_fourcat) +
  geom_sf(data = countries, fill = 'transparent') +
  scale_fill_brewer(palette = 'Dark2', na.value = 'transparent', na.translate = FALSE, labels = c('high WS, high FW', 'high WS, low FW', 'low WS, high FW', 'low WS, low FW')) +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_line(color = 'gray75'),
        axis.title = element_blank(),
        legend.title = element_blank())

# Attempt with biscale
flw_water_data <- fortify(flw_water_raster)
flw_water_data <- bi_class(flw_water_data, x = "ws_avg", y = "layer", dim = 2, style = "quantile")

brewer_dark <- RColorBrewer::brewer.pal(8, "Dark2")
bipal <- bi_pal_manual(val_1_1 = brewer_dark[1], val_1_2 = brewer_dark[6], val_2_1 = brewer_dark[2], val_2_2 = brewer_dark[4])

pbiv <- ggplot() +
  geom_raster(aes(x = x, y = y, fill = bi_class), data = flw_water_data %>% filter(!is.na(ws_avg) & !is.na(layer)), show.legend = FALSE) +
  geom_sf(data = countries, fill = 'transparent') +
  bi_scale_fill(pal = "GrPink", dim = 2) +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_line(color = 'gray75'),
        axis.title = element_blank(),
        legend.title = element_blank())

legend <- bi_legend(pal = "GrPink",
                    dim = 2,
                    xlab = "Higher Water Scarcity ",
                    ylab = "Higher Food Waste ",
                    size = 8)

finalPlot <- ggdraw() +
  draw_plot(pbiv, 0, 0, 0.7, 1) +
  draw_plot(legend, 0.7, 0.3, 0.3, 0.3)

finalPlot
