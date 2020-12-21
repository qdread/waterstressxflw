# Final draft of all maps
# 2x2 panel.
# Panel a: dominant crops
# Panel b: water scarcity
# Panel c: food waste
# Panel d: water scarcity x food waste hotspots

# Robinson projection, no gridlines


# Load packages --------------------------------------------------



library(raster)
library(sf)
library(tidyverse)
library(RStoolbox)
library(geosphere)
library(fasterize)
library(biscale)
library(cowplot)

robin <- '+proj=robin' # Robinson projection

theme_set(
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
)

# country boundaries in global equal area projection, then transform to Robinson
countries <- st_read('data/raw_data/landuse/ecoregions', 'ne_50m_admin_0_countries') %>%
  st_transform(robin) 

# Panel b: water scarcity -------------------------------------------------

# Global water scarcity, taken directly from Mekonnen and Hoekstra 2016.
# Water scarcity raster
avg_ws <- raster('data/raw_data/water_x_food/water_scarcity/WSbl_monthly_30m/ws_avg')

# Project water raster into the equal area projection
avg_ws_proj <- projectRaster(avg_ws, crs = CRS(robin))

# Create a mask for countries

extent_countries <- as(extent(c(-180, 180, -58, 84)), "SpatialPolygons")
crs(extent_countries) <- "+proj=longlat +ellps=WGS84"
extent_countries <- makePoly(extent_countries)  # add additional vertices
extent_countries <- spTransform(extent_countries, robin)

avg_ws_proj <- mask(avg_ws_proj, extent_countries) 
# This must be done or Alaska and Siberia will appear as duplicated areas.


# Panel a: dominant crops -------------------------------------------------

crop_table <- read_csv('data/raw_data/landuse/global_aglands/HarvestedAreaYield175Crops_Geotiff/earthstat_crops.csv')
crop_categories <- unique(crop_table$GROUP)

# Sum the rasters within each group.
crop_group_rasters <- map(set_names(crop_categories), ~ brick(paste0('data/raw_data/landuse/global_aglands/HarvestedAreaYield175Crops_Geotiff/', ., '.vrt')))
crop_sum_rasters <- map(crop_group_rasters, ~ calc(., function(x) sum(x, na.rm = TRUE)))

# Combine them, finding which is the maximum.
crop_sum_brick <- brick(crop_sum_rasters)
crop_dominant_raster <- calc(crop_sum_brick, function(x) {
  max_x <- max(x, na.rm = TRUE)
  max_idx <- which(x == max_x)
  if (length(max_idx) > 1 & max_x > 0) return(12) # Return 12 if multiple crops are tied for maximum.
  if (length(max_idx) == 0 | max_x == 0) return(NA) # Missing data.
  return(max_idx)
})

# Project raster to Robinson with nearest neighbor method.
crop_dominant_proj <- projectRaster(crop_dominant_raster, crs = CRS(robin), method = 'ngb')

# Mask by the country polygons.
crop_dominant_mask <- mask(crop_dominant_proj, extent_countries)

# Reclassify, based on FAO scheme
# fruits and vegetables / oilseeds, nuts, and pulses / cereals / roots and tubers / other + sugar
# Commented version uses fiber and forage
rcl_mat <- cbind(from = 1:11, to = c(6, 6, 2, 5, 1, 1, 2, 3, 4, 2, 5))
crop_dominant_reclass <- reclassify(crop_dominant_mask, rcl = rcl_mat)

group_labels <- c('Fruits and vegetables', 'Oilseeds, nuts, and pulses', 'Cereals', 'Roots and tubers', 'Sugar and other food crops', 'Fiber and forage crops')

group_colors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")[c(1,7,4,5,6,3)]


# Panel c: Weighted average food waste ------------------------------------

# For each pixel, take a weighted average of food waste rate, where the weights are the production, in tons, of each of the crops grown there.
production_group_rasters <- map(set_names(crop_categories), ~ brick(paste0('data/raw_data/landuse/global_aglands/HarvestedAreaYield175Crops_Geotiff/production_', ., '.vrt')))
production_sum_rasters <- map(production_group_rasters, ~ calc(., function(x) sum(x, na.rm = TRUE)))
production_brick <- brick(production_sum_rasters)

# Project brick to Robinson with nearest neighbor method.
production_proj <- projectRaster(production_brick, crs = CRS(robin), method = 'bilinear')

# Mask by the country polygons.
production_mask <- mask(production_proj, extent_countries)

# Lookup table for countries
country_filled <- read_csv('data/crossreference_tables/fbs_country_lookup_filled.csv')

# Food loss worldwide dataset
fao_flw_world <- read_csv('data/raw_data/FAOSTAT/fao_world_flw_clean.csv')

# Manipulate FLW data for only the crops we need, and cumulative waste through the supply chain.
flw_crops <- fao_flw_world %>% 
  filter(category_number %in% c(1,2,4,5)) %>%
  mutate(loss_cumulative = 1 - (1-loss_agricultural_production)*(1-loss_handling_storage)*(1-loss_processing_packaging)*(1-loss_distribution)*(1-loss_consumption))

# Add an average across crops to be used for "other"
flw_crops_average <- flw_crops %>%
  group_by(continent) %>%
  summarize_at(vars(starts_with('loss')), mean) %>%
  mutate(category_number = 6)

flw_crops <- bind_rows(flw_crops, flw_crops_average)

flw_crops$category[is.na(flw_crops$category)] <- 'Other'

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

countries <- left_join(countries, country_filled, by = c('NAME' = 'correct_name'))

continent_lookup <- read_csv('data/raw_data/water_x_food/world_region_lookup_for_map_filled.csv')

# Pare down countries to sf object with just name and continent, then rasterize.
countries_regions <- countries %>% dplyr::select(NAME) %>% cbind(continent_lookup[,-1])

countries_regions_raster <- fasterize(countries_regions, production_mask[[1]], by = 'region')
region_code_raster <- calc(countries_regions_raster, function(x) which(x == 1)[1])

# Stack the reclassified production with the region code
production_and_region <- stack(production_reclass_brick, region_code_raster)


# Lookup of continents
continent_number_lookup <- data.frame(continent = c("Europe", "North America and Oceania", "Industrialized Asia", 
                                                    "Sub-Saharan Africa", "North Africa, West and Central Asia", 
                                                    "South and Southeast Asia", "Latin America"),
                                      number = c(5, 8, 11, 1, 2, 3, 4))



waste_rates_list <- map(continent_number_lookup$continent, ~ flw_crops[flw_crops$continent == ., c('category', 'loss_cumulative')])

# Function to get the weighted mean waste rate for each pixel, based on its continent's waste rates.
waste_wtdmeanrate_fn <- function(vec, na.rm) {
  if (!vec[6] %in% c(1:5,8,11)) return(NA)
  the_continent <- continent_number_lookup$continent[which(continent_number_lookup$number == as.integer(vec[6]))]
  waste_rates <- as.numeric(waste_rates_list[[which(continent_number_lookup$continent == the_continent)]]$loss_cumulative)
  weighted.mean(waste_rates, vec[1:5])
}

waste_wtdmeanrate_raster <- calc(production_and_region, waste_wtdmeanrate_fn)


# Panel d: FLW x WS hotspots ----------------------------------------------

# Water scarcity raster projected to same resolution as food waste raster so they can be stacked.
avg_ws_downscale <- projectRaster(avg_ws_proj, waste_wtdmeanrate_raster, method = 'bilinear')
flw_water_raster <- stack(avg_ws_downscale, waste_wtdmeanrate_raster)

flw_water_data <- fortify(flw_water_raster)
flw_water_data <- bi_class(flw_water_data, x = "ws_avg", y = "layer", dim = 2, style = "quantile")


# align all plots ------------------------------------------

line_width <- 0.2

p_waterscarcity <- ggplot() +
  geom_raster(aes(x = x, y = y, fill = ws_avg), data = avg_ws_proj, na.rm = TRUE) +
  geom_sf(data = countries %>% st_crop(extent_countries), fill = 'transparent', size = line_width) +
  geom_sf(data = st_as_sfc(extent_countries), fill = 'transparent') +
  scale_fill_viridis_c(name = 'water scarcity\nindex', na.value = 'transparent')

p_dominantcrop <- ggplot() +
  geom_raster(aes(x = x, y = y, fill = factor(layer, labels = group_labels)), data = crop_dominant_reclass) +
  geom_sf(data = countries %>% st_crop(extent_countries), fill = 'transparent', size = line_width) +
  geom_sf(data = st_as_sfc(extent_countries), fill = 'transparent') +
  scale_fill_manual(name = 'dominant crop type', labels = group_labels, values = group_colors, na.value = 'transparent', na.translate = FALSE)

p_wasterate <- ggplot() +
  geom_raster(aes(x = x, y = y, fill = layer * 100), data = waste_wtdmeanrate_raster) +
  geom_sf(data = countries %>% st_crop(extent_countries), fill = 'transparent', size = line_width) +
  geom_sf(data = st_as_sfc(extent_countries), fill = 'transparent') +
  scale_fill_viridis_c(name = 'food waste\nrate (%)', na.value = 'transparent') 

p_hotspot <- ggplot() +
  geom_raster(aes(x = x, y = y, fill = bi_class), data = flw_water_data %>% filter(!is.na(ws_avg) & !is.na(layer)), show.legend = FALSE) +
  geom_sf(data = countries %>% st_crop(extent_countries), fill = 'transparent', size = line_width) +
  geom_sf(data = st_as_sfc(extent_countries), fill = 'transparent') +
  bi_scale_fill(pal = "GrPink", dim = 2) 

legend_hotspot <- bi_legend(pal = "GrPink",
                            dim = 2,
                            xlab = "Higher Water Scarcity ",
                            ylab = "Higher Food Waste ",
                            size = 6)

legend_waterscarcity <- get_legend(p_waterscarcity)
legend_dominantcrop <- get_legend(p_dominantcrop)
legend_wasterate <- get_legend(p_wasterate)

noleg <- theme(legend.position = 'none')

p_final <- ggdraw(plot_grid(
       plot_grid(plot_grid(p_waterscarcity + noleg, p_wasterate + noleg, ncol = 1, align = 'v'),
                 plot_grid(legend_waterscarcity, legend_wasterate, ncol = 1, align = 'v'),
                 rel_widths = c(1, 0.3)),
       plot_grid(plot_grid(p_dominantcrop + noleg, p_hotspot, ncol = 1, align = 'v'),
                 plot_grid(legend_dominantcrop, legend_hotspot, ncol = 1, align = 'v'),
                 rel_widths=c(1, 0.3)))
)

ggsave('data/figures/frontiers_ms/four_maps_frontiers_review.pdf', p_final, height = 4, width = 11)


# Plot each one as a separate figure --------------------------------------

# If only panel D is used in the main MS, the remaining ones can go in the appendix.

# Use ggdraw() to lay out plot and legend
# Manually draw the legend with four color boxes (bi_legend does not give enough control over the relative sizes).
label_x <- 'water scarcity'
label_y <- 'food loss and waste'
grpink <- biscale:::pal_grpink(n = 2)
legdat <- data.frame(x = c(2,1,2,1), y=c(2,2,1,1), class=grpink)
legend_hotspot_manual <- ggplot(legdat, aes(x=x, y=y)) + 
  geom_text(aes(label = lab), data = data.frame(lab = c('low','high','low','high'), x = c(1, 2, 0, 0), y = c(0, 0, 1, 2)), angle = c(0, 0, 90, 90), size = 2, vjust = c(-0.5, -0.5, 1.5, 1.5)) +
  geom_tile(aes(fill = class)) + scale_fill_identity() +
  labs(x = label_x, y = label_y) +
  theme(axis.title = element_text(size = rel(0.7))) + coord_fixed()

p_hotspot_with_legend <- ggdraw() +
  draw_plot(p_hotspot, x = 0, y = 0, width = 0.8, height = 1, scale = 1, hjust = 0, vjust = 0) +
  draw_plot(legend_hotspot_manual, x = 0.76, y = 0.3, width = 0.2, height = 0.4, scale = 1, hjust = 0, vjust = 0)

# Save
ggsave('data/figures/frontiers_ms/hotspot_map_frontiers_review.pdf', p_hotspot_with_legend, height = 2.5, width = 5.5)
ggsave('data/figures/frontiers_ms/hotspot_map_frontiers_review.png', p_hotspot_with_legend, height = 2.5, width = 5.5, dpi = 400)


# Save the remaining figures as individual PNG files.
ggsave('data/figures/frontiers_ms/waterscarcity_map.png', p_waterscarcity, height = 2.5, width = 5.5, dpi = 400)
ggsave('data/figures/frontiers_ms/dominantcrop_map.png', p_dominantcrop + theme(legend.text = element_text(size = rel(0.5))), height = 2.5, width = 5.5, dpi = 400)
ggsave('data/figures/frontiers_ms/wasterate_map.png', p_wasterate, height = 2.5, width = 5.5, dpi = 400)
