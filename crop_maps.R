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
crop_group_rasters <- map(set_names(crop_categories), ~ raster(paste0('data/raw_data/landuse/global_aglands/HarvestedAreaYield175Crops_Geotiff/', ., '.vrt')))
crop_sum_rasters <- map(crop_group_rasters, ~ calc(., function(x) sum(x, na.rm = TRUE)))

# Combine them, finding which is the maximum.
# excludes fiber and forage crops
crop_sum_brick <- brick(crop_sum_rasters[which(!names(crop_sum_rasters) %in% c('Fiber', 'Forage'))])
#crop_sum_brick <- brick(crop_sum_rasters)
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
rcl_mat <- cbind(from = 1:9, to = c(2, 5, 1, 1, 2, 3, 4, 2, 5))
crop_dominant_reclass <- reclassify(crop_dominant_mask, rcl = rcl_mat)

group_labels <- c('Fruits and vegetables', 'Oilseeds, nuts, and pulses', 'Cereals', 'Roots and tubers', 'Sugar and other food crops')

ggplot() +
  geom_raster(aes(x = x, y = y, fill = factor(layer, labels = group_labels)), data = crop_dominant_reclass) +
  geom_sf(data = countries, fill = 'transparent') +
  scale_fill_brewer(name = 'Crop', palette = 'Set1', na.value = 'transparent', na.translate = FALSE) +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_line(color = 'gray75'),
        axis.title = element_blank())
