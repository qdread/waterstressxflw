# New FBS extraction script
# QDR / Waterstressxflw 


library(tidyverse)
fp_fao <- 'data/raw_data/FAOSTAT/31aug2020'

fbs <- read_csv(file.path(fp_fao, "FoodBalanceSheets_E_All_Data_(Normalized).csv"))

# Name repair of fbs
fbs <- fbs %>% rename_with(function(x) gsub(' ', '_', tolower(x)))

# Write FBS "item" table so I can manually code the items to the main categories.
item_table <- unique(fbs[,c('item_code', 'item')])
write_csv(item_table, 'data/raw_data/water_x_food/fbs_item_lookup.csv')

# FAO world region lookup table
region_lookup <- read_csv('data/raw_data/FAOSTAT/world_region_lookup.csv') %>%
  mutate(country = gsub('\n', ' ', country))

# Write FBS "country" table so I can manually codethe countries to the world regions (not all are in FAO dataset)
country_table <- unique(fbs[, c('area_code', 'area')]) %>% left_join(region_lookup, by = c('area' = 'country'))
write_csv(country_table, 'data/raw_data/water_x_food/fbs_country_lookup.csv')

# Use only the most recent data with complete years
# FIXME convert this to an average of the most recent decade
fbs <- fbs %>% 
  filter(year == 2016) %>%
  select(-year_code)

fbs_foodsupply_weight <- fbs %>% filter(element_code == 645)
fbs_losses <- fbs %>% filter(element_code == 5123)
fbs_pop <- fbs %>% filter(element_code == 511) # Population, for per capita calculations

unique(fbs_losses$unit) # All fbs losses are in 1k tonnes units.

# Formula: calculate per-capita losses, in kg/capita/year, of all food combined. (or by category)
# Then calculate per-capita food supply and divide into the 11 FAO waste categories.
# Remove all "aggregated" categories.
# Use FAO's household waste number to get the amount of household waste and sum up.


# Per capita loss and supply ----------------------------------------------



# Per capita losses of each food.

fbs_losses <- fbs_losses %>% rename(loss = value) %>% select(area_code, area, item_code, item, loss)
fbs_pop <- fbs_pop %>% rename(population = value) %>% select(area_code, area, population)

fbs_losses_percapita <- left_join(fbs_losses, fbs_pop) %>%
  mutate(loss_kg_per_capita = (loss * 1e6)/(population * 1e3))


# Per capita food supply of each food.
fbs_foodsupply_weight <- fbs_foodsupply_weight %>% rename(supply = value) %>% select(area_code, area, item_code, item, supply)


# Join with coarser food loss categories ----------------------------------

# Lookup table for items
item_harmonized <- read_csv('data/crossreference_tables/fbs_item_lookup_harmonized.csv')

# Lookup table for countries
country_filled <- read_csv('data/crossreference_tables/fbs_country_lookup_filled.csv')

# Food loss worldwide dataset
fao_flw_world <- read_csv('data/raw_data/FAOSTAT/fao_world_flw_clean.csv')

# Get only consumption waste
consumption_waste <- fao_flw_world %>%
  select(category_number, category, continent, loss_consumption)

# Consumption waste by country
consumption_waste_by_country <- inner_join(consumption_waste, country_filled, by = c('continent' = 'region'))

# These will be food loss and food supply values, excluding categories such as sweeteners, alcoholic beverages, and small miscellaneous ones.
# Also ignore differences in household loss rates between fresh and processed items including fruits, veg, potatoes, and seafood.

all_fbs_flw <- fbs_losses_percapita %>%
  left_join(item_harmonized) %>%
  left_join(fbs_foodsupply_weight) %>%
  filter(!is.na(category_number)) %>%
  inner_join(consumption_waste_by_country)


# Calculate summary values ------------------------------------------------



fbs_flw_by_category <- all_fbs_flw %>%
  mutate(waste_kg_per_capita = loss_consumption *  supply) %>%
  group_by(continent, area_code, area, category_number, category) %>%
  summarize(loss_kg_per_capita = sum(loss_kg_per_capita, na.rm = TRUE),
            waste_kg_per_capita = sum(waste_kg_per_capita, na.rm = TRUE))

# Sum up all loss and waste to get the total amount.
fbs_flw_total <- fbs_flw_by_category %>%
  group_by(continent, area_code, area) %>%
  summarize(loss_kg_per_capita = sum(loss_kg_per_capita),
            waste_kg_per_capita = sum(waste_kg_per_capita),
            flw_kg_per_capita = loss_kg_per_capita + waste_kg_per_capita)

ggplot(fbs_flw_total, aes(x = continent, y = loss_kg_per_capita)) + geom_boxplot()
ggplot(fbs_flw_total, aes(x = continent, y = waste_kg_per_capita)) + geom_boxplot()
ggplot(fbs_flw_total, aes(x = continent, y = flw_kg_per_capita)) + geom_boxplot()


# Save values -------------------------------------------------------------

write_csv(fbs_flw_by_category, 'data/raw_data/water_x_food/fbs_flw_by_category.csv')
write_csv(fbs_flw_total, 'data/raw_data/water_x_food/fbs_flw_total.csv')
