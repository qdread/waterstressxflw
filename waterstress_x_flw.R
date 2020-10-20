# Relationship between water stress/scarcity and food loss and waste
# QDR / 15 Oct 2020

# Need to do:
# 1. Get index of water stress by country from WRI
# 2. Get food production data from each country from FAOSTAT
# 3. Aggregate the food production data by the 11 FAO food waste categories
# 4. Calculate an overall food waste rate for each food category x country based on this
# 5. Join the data together
# 6. Test whether there is a relationship between water stress and food waste at food x country level, and at country level overall.


# Load data ---------------------------------------------------------------

# Run this line once
#file.symlink('/nfs/qread-data', 'data')

library(tidyverse)
library(readxl)
library(lme4)
library(MuMIn)

# Water stress
# Read a highly formatted xlsx in, needs to be cleaned but just a little
# Use the baseline water stress data for now
waterstress_countries <- read_xlsx('data/raw_data/WRI/aqueduct_countries_20140109.xlsx', sheet = 'Baseline Water Stress', .name_repair = 'universal') %>%
  select(Rank, Name, All.sectors, sd, Agricultural, Domestic, Industrial) %>%
  mutate_at(vars(All.sectors:Industrial), as.numeric)

# FAO food loss and waste rates data
fao_flw <- read_csv('data/raw_data/FAOSTAT/fao_world_flw_clean.csv')

# FAO world region lookup table
region_lookup <- read_csv('data/raw_data/FAOSTAT/world_region_lookup.csv') %>%
  mutate(country = gsub('\n', ' ', country))

# FAOSTAT production data for crops and animals
# Already averaged this for 2014-2018, somewhat cleaned

fp_fao <- file.path('data/cfs_io_analysis/faostat2017')
files_read <- dir(fp_fao, pattern = '^production') # all files begin with production
names(files_read) <- gsub('(production_)|(.csv)', '', files_read)

fao_prod <- map_dfr(files_read, ~ read_csv(file.path(fp_fao, .)), .id = 'category') 

# Crosswalk table with FAO codes
fao_cw <- read_csv('data/crossreference_tables/faostat_flw_crosswalk.csv')

# World population
data(pop, package = 'wpp2019')

# Calculate cumulative FLW rates ------------------------------------------

fao_flw <- fao_flw %>%
  mutate(flw_cumulative = 1 - (1-loss_agricultural_production)*(1-loss_handling_storage)*(1-loss_processing_packaging)*(1-loss_distribution)*(1-loss_consumption))

# Clean FAO data ----------------------------------------------------------

# Just do a very crude estimate of how much of each of the FLW category numbers, by weight, is produced in each country.

setdiff(fao_prod$item_code, fao_cw$code) # All codes are found in the crosswalk.

fao_prod <- fao_prod %>%
  left_join(fao_cw %>% select(code, flw_category_number), by = c('item_code' = 'code')) %>%
  filter(element %in% 'Production', unit %in% 'tonnes')

fao_prod_sums <- fao_prod %>%
  group_by(area_code, area, flw_category_number) %>%
  summarize(production = sum(value, na.rm = TRUE))


# Join production, food waste, and water stress datasets ------------------

# Fix Ivory Coast
fao_prod_sums$area[grepl('Ivoire',fao_prod_sums$area)] <- "C\xf4te d'Ivoire"
region_lookup$country[grepl('Ivoire',region_lookup$country)] <- "C\xf4te d'Ivoire"

# Join production with the world region lookup table
sort(setdiff(x = region_lookup$country, y = fao_prod_sums$area)) # These names need to match

# Manually create matching key
region_names_match <- c('Bolivia', 'Bosnia & Herzegovina', 'Czech Republic', 'Iran', "Lao People㤼㸲s Democratic Republic", 'Luxemburg', 'Macedonia', 'Moldova', 'United republic of Tanzania', 'USA', 'Venezuela', 'Vietnam', 'Islamic republic of Iraq', 'Libyan Arab Jamahiriya')
faoprod_names_match <- c('Bolivia (Plurinational State of)', 'Bosnia and Herzegovina', 'Czechia', 'Iran (Islamic Republic of)', "Lao People's Democratic Republic", 'Luxembourg', "The former Yugoslav Republic of Macedonia", 'Republic of Moldova', 'United Republic of Tanzania', 'United States of America', 'Venezuela (Bolivarian Republic of)', 'Viet Nam', 'Iraq', 'Libya')

# Change names in region lookup table to match
for (i in seq_along(region_names_match)) {
  region_lookup$country[region_lookup$country == region_names_match[i]] <- faoprod_names_match[i]
}

# Join production and global region
fao_prod_sums <- inner_join(fao_prod_sums, region_lookup, by = c('area' = 'country'))

# Join production and waste now that we have region
fao_prod_sums <- inner_join(fao_prod_sums, fao_flw, by = c('region' = 'continent', 'flw_category_number' = 'category_number'))

# Country names need to be matched to join water stress with production+waste

setdiff(x = fao_prod_sums$area, y = waterstress_countries$Name)

# These are the countries we need to find in the FAO dataset
sort(setdiff(y = fao_prod_sums$area, x = waterstress_countries$Name))

# Manually create matching key
waterstress_names_match <- c('Bolivia', 'Brunei', 'Cape Verde', 'Czech Republic', 'East Timor', 'Federated States of Micronesia', 'Guinea Bissau', 'Iran', 'Ivory Coast', 'Laos', 'Macedonia', 'Moldova', 'North Korea', 'Republic of Serbia', "Republic of the Congo", 'Russia', 'South Korea', 'Syria', 'Taiwan', 'The Bahamas', 'Venezuela', 'Vietnam')

faoprod_names_match <- c('Bolivia (Plurinational State of)', 'Brunei Darussalam', 'Cabo Verde', 'Czechia', 'Timor-Leste', 'Micronesia (Federated States of)', 'Guinea-Bissau', 'Iran (Islamic Republic of)', "Côte d'Ivoire", "Lao People's Democratic Republic", "The former Yugoslav Republic of Macedonia", "Republic of Moldova", "Democratic People's Republic of Korea", 'Serbia', 'Congo', 'Russian Federation', 'Republic of Korea', 'Syrian Arab Republic', 'China, Taiwan Province of', 'Bahamas', 'Venezuela (Bolivarian Republic of)', 'Viet Nam')

# Change names in waterstress to match
for (i in seq_along(waterstress_names_match)) {
  waterstress_countries$Name[waterstress_countries$Name == waterstress_names_match[i]] <- faoprod_names_match[i]
}

# Now all are fixed except a few small countries.
# Join
prod_stress_waste <- inner_join(fao_prod_sums, waterstress_countries, by = c('area' = 'Name'))
# This results in 152 countries with all data.

# Join population
setdiff(unique(prod_stress_waste$area), pop$name)

pop_names_match <- c('North Macedonia', 'Eswatini', 'Dem. Republic of the Congo')
faoprod_names_match <- c("The former Yugoslav Republic of Macedonia", "Swaziland", "Democratic Republic of the Congo")

# Change names in pop to match
for (i in seq_along(pop_names_match)) {
  pop$name[pop$name == pop_names_match[i]] <- faoprod_names_match[i]
}

prod_stress_waste <- inner_join(prod_stress_waste, pop, by = c('area' = 'name'))

# Do some basic tests -----------------------------------------------------

# 1. Rate of food waste vs water stress
# 2. Total food waste vs water stress
# 3. Per capita food waste vs water stress

# Do a crude measurement of water stress by region

theme_set(theme_minimal())

ggplot(prod_stress_waste, aes(x = region, y = Agricultural)) +
  geom_boxplot() +
  ggtitle('Agricultural water stress score distributions by global region')

# For each FLW category, plot the rate of food waste vs ag water stress score

ggplot(prod_stress_waste, aes(y = Agricultural, x = flw_cumulative, group = region, color = region)) +
  facet_wrap(~ paste(category, type), scales = 'free_y') +
  stat_summary(geom = 'pointrange', 
               fun = function(y) quantile(y, probs = 0.5),
               fun.min = function(y) quantile(y, probs = 0.25),
               fun.max = function(y) quantile(y, probs = 0.75)) +
  scale_color_brewer(palette = "Set1") +
  coord_flip()

# Do another crude calculation of food waste by category and country, to see if total food waste vs ag water stress score is different
# This may need to be converted to per capita

prod_stress_waste %>%
  mutate(total_waste = flw_cumulative * production) %>%
  ggplot(aes(x = Agricultural, y = total_waste, color = region)) +
  facet_wrap(~ paste(category, type), scales = 'free') +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  scale_y_log10()

prod_stress_waste %>%
  mutate(total_waste_per_capita = (flw_cumulative * production)/(`2015`* 1000)) %>%
  ggplot(aes(x = Agricultural, y = total_waste_per_capita, color = region)) +
  facet_wrap(~ paste(category, type), scales = 'free') +
  geom_point() +
  scale_color_brewer(palette = "Set1") 

# Sum up the per capita food waste by country
overall_waste <- prod_stress_waste %>%
  mutate(total_waste_per_capita = (flw_cumulative * production)/(`2015`* 1000)) %>%
  group_by(region, area_code, area) %>%
  summarize(total_waste_per_capita = sum(total_waste_per_capita),
            ag_water_stress = Agricultural[1])

ggplot(overall_waste, aes(x = ag_water_stress, y = total_waste_per_capita, color = region)) +
  geom_point() +
  scale_color_brewer(palette = "Set1") 

ggplot(overall_waste, aes(x = ag_water_stress, y = total_waste_per_capita, color = region)) +
  facet_wrap(~ region, scales = 'free_y') +
  geom_point() +
  scale_color_brewer(palette = "Set1") 

# Simple mixed effects model with a random intercept for each region
totalwaste_mm <- lmer(total_waste_per_capita ~ ag_water_stress + (1|region), data = overall_waste)
summary(totalwaste_mm)
confint(totalwaste_mm, method = 'boot')
r.squaredGLMM(totalwaste_mm) # 68% of variation explained by region, 4% explained by water stress index.

# Result: 

# Alternative with the two small regions collapsed ------------------------

overall_waste_consolidated <- overall_waste %>%
  mutate(region = fct_collapse(region, 
                               `South and Southeast Asia` = c('South and Southeast Asia', 'Industrialized Asia'),
                               `Americas and Oceania` = c('Latin America', 'North America and Oceania')
                               ))

totalwaste_mm <- lmer(total_waste_per_capita ~ ag_water_stress + (1|region), data = overall_waste_consolidated)
summary(totalwaste_mm)
confint(totalwaste_mm, method = 'boot', nsim = 9999)
r.squaredGLMM(totalwaste_mm) # 25% of variation explained by region, 4% explained by water stress index.
# This might be a bit of an improvement

# Results
# Coefficient: -0.037, 95% bootstrap CI = [-0.072, -0.004]
# R-squared marginal: 0.040, conditional: 0.249

# Plot without model fits
ggplot(overall_waste_consolidated, aes(x = ag_water_stress, y = total_waste_per_capita, color = region)) +
  facet_wrap(~ region) +
  geom_point() +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(name = 'Agricultural water stress index') +
  scale_y_continuous(name = 'Per capita food waste index', limits = c(0, 1.9), expand = c(0, 0)) +
  theme(legend.position = 'none')

# Use coefficient estimates from each continent to show a fitted line plus confidence interval separately on each panel.
pred_dat <- expand_grid(ag_water_stress = seq(0, 5, by = 0.01), region = unique(overall_waste_consolidated$region))
# Use bootstrap to get the fitted values with each iteration and then take quantiles


pred_func <- function(mm) {
  mm_coef <- coef(mm)
  pred_dat_intercept <- left_join(pred_dat, data.frame(region = row.names(mm_coef$region), intercept = mm_coef$region[,1]))
  with(pred_dat_intercept, intercept + ag_water_stress * mm_coef$region$ag_water_stress[1])
}
# try with bootMer()
pred_boot <- bootMer(totalwaste_mm, pred_func, nsim = 9999)

predCL <- t(apply(pred_boot$t, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.975)))

pred_dat <- pred_dat %>% 
  mutate(total_waste_per_capita = predict(totalwaste_mm, newdata = pred_dat, type = 'response'),
         ci_min = predCL[,1],
         ci_max = predCL[,2])

# Plot with model fits
p_fit <- ggplot(overall_waste_consolidated, aes(x = ag_water_stress, y = total_waste_per_capita)) +
  facet_wrap(~ region) +
  geom_line(data = pred_dat) +
  geom_line(data = pred_dat, aes(y = ci_min), linetype = 'dashed') +
  geom_line(data = pred_dat, aes(y = ci_max), linetype = 'dashed') +
  geom_point(aes(color = region)) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(name = 'Agricultural water stress index') +
  scale_y_continuous(name = 'Per capita food waste index', limits = c(0, 1.9), expand = c(0, 0)) +
  theme(legend.position = 'none')

ggsave('/nfs/qread-data/figures/foodwaste_vs_waterstress.png', p_fit, height = 5, width = 7, dpi = 300)
