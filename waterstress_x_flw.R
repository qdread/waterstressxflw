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

# Water stress
# Read a highly formatted xlsx in, needs to be cleaned but just a little
# Use the baseline water stress data for now
waterstress_countries <- read_xlsx('data/raw_data/WRI/aqueduct_countries_20140109.xlsx', sheet = 'Baseline Water Stress', .name_repair = 'universal') %>%
  select(Rank, Name, All.sectors, sd, Agricultural, Domestic, Industrial) %>%
  mutate_at(vars(All.sectors:Industrial), as.numeric)

# FAO food loss and waste rates data
fao_flw <- read_csv('data/crossreference_tables/fao_percentages_extended.csv')

# FAOSTAT production data for crops and animals
# Will need to be cleaned and individual items will need to be harmonized with the FLW rate data.

fp_fao <- file.path('data', 'raw_data/FAOSTAT/31aug2020')

# Production and yield data on crops, crops processed, livestock, livestock primary, and livestock processed.
# Read the production CSVs
prod_crop_rawdata <- read_csv(file.path(fp_fao, 'Production_Crops_E_All_Data_(Normalized).csv'))
prod_cropsprocessed_rawdata <- read_csv(file.path(fp_fao, 'Production_CropsProcessed_E_All_Data_(Normalized).csv'))
prod_livestock_rawdata <- read_csv(file.path(fp_fao, 'Production_Livestock_E_All_Data_(Normalized).csv'))
prod_livestockprimary_rawdata <- read_csv(file.path(fp_fao, 'Production_LivestockPrimary_E_All_Data_(Normalized).csv'))
prod_livestockprocessed_rawdata <- read_csv(file.path(fp_fao, 'Production_LivestockProcessed_E_All_Data_(Normalized).csv'))


# Clean FAO data ----------------------------------------------------------


