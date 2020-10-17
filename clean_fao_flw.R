# Script to clean FAO food waste data pasted into a CSV from Gustavsson et al 2013
# QDR / 16 Oct 2020

library(tidyverse)

flwraw <- read_csv('data/raw_data/FAOSTAT/fao_world_flw.csv')
names(flwraw) <- c('continent', 'category', 'loss_agricultural_production', 'loss_handling_storage', 'loss_processing_packaging', 'loss_distribution', 'loss_consumption')

# Clean data
flwclean <- flwraw %>%
  mutate(loss_handling_storage = if_else(is.na(loss_handling_storage), '0', loss_handling_storage)) %>% # fill in empty cells for eggs
  pivot_longer(starts_with('loss'), names_to = 'stage') %>% # Make into longform
  separate(value, into = c('value1', 'value2'), sep = ',') %>% # Separate the ones that have two comma separated numbers
  mutate(flag1 = str_extract(value1, '[a-z]'),
         flag2 = str_extract(value2, '[a-z]'),
         value1 = str_extract(value1, '[0-9.]+'),
         value2 = str_extract(value2, '[0-9.]+'))

# For cereals where milling and processing are listed separately, change value to the total loss between milling and processing
# For any category where fresh and processed are listed separately, separate out the rows for fresh and processed

flwclean <- flwclean %>%
  mutate(value1 = as.numeric(value1)/100, value2 = as.numeric(value2)/100) %>% # convert to numeric proportion
  mutate(value1 = if_else(flag1 %in% 'm', 1 - (1 - value1) * (1 - value2), value1)) %>% # combine milling and processing loss
  mutate(value2 = if_else(flag1 %in% 'f', value2, as.numeric(NA))) %>% # Get rid of all value2 except where needed to separate
  pivot_longer(c(value1, value2), names_to = 'type') %>%
  filter(!is.na(value)) %>%
  mutate(type = case_when(flag1 %in% 'f' & type %in% 'value1' ~ 'fresh',
                          flag1 %in% 'f' & type %in% 'value2' ~ 'processed',
                          TRUE ~ as.character(NA))) %>%
  select(-flag1, -flag2) 

# Create a version that fills in a full row of data for fresh and processed versions of each food type.
flwfinal <- flwclean %>%
  pivot_wider(names_from = stage, values_from = value) %>%
  mutate_at(vars(loss_agricultural_production, loss_handling_storage, loss_processing_packaging), ~ zoo::na.locf(.)) %>%
  filter(category %in% c('Cereals', 'Oilseeds & Pulses', 'Meat', 'Milk', 'Eggs') | type %in% c('fresh', 'processed'))

write_csv(cbind(category_number = 1:11, flwfinal), 'data/raw_data/FAOSTAT/fao_world_flw_clean.csv')
