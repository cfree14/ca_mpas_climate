# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "/Users/cfree/Library/CloudStorage/GoogleDrive-cfree@ucsb.edu/My Drive/current-projects/ca_mpas_climate/data/cpfv"

# Read CPFV data
data_orig <- readRDS(file.path(datadir, "CDFW_2000_2020_cpfv_logbook_data.Rds"))


# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Remove observations missing species info
  filter(!is.na(comm_name)) %>% 
  # Simplify
  select(logbook_id_use, date, 
         target_species, fishing_method, bait_used, 
         block_id, depth_ft, temp_f, 
         hrs_fished, n_fishers, 
         species_code, comm_name, sci_name, 
         n_kept, n_released) %>% 
  # Calculate CPUE
  mutate(fisher_hrs = n_fishers * hrs_fished,
         n_caught=n_kept + n_released,
         cpue=n_caught/fisher_hrs) %>% 
  # Remove observations missing effort info
  filter(fisher_hrs!=0)

# Species key
spp_key <- data %>% 
  group_by(species_code, comm_name, sci_name) %>% 
  summarize(n_caught=sum(n_caught)) %>% 
  ungroup()

# Export
saveRDS(data, file=file.path(datadir, "2000_2020_cpfv_cpue_data.Rds"))



