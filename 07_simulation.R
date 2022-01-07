# Import packages ---------------------------------------------------------

library(tidyverse)

# Import data and functions -----------------------------------------------

data_year_1_prime <- readRDS("data_year_1_prime.rds")
source("nonparam-bootstrap-aggclaim.R")

# Simulation taux de chargement -------------------------------------------

data_year_1_freq <- data_year_1_prime %>% 
  select(- result_model_sev) %>% 
  rename(claim_nb = result_model_freq)
  
data_year_1_sev <- data_year_1_prime %>% 
  select(- result_model_freq) %>% 
  rename(claim_amount = result_model_sev)

result_charge_simulation <- rclaimagg(1e3, data_year_1_freq, data_year_1_sev)

# Determination de la fonction de repartition associée
