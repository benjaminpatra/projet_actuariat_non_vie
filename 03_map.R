# Import packages ---------------------------------------------------------

library(tidyverse)
library(sp)
library(sf)
library(RColorBrewer)
library(viridis)


# Import data -------------------------------------------------------------

departement_shape <- st_read("data/DEPARTEMENTS/DEPARTMENTS.shp")

#data <- 

data_by_dep <- data_freq %>%  
  group_by(DEP) %>% 
  summarise(nb = sum(claim_nb), mean_claim = mean(claim_amount)) %>% 
  left_join(population_dep , by = c("DEP" = "Code.département")) %>% 
  mutate(nb_pour_10000 = (nb / Population.totale) * 100000)

data_plot <- data_by_dep %>% 
  left_join(departement_shape, 
          by = c("DEP" = "DEPT")) 


data_plot <- st_set_geometry(data_plot,data_plot$geometry)

plot(data_plot["nb_pour_10000"], pal = magma(10, direction = -1))

plot(data_plot["nb"], pal = magma(13, direction = -1))

claim_by_dep <- data_claims %>% 
  group_by(DEP) %>% 
  summarise(mean_claim = mean(claim_amount)) %>% 
  left_join(departement_shape, 
            by = c("DEP" = "DEPT"))

claim_by_dep <- st_set_geometry(claim_by_dep,claim_by_dep$geometry)

plot(claim_by_dep["mean_claim"],pal = magma(14, direction = -1))


            
                   