# Import packages ---------------------------------------------------------

library(tidyverse)

# Import data and functions -----------------------------------------------

data_year_1_prime <- readRDS("data/data_year_1_prime.rds")
source("nonparam-bootstrap-aggclaim.R")

# Simulation taux de chargement -------------------------------------------

data_year_1_freq <- data_year_1_prime %>% 
  select(- result_model_sev) %>% 
  rename(claim_nb = result_model_freq)
  
data_year_1_sev <- data_year_1_prime %>% 
  select(- result_model_freq) %>% 
  rename(claim_amount = result_model_sev)

result_charge_simulation <- rclaimagg(1e4, data_year_1_freq, data_year_1_sev)

fonction_repartition <- ecdf(result_charge_simulation)
plot(fonction_repartition)

d  <- density(result_charge_simulation)
plot(d)

quantile(result_charge_simulation,c(0.90,0.95,0.99,0.999))


hist(result_charge_simulation, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "temp",
     main = "Beaver #1")
lines(density(result_charge_simulation), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")
abline(v = mean(result_charge_simulation),
       col = "royalblue",
       lwd = 2)
abline(v = quantile(result_charge_simulation,0.95),
       col = "red",
       lwd = 2)
abline(v = sum(data_year_1_prime$prime_pure),
       col = "green",
       lwd = 2)
legend(x = "topright", # location of legend within plot area
       c("Density plot", "Mean", "Quantile 0.95","Acc sum prime"),
       col = c("chocolate3", "royalblue", "red","green"),
       lwd = c(2, 2, 2))


# Determination de la fonction de repartition associée


