# Import packages ---------------------------------------------------------

library(tidyverse)
library(actuar)
library(evir)

# Import data -------------------------------------------------------------

data_claim <- readRDS("data/data_claims_year0.rds")

valeur_limite <- as.numeric(quantile(data_claim$claim_amount,0.995))

data_claim_inferieur <- data_claim %>% 
  filter(claim_amount <= valeur_limite)

data_claim_superieur <- data_claim %>% 
  filter(claim_amount > valeur_limite)


m = mean(data_claim_superieur$claim_amount)
v = var(data_claim_superieur$claim_amount)
alpha <- 2/(1-m^2/v)

surprime <- (nrow(data_claim_superieur) / nrow(data_claim)) * alpha*valeur_limite / (alpha - 1)
surprime


fgamma <- glm(claim_amount ~ drv_age1+
                 pol_coverage+
                 pol_pay_freq+
                 vh_age_G2+
                 vh_value_G3,
               family=Gamma("log"), data=data_claim_inferieur)
summary(fgamma)
