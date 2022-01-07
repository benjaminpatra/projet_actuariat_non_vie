# Import packages ---------------------------------------------------------

library(tidyverse)

# Import data -------------------------------------------------------------

data_claim <- readRDS("data/data_claims_year0.rds")
data_freq <- readRDS("data/data_freq_year0.rds")

# On fixe le paramètre de valeur limite -----------------------------------

valeur_limite <- as.numeric(quantile(data_claim$claim_amount,0.995))

# Separation des données selon la valeur limite
data_claim_attritionel <- data_claim %>% 
  filter(claim_amount <= valeur_limite)

data_claim_extreme <- data_claim %>% 
  filter(claim_amount > valeur_limite)

# On détermine la valeur alpha d'un modèle de Pareto

m = mean(data_claim_extreme$claim_amount)
v = var(data_claim_extreme$claim_amount)
alpha <- 2/(1-m^2/v)

# On en déduit la surprime à ajouter

surprime <- (nrow(data_claim_extreme) / nrow(data_freq)) * alpha*valeur_limite / (alpha - 1)
surprime

# On calibre alors le modèle que sur les risques attritionnels

fgamma <- glm(claim_amount ~ drv_age1+
                 pol_coverage+
                 pol_pay_freq+
                 vh_age_G2+
                 vh_value_G3,
               family=Gamma("log"), data=data_claim_inferieur)
summary(fgamma)
