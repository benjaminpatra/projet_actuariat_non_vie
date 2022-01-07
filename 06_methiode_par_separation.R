# Import packages ---------------------------------------------------------

library(tidyverse)

# Import data -------------------------------------------------------------

data_claim <- readRDS("data/data_claims_year0.rds")
data_freq <- readRDS("data/data_freq_year0.rds")

data_year1 <- readRDS("data/data_complete_year1.rds")
model_freq <-  readRDS("data/model_fpois2_f_freq.rds")
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

fgamma_inf <- glm(claim_amount ~ drv_age1+
                 pol_coverage+
                 pol_pay_freq+
                 vh_age_G2+
                 vh_value_G3,
               family=Gamma("log"), data=data_claim_attritionel)
summary(fgamma)

################################
#Ajouter pour comparer mais à supp
################################

#modèle de sévérité
result_model_sev_inf <- predict(fgamma_inf, newdata = data_year1, type="response" )

result_model_sev_inf <- as.data.frame(result_model_sev_inf)


#modèle de fréquence
result_model_freq <- predict(model_freq, newdata = data_year1, type="response" )
result_model_freq <- as.data.frame(result_model_freq)

#Combinaison des résultats
data_year_1_prime <- cbind(data_year1,result_model_freq,result_model_sev_inf)

#Calcul de la prime
data_year_1_prime <- data_year_1_prime %>% 
  mutate(prime_pure = result_model_freq * result_model_sev_inf + surprime)
