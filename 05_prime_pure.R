# Import packages ---------------------------------------------------------

library(tidyverse)

# Import Data -------------------------------------------------------------

data_year1 <- readRDS("data/data_complete_year1.rds")
data_claim <- readRDS("data/data_claims_year0.rds")
data_freq <- readRDS("data/data_freq_year0.rds")

# Import model ------------------------------------------------------------

model_freq <-  readRDS("data/binomial_neg.rds")
model_sev <- readRDS("data/fgamma_separe.rds")

# Run model on data_YEAR_1 ------------------------------------------------


# Modèle de fréquence

result_model_freq <- predict(model_freq, newdata = data_year1, type="response" )
#hist(result_model_freq)
result_model_freq <- as.data.frame(result_model_freq)

# Modèle de sévérité

result_model_sev <- predict(model_sev, newdata = data_year1, type="response" )
#hist(result_model_sev)
result_model_sev <- as.data.frame(result_model_sev)

# Combinaison des résultats
data_year_1_prime <- cbind(data_year1,result_model_freq,result_model_sev)

# Calcul de E[B].E[N]
data_year_1_prime <- data_year_1_prime %>% 
  mutate(prime_pure = result_model_freq * result_model_sev)

# Nous avons fait le choix d'utilisé la méthode de séparation.
# Il faut donc déterminer la surprime à ajouter.

# On fixe le paramètre de valeur limite -

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

# On ajoute la surprime :

data_year_1_final <- data_year_1_prime %>% 
  mutate(prime_pure = prime_pure + surprime)

hist(data_year_1_prime$prime_pure)

#sauvergarde
saveRDS(data_year_1_final,"data/data_year_1_prime.rds")



boxplot(data_year_1_final$prime_pure~data_year_1_prime$drv_age1_G2,varwidth = TRUE, notch = TRUE, outline = TRUE)
boxplot(data_year_1_final$prime_pure~data_year_1_prime$vh_age,varwidth = TRUE, notch = TRUE, outline = TRUE)
boxplot(data_year_1_final$prime_pure~data_year_1_prime$pol_coverage,varwidth = TRUE, notch = TRUE, outline = TRUE)
