# Import packages ---------------------------------------------------------

library(tidyverse)

# Import Data -------------------------------------------------------------

data_year1 <- readRDS("data/data_complete_year1.rds")

#data_year1 <- data_year1 %>% 
#  mutate(vh_make_G = ifelse(vh_make %in% c("BABOULIN","INFINITI","LDV","LM SOVRA","MATRA","MEGA","NSU","RILEY","UNIMOG"), "autre",vh_make_G))


# Import model ------------------------------------------------------------
model_freq <-  readRDS("data/model_fpois2_f_freq.rds")
model_sev <- readRDS("data/model_fgamma2_sev.rds")


# Run model on data_YEAR_1 ------------------------------------------------

#modèle de fréquence
result_model_freq <- predict(model_freq, newdata = data_year1, type="response" )
hist(result_model_freq)
result_model_freq <- as.data.frame(result_model_freq)
#modèle de sévérité
result_model_sev <- predict(model_sev, newdata = data_year1, type="response" )
hist(result_model_sev)
result_model_sev <- as.data.frame(result_model_sev)


#Combinaison des résultats
data_year_1_prime <- cbind(data_year1,result_model_freq,result_model_sev)

#Calcul de la prime
data_year_1_prime <- data_year_1_prime %>% 
  mutate(prime_pure = result_model_freq * result_model_sev)

hist(data_year_1_prime$prime_pure)

#sauvergarde
saveRDS(data_year_1_prime,"data/data_year_1_prime.rds")




boxplot(data_year_1_prime$prime_pure~data_year_1_prime$drv_age1,varwidth = TRUE, notch = TRUE, outline = TRUE)

boxplot(data_year_1_prime$prime_pure~data_year_1_prime$vh_age,varwidth = TRUE, notch = TRUE, outline = TRUE)
