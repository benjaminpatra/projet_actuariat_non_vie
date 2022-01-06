# Import packages ---------------------------------------------------------
library(tidyverse)


# Import data -------------------------------------------------------------

data_claims <- readRDS("data/data_claims_year0.rds")


# Ecrêtement des sinistres ------------------------------------------------

# On définit notre valeur d'écrêtement
hist(data_claims$claim_amount)

nrow(data_claims %>%  filter(claim_amount > 10317.52))
quantile(data_claims$claim_amount,c(0.90,0.95,0.995,0.999))

data_hist <- data_claims %>%  filter(claim_amount < 10317.52)
hist(data_hist$claim_amount)

# nous décidons d'utiliser le quantile à 99.5%
valeur_limite <- quantile(data_claims$claim_amount,0.995)

charge_surcrete <- sum(data_claims %>% mutate(surcrete = pmax(claim_amount - valeur_limite, 0)) %>% select(surcrete))

data_claims <- data_claims %>% 
  mutate(claim_amount_ecrete = pmin(claim_amount, valeur_limite) + charge_surcrete / nrow(data_claims),
         log_claim_amount_ecrete = log(claim_amount_ecrete))

#On teste que l'on retrouve bien le même total pour claim_amount et claim_amount_ecrete
sum(data_claims$claim_amount) == sum(data_claims$claim_amount_ecrete)


# On sauvegarder la base obtenur ------------------------------------------

saveRDS(data_claims,"data/data_claims_ecrete_year0.rds")
