###############################################################################
# Voici le code a lancer afin d'obtenir notre prime pure et prime commerciale #
# E. Maucuer & B. Patra
# Projet realise sous R version 4.1.2
###############################################################################
# Import Packages

library(MASS) # version 7.3.54
library(tidyverse) # version 1.3.1

# Preparation des donnees 
source("codes/01_import_and_prepare_data.R",encoding = "utf-8")

# Fonction de visualisation
source("codes/02_plot_function.R")

# Pour voir l'analyse des donnees : ouvrir le html 03_analyse.html

# Import des donnees necessaires a la modelisation

data_freq_year0 <-readRDS("data/data_freq_year0.rds")
data_claims_year0 <- readRDS("./data/data_claims_year0.rds")

### Modele de Frequence: Modele binomiale negative theta estime ---------------

fnb2_f <- glm.nb(claim_nb ~ bonus+drv_age1+pol_coverage+pol_pay_freq+
                   pol_payd+risk_class_G+vh_age_G2+vh_fuel+
                   vh_make_G+vh_value_G3, data=data_freq_year0)
summary(fnb2_f)
#xtable(summary(fnb2_f))
results_model(fnb2_f, m =80)
# pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "bonus")
# pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "drv_age1")
# pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "drv_age1_G1")
# pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "pol_coverage")
# pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "pol_pay_freq")
# pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "pol_payd")
# pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "risk_class_G")
# pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "vh_age_G2")
# pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "vh_fuel")
# pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "vh_make_G")
# pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "vh_value_G3")

saveRDS(fnb2_f,"data/binomial_neg.rds")


### Modele de severite : ------------------------------------------------
# On fixe le paramètre de valeur limite
valeur_limite <- as.numeric(quantile(data_claims_year0$claim_amount,0.995))

# Separation des données selon la valeur limite
data_claim_attritionel <- data_claims_year0 %>% 
  filter(claim_amount <= valeur_limite)

data_claim_extreme <- data_claims_year0 %>% 
  filter(claim_amount > valeur_limite)

###  Modele Gamma lien log ----
fgamma_s <- glm(claim_amount ~ #bonus+
                  #densite+
                  drv_age1+
                  #drv_age2+
                  pol_coverage+
                  pol_pay_freq+
                  #pol_payd+
                  #REG_LABEL+ # pourrait etre enleve ?
                  #risk_class_G+
                  vh_age_G2+
                  #vh_cyl_G+
                  #vh_fuel+
                  #vh_make_G
                  vh_value_G3,
                family=Gamma("log"), data=data_claim_attritionel)
summary(fgamma_s)
results_model(fgamma_s)
# xtable(summary(fgamma_s))
# pred_group(model = fgamma_s, data= data_claim_attritionel, "claim_amount", "drv_age1")
# pred_group(model = fgamma_s, data= data_claim_attritionel, "claim_amount", "pol_coverage")
# pred_group(model = fgamma_s, data= data_claim_attritionel, "claim_amount", "pol_pay_freq")
# pred_group(model = fgamma_s, data= data_claim_attritionel, "claim_amount", "vh_age_G2")
# pred_group(model = fgamma_s, data= data_claim_attritionel, "claim_amount", "vh_value_G3")
saveRDS(fgamma_s,"data/fgamma_separe.rds")

# Les autres lois calibrees sont disponibles dans le script 04_modeles_projet.R


### Prime Pure ----------------------------------------------------------------

source("codes/05_prime_pure.R")

### Simulation et prime commerciale -------------------------------------------

source("codes/06_simulation.R")

