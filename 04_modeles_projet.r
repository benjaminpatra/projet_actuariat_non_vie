# Import packages ---------------------------------------------------------

library(MASS)
library(tidyverse)
library(pscl)
library(corrplot)
source("./codes/02_plot_function.R")
source("02_plot_function.R")

# Import data -------------------------------------------------------------

data_claims_year0 <- readRDS("../data/data_claims_year0.rds")
data_claims_ecrete_year0 <- readRDS("../data/data_claims_ecrete_year0.rds")
data_freq_year0 <-readRDS("../data/data_freq_year0.rds")


# Correlation -------------------------------------------------------------
data_freq_num <- select_if(data_freq_year0, is.numeric)
corr = cor(data_freq_num,use = "complete.obs")
corrplot(corr, type="upper", tl.col="black", tl.srt=45)



# Liste des variables initiales 
# bonus+
# densite+
# drv_age1+
# drv_age2+
# pol_coverage+
# pol_pay_freq+
# pol_payd+
# REG_LABEL+
# risk_class_G+
# vh_age_G2+
# vh_cyl_G+
# vh_fuel+
# vh_make_G+
# vh_value_G3

# Puis on applique une methode descendante : 
# On supprime les variables les moins significatives en premier, 
# au fur et à mesure, jusqu'a avoir uniquement des vars significatives


#*******************************************************************#
# Modèles de frequence -----------------
#*******************************************************************#

### 1) Log Poisson --------------------------------------------------------

# Premier essai : sans prendre en compte la region
# On supprime dans l'ordre :
# pol_usage 
# vh_type
# densite
# drv_age2
# vh_cyl

fpois1_f <- glm(claim_nb ~ bonus+drv_age1+pol_coverage+pol_pay_freq+
                  pol_payd+risk_class_G+vh_age_G2+vh_fuel+
                  vh_make_G+vh_value_G1,
                family=poisson("log"), data = data_freq_year0)
summary(fpois1_f)
results_model(fpois1_f, m=10)


# Deuxième test (Ajout de la region + modif de vh_value)
# Suppression successive des var 
# pol_usage
# vh_type
# drv_age2
# vh_cyl_G
# Region et densite sont aussi en leves car peu significatifs

fpois2_f <- glm(claim_nb ~ bonus+drv_age1+pol_coverage+pol_pay_freq+
                        pol_payd+risk_class_G+vh_age_G2+vh_fuel+
                        vh_make_G+vh_value_G3,
                      family=poisson("log"), data = data_freq_year0)
summary(fpois2_f)
results_model(fpois2_f)
saveRDS(fpois2_f, "data/model_fpois2_f_freq.rds")


### 2) Quasi Poisson -----------------------------------------------------------
# On supprime successivement 
# pol_usage
# vh_type
# drv_age2
# vh_cyl_G
# REG_LABEL
# densite

fqpois_f <- glm(claim_nb ~ bonus+drv_age1+pol_coverage+pol_pay_freq+
                  pol_payd+risk_class_G+vh_age_G2+vh_fuel+
                  vh_make_G+vh_value_G3,
                family=quasipoisson("log"), data=data_freq_year0)
summary(fqpois_f)
summary(fqpois_f)$dispersion #phi
results_model(fqpois_f)


### 3) Modele binomiale negative theta fixe ------------------------------------------------
# theta = phi determine en faisant une regression linéaire
mean_variance(data_freq_year0, 'vh_age', 'claim_nb')
phi <- 1/0.229

# On supprime successivement 
# pol_usage
# vh_type
# REG_LABEL
# densite
# drv_age2
# vh_cyl_G

fnb_f <- glm(claim_nb ~ bonus+drv_age1+pol_coverage+pol_pay_freq+
               pol_payd+risk_class_G+vh_age_G2+vh_fuel+
               vh_make_G+vh_value_G3, 
             family=negative.binomial(phi), data=data_freq_year0)
summary(fnb_f)
results_model(fnb_f)


### 4) Modele binomiale negative theta estime ------------------------------
fnb2_f <- glm.nb(claim_nb ~ bonus+drv_age1+pol_coverage+pol_pay_freq+
               pol_payd+risk_class_G+vh_age_G2+vh_fuel+
               vh_make_G+vh_value_G3, data=data_freq_year0)
summary(fnb2_f)
xtable(summary(fnb2_f))
results_model(fnb2_f, m =80)
pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "bonus")
pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "drv_age1")
pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "drv_age1_G1")
pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "pol_coverage")
pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "pol_pay_freq")
pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "pol_payd")
pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "risk_class_G")
pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "vh_age_G2")
pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "vh_fuel")
pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "vh_make_G")
pred_group(model = fnb2_f, data= data_freq_year0, "claim_nb", "vh_value_G3")


### Step AIC ----------------------------------------------------------------

data_freq <- data_freq_year0 %>% 
  select(claim_nb,
         drv_age1,
         drv_age_lic1,
         vh_cyl_G,
         drv_age2,
         vh_fuel,
         bonus,
         densite,
         pol_coverage,
         pol_pay_freq,
         pol_payd,
         REG_LABEL,
         risk_class_G,
         vh_age_G2,
         vh_make_G,
         vh_value_G3)

glmreg = glm(claim_nb ~.,family = negative.binomial(phi), data = data_freq)
glmreg_AIC <- stepAIC(glmreg, direction = 'both')
summary(glmreg_AIC)
results_model(glmreg_AIC)

glmreg_p = glm(claim_nb ~.,family = poisson("log"), data = data_freq)
glmreg_p_AIC <- stepAIC(glmreg_p, direction = 'both')
summary(glmreg_p_AIC)
results_model(glmreg_p_AIC)


### 5) Zero modifie P logit----
fzmP_logit <- hurdle(claim_nb ~ bonus+drv_age1+pol_coverage+pol_pay_freq+
                       pol_payd+risk_class_G+vh_age_G2+vh_fuel+
                       vh_make_G+vh_value_G3, 
                     dist = "poisson", link = "logit",
                     data=data_freq)
summary(fzmP_logit) 
results_model(fzmP_logit, m = 50, dev = F)


### 6) Zero modifie P probit----
fzmP_probit <- hurdle(claim_nb ~ bonus+drv_age1+pol_coverage+pol_pay_freq+
                        pol_payd+risk_class_G+vh_age_G2+vh_fuel+
                        vh_make_G+vh_value_G3, 
                      dist = "poisson", link = "probit",
                      data=data_freq)
summary(fzmP_probit)
results_model(fzmP_probit, m = 50, dev = F)


### 7) Zero modifie P cloglog ----
fzmP_clolog <- hurdle(claim_nb ~ bonus+drv_age1+pol_coverage+pol_pay_freq+
                        pol_payd+risk_class_G+vh_age_G2+vh_fuel+
                        vh_make_G+vh_value_G3, 
                      dist = "poisson", link = "cloglog",
                      data=data_freq)
summary(fzmP_clolog) 
results_model(fzmP_clolog, m = 50, dev = F)


### 8) Zero modifie P cauchit ----
fzmP_cauchit <- hurdle(claim_nb ~ bonus+drv_age1+pol_coverage+pol_pay_freq+
                         pol_payd+risk_class_G+vh_age_G2+vh_fuel+
                         vh_make_G+vh_value_G3, 
                       dist = "poisson", link = "cauchit",
                       data=data_freq)
summary(fzmP_cauchit) 
results_model(fzmP_cauchit, m = 50, dev = F)


### 9) Zero modifie NB logit----
fzmNB_logit <- hurdle(claim_nb ~ bonus+drv_age1+pol_coverage+pol_pay_freq+
                        pol_payd+risk_class_G+vh_age_G2+vh_fuel+
                        vh_make_G+vh_value_G3, 
                      dist = "negbin", link = "logit",
                      data=data_freq)
summary(fzmNB_logit) # Presque bon
results_model(fzmNB_logit, m = 50, dev = F)



#*******************************************************************#
# Modèles pour la sévérité -----------------
#*******************************************************************#

# -------------------------------------------------------------------------------#
### A) MODELES SIMPLES : --------------------------------------------------------
# -------------------------------------------------------------------------------#
### 1) Modele Gamma lien log ----
fgamma <- glm(claim_amount ~ drv_age1 +Ldensite,
              family=Gamma("log"), data=data_claims_ecrete_year0)
summary(fgamma)
plotgroupresiduals(fgamma, m=1, trim = F) #, trim = F, m =1

#On supprime successivement 
# bonus et vh_make_G 
# risk_class_G
#drv_age2 vh_cyl pol_payd densite vh_fuel
fgamma2 <- glm(claim_amount ~ drv_age1+
                 pol_coverage+
                 pol_pay_freq+
                 vh_age_G2+
                 vh_value_G3,
               family=Gamma("log"), data=data_claims_ecrete_year0)
summary(fgamma2)
results_model(fgamma2, m = 50)

saveRDS(fgamma2,"data/model_fgamma2_sev.rds")


### 2) Modele Inverse gaussienne lien log ----
fig_log <- glm(claim_amount ~ drv_age1+
                 pol_coverage+
                 pol_pay_freq+
                 vh_age_G2+
                 vh_value_G3,
               family=inverse.gaussian("log"), data=data_claims_ecrete_year0, maxit = 50)
summary(fig_log)
results_model(fig_log, m = 1, trim = F)


### 3) Modele Log Normal ----
flnorm <- lm(log(claim_amount) ~ drv_age1+
                 pol_coverage+
                 pol_pay_freq+
                 vh_age_G2+
                 vh_value_G3,
               data=data_claims_ecrete_year0)
summary(flnorm)
results_model(flnorm, m = 1, trim = F, dev = F)


# -------------------------------------------------------------------------------#
### B) MODELES ECRETES : --------------------------------------------------------
# -------------------------------------------------------------------------------#

### 1) Modele Gamma lien log ----
fgamma_e <- glm(claim_amount_ecrete ~ drv_age1 +Ldensite,
              family=Gamma("log"), data=data_claims_ecrete_year0)
summary(fgamma_e)
results_model(fgamma_e, m = 1, trim = F)


fgamma2_e <- glm(claim_amount_ecrete ~ drv_age1+
               pol_coverage+
               pol_pay_freq+
               vh_age_G2+
               vh_value_G3,
              family=Gamma("log"), data=data_claims_ecrete_year0)
summary(fgamma2_e)
results_model(fgamma2_e, m = 1, trim = F)

saveRDS(fgamma2_e,"data/model_fgamma2_sev_e.rds")
#On supprime bonus et vh_make_G 
# risk_class_G
#drv_age2 vh_cyl pol_payd densite vh_fuel


### 2) Modele Inverse gaussienne lien log ----
fig_log_e <- glm(claim_amount_ecrete ~ drv_age1+
                 pol_coverage+
                 pol_pay_freq+
                 vh_age_G2+
                 vh_value_G3,
               family=inverse.gaussian("log"), data=data_claims_ecrete_year0, maxit = 50)
summary(fig_log_e)
results_model(fig_log_e, m = 1, trim = F)

#on elimine 
#drv_age2, bonus,vh_cyl,densite,pol_payd,vh_make_G,risk_class_G,vh_fuel

### 3) Modele Inverse gaussienne lien log ----
fig_mu_e <- glm(claim_amount_ecrete ~ drv_age1+
                   pol_coverage+
                   pol_pay_freq+
                   vh_age_G2+
                   vh_value_G3,
                 family=inverse.gaussian("1/mu^2"), data=data_claims_ecrete_year0, maxit = 50)
summary(fig_mu_e)
results_model(fig_mu_e, m = 1, trim = F)


### 4) Modele Log Normal ----
flnorm_e <- lm(log(claim_amount_ecrete) ~ drv_age1+
                   pol_coverage+
                   pol_pay_freq+
                   vh_age_G2+
                   vh_value_G3,
                  data=data_claims_ecrete_year0)
summary(flnorm_e)
results_model(flnorm_e, m = 1, trim = F, dev = F)


# -------------------------------------------------------------------------------#
### C) MODELES PAR SEPARATION : ------------------------------------------------
# -------------------------------------------------------------------------------#
# On fixe le paramètre de valeur limite -----------------------------------
valeur_limite <- as.numeric(quantile(data_claims_year0$claim_amount,0.995))

# Separation des données selon la valeur limite
data_claim_attritionel <- data_claims_year0 %>% 
  filter(claim_amount <= valeur_limite)

data_claim_extreme <- data_claims_year0 %>% 
  filter(claim_amount > valeur_limite)


### 1) Modele Gamma lien log ----
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
xtable(summary(fgamma_s))
pred_group(model = fgamma_s, data= data_claim_attritionel, "claim_amount", "drv_age1")
pred_group(model = fgamma_s, data= data_claim_attritionel, "claim_amount", "pol_coverage")
pred_group(model = fgamma_s, data= data_claim_attritionel, "claim_amount", "pol_pay_freq")
pred_group(model = fgamma_s, data= data_claim_attritionel, "claim_amount", "vh_age_G2")
pred_group(model = fgamma_s, data= data_claim_attritionel, "claim_amount", "vh_value_G3")


### 2) Modele Inverse Gaussienne lien Log----
figl_s <- glm(claim_amount ~ drv_age1+
               pol_coverage+
               pol_pay_freq+
               vh_age_G2+
               vh_value_G3,
             family=inverse.gaussian("log"), data=data_claim_attritionel)
results_model(figl_s)

### 3) Modele Inverse Gaussienne lien 1/mu2----
fig_s <- glm(claim_amount ~ drv_age1+
                  pol_coverage+
                  pol_pay_freq+
                  vh_age_G2+
                  vh_value_G3,
                family=inverse.gaussian("1/mu^2"), data=data_claim_attritionel)
results_model(fig_s)


### 4) Modele Log Normal----
fLN_s <- lm(log(claim_amount) ~ drv_age1+
               pol_coverage+
               pol_pay_freq+
               vh_age_G2+
               vh_value_G3,
             data=data_claim_attritionel)
results_model(fLN_s, dev = F)
