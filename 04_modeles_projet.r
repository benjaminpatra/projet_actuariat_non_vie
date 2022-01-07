# Import packages ---------------------------------------------------------

library(MASS)
library(tidyverse)
library(pscl)
library(corrplot)
source("./codes/02_plot_function.R")
source("02_plot_function.R")

# Import data -------------------------------------------------------------

data_claims_year0 <- readRDS("data/data_claims_year0.rds")
data_claims_ecrete_year0 <- readRDS("data/data_claims_ecrete_year0.rds")
data_freq_year0 <-readRDS("data/data_freq_year0.rds")


# Correlation -------------------------------------------------------------
data_freq_num <- select_if(data_freq_year0, is.numeric)
corr = cor(data_freq_num,use = "complete.obs")
corrplot(corr, type="upper", tl.col="black", tl.srt=45)


#*******************************************************************#
# Modèle de frequence -----------------
#*******************************************************************#

### 1) Log Poisson --------------------------------------------------------

# Premier essai
fpois1_i <- glm(claim_nb ~ bonus+densite+drv_age1+pol_coverage+pol_pay_freq+
                pol_payd+risk_class_G+vh_age_G2+vh_cyl_G+vh_fuel+
                vh_make_G+vh_value_G1,
              family=poisson("log"), data = data_freq_year0)
summary(fpois1_i)

#methode descendante : On supprime les variiables non significative au fur et à mesure en commencant par les - significatives 
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
AIC(fpois1_f)
BIC(fpois1_f)
logLik(fpois1_f)
plotgroupresiduals(fpois1_f, m=10) #, trim = F, m =1

# Deuxième test (Ajout de la region + modif de vh_value)
fpois2_i <- glm(claim_nb ~ bonus+densite+drv_age1+drv_age2+pol_coverage+pol_pay_freq+
                        pol_payd+pol_usage+REG_LABEL+risk_class_G+vh_age_G2+vh_cyl_G+vh_fuel+
                        vh_make_G+vh_type+vh_value_G3,
                      family=poisson("log"), data = data_freq_year0)
summary(fpois2_i)

# Suppression successive des var suivantes
# pol_usage
# vh_type
# drv_age2
# vh_cyl_G
# Region et densite peuvent egalemment etre enleves ou gerdes comme on veut

fpois2_f <- glm(claim_nb ~ bonus+densite+drv_age1+pol_coverage+pol_pay_freq+
                        pol_payd+REG_LABEL+risk_class_G+vh_age_G2+vh_fuel+
                        vh_make_G+vh_value_G3,
                      family=poisson("log"), data = data_freq_year0)
summary(fpois2_f)
AIC(fpois2_f)
BIC(fpois2_f)
logLik(fpois2_f)
plotgroupresiduals(fpois2_f, m= 30)

saveRDS(fpois2_f, "data/model_fpois2_f_freq.rds")
### 2) Quasi Poisson -----------------------------------------------------------
fqpois_i <- glm(claim_nb ~ bonus+densite+drv_age1+drv_age2+pol_coverage+pol_pay_freq+
                  pol_payd+REG_LABEL+risk_class_G+vh_age_G2+vh_cyl_G+vh_fuel+
                  vh_make_G+vh_value_G3,
               family=quasipoisson("log"), data=data_freq_year0)
summary(fqpois_i)

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
plotgroupresiduals(fqpois_f, m = 30) #, trim = F, m =1


### 3) Modele binomiale negative ------------------------------------------------
# Première méthode : fixer theta hat = phi en faisant une regression linéaire
mean_variance(data_freq_year0, 'vh_age', 'claim_nb')

phi <- 1/0.229
fnb_i <- glm(claim_nb ~ bonus+densite+drv_age1+drv_age2+pol_coverage+pol_pay_freq+
              pol_payd+REG_LABEL+risk_class_G+vh_age_G2+vh_cyl_G+vh_fuel+
              vh_make_G+vh_value_G3, 
            family=negative.binomial(phi), data=data_freq_year0)
summary(fnb_i)

# REG_LABEL
# densite
# drv_age2
# vh_cyl_G

fnb_f <- glm(claim_nb ~ bonus+drv_age1+pol_coverage+pol_pay_freq+
               pol_payd+risk_class_G+vh_age_G2+vh_fuel+
               vh_make_G+vh_value_G3, 
             family=negative.binomial(phi), data=data_freq_year0)
summary(fnb_f)
AIC(fnb_f)
BIC(fnb_f)
logLik(fnb_f)
plotgroupresiduals(fnb_f, m = 30) #, trim = F, m =1



# Step AIC ----------------------------------------------------------------

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

glmreg_p = glm(claim_nb ~.,family = poisson("log"), data = data_freq)
glmreg_p_AIC <- stepAIC(glmreg_p, direction = 'both')
summary(glmreg_p_AIC)


#*******************************************************************#
# Modèle pour la sévérité -----------------
#*******************************************************************#

# -------------------------------------------------------------------------------#
### A) MODELES SIMPLES : --------------------------------------------------------
# -------------------------------------------------------------------------------#
### 1) Modele Gamma lien log ----
fgamma <- glm(claim_amount ~ drv_age1 +Ldensite,
              family=Gamma("log"), data=data_claims_ecrete_year0)
summary(fgamma)
plotgroupresiduals(fgamma, m=1, trim = F) #, trim = F, m =1


fgamma2 <- glm(claim_amount ~ drv_age1+
                 pol_coverage+
                 pol_pay_freq+
                 vh_age_G2+
                 vh_value_G3,
               family=Gamma("log"), data=data_claims_ecrete_year0)
summary(fgamma2)
AIC(fgamma2)
BIC(fgamma2)
logLik(fgamma2)

saveRDS(fgamma2,"data/model_fgamma2_sev.rds")
#On supprime bonus et vh_make_G 
# risk_class_G
#drv_age2 vh_cyl pol_payd densite vh_fuel


### 3) Modele Inverse gaussienne lien log ----
fig_log <- glm(claim_amount ~ drv_age1+
                 pol_coverage+
                 pol_pay_freq+
                 vh_age_G2+
                 vh_value_G3,
               family=inverse.gaussian("log"), data=data_claims_ecrete_year0, maxit = 50)
summary(fig_log)
plotgroupresiduals(fig_log, m=1, trim = F) #, trim = F, m =1

# -------------------------------------------------------------------------------#
### B) MODELES ECRETES : --------------------------------------------------------
# -------------------------------------------------------------------------------#

### 1) Modele Gamma lien log ----
fgamma_e <- glm(claim_amount_ecrete ~ drv_age1 +Ldensite,
              family=Gamma("log"), data=data_claims_ecrete_year0)
summary(fgamma_e)
plotgroupresiduals(fgamma, m=1, trim = F) #, trim = F, m =1


fgamma2_e <- glm(claim_amount_ecrete ~ drv_age1+
               pol_coverage+
               pol_pay_freq+
               vh_age_G2+
               vh_value_G3,
              family=Gamma("log"), data=data_claims_ecrete_year0)
summary(fgamma2_e)

saveRDS(fgamma2_e,"data/model_fgamma2_sev_e.rds")
#On supprime bonus et vh_make_G 
# risk_class_G
#drv_age2 vh_cyl pol_payd densite vh_fuel



### 2) Modele log Gamma ----
f_loggamma_e <- glm(log_claim_amount_ecrete ~ drv_age1+
                 pol_coverage+
                 pol_pay_freq+
                 vh_age_G2+
                 vh_value_G3,
               family=Gamma("identity"), data=data_claims_ecrete_year0)
summary(f_loggamma_e)

### 3) Modele Inverse gaussienne lien log ----

fig_log_e <- glm(claim_amount_ecrete ~ drv_age1+
                 pol_coverage+
                 pol_pay_freq+
                 vh_age_G2+
                 vh_value_G3,
               family=inverse.gaussian("log"), data=data_claims_ecrete_year0, maxit = 50)
summary(fig_log_e)
plotgroupresiduals(fig_log_e, m=1, trim = F) #, trim = F, m =1

#on élimine drv_age2, bonus,vh_cyl,densite,pol_payd,vh_make_G,risk_class_G,vh_fuel
 
### 4)
