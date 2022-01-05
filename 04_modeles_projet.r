# Import packages ---------------------------------------------------------

library(MASS)
library(tidyverse)
library(pscl)
library(corrplot)
source("./codes/02_plot_function.R")

# Import data -------------------------------------------------------------

data_claims_year0 <- readRDS("data/data_claims_year0.rds")
data_freq_year0 <-readRDS("data/data_freq_year0.rds")


# Correlation -------------------------------------------------------------
data_freq_num <- select_if(data_freq_year0, is.numeric)
corr = cor(data_freq_num,use = "complete.obs")
corrplot(corr, type="upper", tl.col="black", tl.srt=45)

# Modeles de frequence ----------------------------------------------------

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
plotgroupresiduals(fpois2_f, m=10)


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
densite
drv_age2
vh_cyl_G

fnb_i <- glm(claim_nb ~ bonus+drv_age1+pol_coverage+pol_pay_freq+
               pol_payd+risk_class_G+vh_age_G2+vh_fuel+
               vh_make_G+vh_value_G3, 
             family=negative.binomial(phi), data=data_freq_year0)
summary(fnb_i)
AIC(fnb_i)
BIC(fnb_i)
logLik(fnb_i)


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

glmreg = glm(claim_nb ~.,family = negative.binomial(1/0.229), data = data_freq)
glmreg_AIC <- stepAIC(glmreg, direction = 'both')
summary(glmreg_AIC)

"bonus"            "claim_nb"         "densite"          "DEP"              "departement"      "drv_age_lic1"     "drv_age_lic1_G"   "drv_age_lic2"    
"drv_age_lic2_G"   "drv_age1"         "drv_age1_G1"      "drv_age1_G2"      "drv_age1_G3"      "drv_age2"         "drv_age2_G1"      "drv_age2_G2"     
"drv_age2_G3"      "drv_drv2"         "drv_sex1"         "drv_sex2"         "id_client"        "id_policy"        "id_vehicle"       "id_year"         
"Ldensite"         "Lvh_cyl"          "Lvh_value"        "Lvh_weight"       "pol_bonus"        "pol_coverage"     "pol_duration"     "pol_insee_code"  
"pol_pay_freq"     "pol_payd"         "pol_sit_duration" "pol_usage"        "REG"              "REG_LABEL"        "risk_class"       "risk_class_G"    
"vh_age"           "vh_age_G1"        "vh_age_G2"        "vh_age_G3"        "vh_cyl"           "vh_cyl_G"         "vh_din"           "vh_fuel"         
"vh_make"          "vh_make_G"        "vh_model"         "vh_sale_begin"    "vh_sale_end"      "vh_speed"         "vh_type"          "vh_value"        
"vh_value_G1"      "vh_value_G2"      "vh_weight"     

