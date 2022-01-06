library('MASS')
library('glmnet')
library(pscl)
library(tidyverse)

data_freq_year0 <-readRDS("data/data_freq_year0.rds")

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

linreg = glm(claim_nb ~.,family = negative.binomial(1/0.229), data = data_freq)
linreg_AIC <- stepAIC(linreg, direction = 'both')
summary(linreg_AIC)



[1] "id_year"          "id_policy"        "drv_age1"         "drv_age_lic1"     "drv_sex1"        
[6] "drv_age2"         "drv_age_lic2"     "drv_sex2"         "drv_drv2"         "id_client"       
[11] "vh_age"           "vh_cyl"           "vh_din"           "vh_fuel"          "vh_make"         
[16] "vh_model"         "vh_sale_begin"    "vh_sale_end"      "vh_speed"         "vh_type"         
[21] "vh_value"         "vh_weight"        "id_vehicle"       "pol_bonus"        "pol_coverage"    
[26] "pol_duration"     "pol_sit_duration" "pol_pay_freq"     "pol_payd"         "pol_usage"       
[31] "pol_insee_code"   "DEP"              "REG"              "REG_LABEL"        "departement"     
[36] "densite"          "bonus"            "drv_age1_G1"      "drv_age1_G2"      "drv_age1_G3"     
[41] "drv_age_lic1_G"   "drv_age2_G1"      "drv_age2_G2"      "drv_age2_G3"      "drv_age_lic2_G"  
[46] "vh_age_G1"        "vh_age_G2"        "vh_age_G3"        "vh_value_G1"      "vh_value_G2"     
[51] "vh_value_G3"      "risk_class"       "risk_class_G"     "vh_cyl_G"         "vh_make_G"       
[56] "Ldensite"         "Lvh_value"        "Lvh_weight"       "Lvh_cyl"          "claim_nb"
