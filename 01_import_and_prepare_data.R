# Import packages ---------------------------------------------------------

library(tidyverse)
library(xlsx)

# Import data -------------------------------------------------------------

data_policy <- read_csv("data/ENSAE-15MAPA/15MAPA-PG_2017_YEAR0.csv")
data_claims <- read_csv("data/ENSAE-15MAPA/15MAPA-PG_2017_CLAIMS_YEAR0.csv")

data_year_1  <- read_csv("data/ENSAE-15MAPA/15MAPA-PG_2017_YEAR1.csv")

density_dep <- read.xlsx("data/FPORSOC21-F41.xlsx", sheetIndex = 3, endRow = 104, colIndex = c(1,2,3), header = TRUE, startRow = 3) 

population_dep <- read.xlsx("data/ensemble.xlsx", sheetIndex = 2, startRow = 8, header = TRUE, colIndex = c(3,4,8,9))

reg_dep <- read_csv("data/reg_dep.csv")

# Création d'une variable numéro du département ------------------------------

data_policy <- data_policy %>% 
  mutate(DEP = as.character(str_extract_all(pol_insee_code,"^\\d."))) 


# Ajout de la variable région ---------------------------------------------

data_policy <- data_policy %>% left_join(reg_dep, by = "DEP")

# Ajout de la variable densité de la population par département -----------

data_policy <- data_policy %>% 
  left_join(density_dep, by = "DEP") %>% 
  rename(densite = Densité,
         departement = Département)


# Variable catégorielle de pol_bonus --------------------------------------

data_policy$bonus <- ifelse(data_policy$pol_bonus < 1, "bonus", ifelse(data_policy$pol_bonus == 1, "neutre", "malus"))


# Création de découpages pour les variables continues ---------------------

data_policy$drv_age1_G1 <- cut(data_policy$drv_age1, c(17, 2:8*10, 100))
data_policy$drv_age1_G2 <- cut(data_policy$drv_age1, c(17, seq(from = 25, to = 80, by = 5),100))
data_policy$drv_age1_G3 <- cut(data_policy$drv_age1, c(17, 25, 45,65,80,100))

data_policy$drv_age_lic1_G <- cut(data_policy$drv_age_lic1,c(-1,3,5,100))

data_policy$drv_age2_G1 <- cut(data_policy$drv_age2, c(17, 2:8*10, 100))
data_policy$drv_age2_G2 <- cut(data_policy$drv_age2, c(17, seq(from = 25, to = 80, by = 5),100))
data_policy$drv_age2_G3 <- cut(data_policy$drv_age2, c(17, 25, 45,65,80,100))

data_policy$drv_age_lic2_G <- cut(data_policy$drv_age_lic2,c(-1,3,5,100))

data_policy$vh_age_G1 <- cut(data_policy$vh_age,c(-1, 0:1*5, 100))
data_policy$vh_age_G2 <- cut(data_policy$vh_age,c(-1,10,20,100))
data_policy$vh_age_G3 <- cut(data_policy$vh_age,c(-1,0,1,2,3,4,5,6,7,8,9,10,100))

data_policy$vh_value_G1 <- cut(data_policy$vh_value,c(seq(from = 0, to = 100000, by = 10000),155498))
data_policy$vh_value_G2 <- cut(data_policy$vh_value,c(seq(from = 0, to = 100000, by = 50000),155498 +1))
data_policy$vh_value_G3 <- cut(data_policy$vh_value,c(seq(from = 0, to = 50000, by = 10000),155498+1))


# Création d'une variables risk_class -------------------------------------

data_policy$risk_class <- (data_policy$vh_din*0.736)*100/ (data_policy$vh_weight + 75)
data_policy$risk_class_G  <- cut(data_policy$risk_class, 
                                 breaks=c(quantile(data_policy$risk_class, probs = seq(0, 1, by = 0.20))),labels = 1:5)

data_policy$vh_cyl_G <- cut(data_policy$vh_cyl,c(-1,1500,Inf), labels = 1:2)

# Création d'une variable regroupant les marques de véhicules --------------

autre <- c("ACL", "APAL", "ARO", "DATSUN", "EBRO", "FSO", "GME", "IVECO", "LADA VAZ", "MORRIS",
           "OM", "PANHARD", "PIAGGIO", "PININFARINA", "SANTANA", "SAVIEM", "SIMCA", "SMART", "STEYR PUCH",
           "TALBOT", "TEILHOL", "UMM", "UNIC", "VD 4 ROUES", "WILLYS","AUTOBIANCHI","AUVERLAND","BEDFORD",
           "BREMACH","BUICK","DAF","DAIHATSU","MAHINDRA","DACIA","COURNIL")
allemande <- c("AUDI", "BMW", "MERCEDES BENZ", "MINI", "VW PORSCHE")
asian <- c("ASIA", "DAEWOO", "HONDA", "HYUNDAI", "ISUZU", "KIA", "MAZDA", "MITSUBISHI", 
           "SSANGYONG", "SUBARU", "SUZUKI", "TOYOTA")
sport <- c("ALPINE", "CORVETTE", "FERRARI", "LOTUS", "MASERATI", "PORSCHE")
americaine <- c("CADILLAC", "CHEVROLET", "CHRYSLER", "DODGE", "FORD", "HUMMER", "JEEP")
collection <- c("AUSTIN", "AUSTIN HEALEY", "BERTONE", "DAIMLER", "HOTCHKISS", "MG", "MORGAN", "PININFARINA", 
                "TRIUMPH","PONTIAC")
luxe <- c("BENTLEY", "JAGUAR", "LAND ROVER", "LEXUS", "ROVER")
europe <- c("OPEL", "SAAB", "SEAT", "VOLKSWAGEN", "VOLVO","SKODA","ALFA ROMEO","LANCIA")
RENAULT <-c("RENAULT","NISSAN")

data_policy$vh_make_G <- ifelse(data_policy$vh_make %in% autre, "autre",
                               ifelse(data_policy$vh_make %in% allemande, "allemande",
                                      ifelse(data_policy$vh_make %in% asian, "asian",
                                             ifelse(data_policy$vh_make %in% sport, "sport",
                                                    ifelse(data_policy$vh_make %in% americaine, "americaine",
                                                           ifelse(data_policy$vh_make %in% collection, "collection",
                                                                  ifelse(data_policy$vh_make %in% luxe,"luxe",
                                                                         ifelse(data_policy$vh_make %in% europe,"europeenne",
                                                                                ifelse(data_policy$vh_make %in% RENAULT, "RENAULT",data_policy$vh_make)))))))))


# Création du log des variables -------------------------------------------

data_policy$Ldensite <- log(data_policy$densite)
data_policy$Lvh_value <- log(data_policy$vh_value)
data_policy$Lvh_weight <- log(data_policy$vh_weight)
data_policy$Lvh_cyl <- log(data_policy$vh_cyl)


# Ajount du nombre de claims ----------------------------------------------

nb_claim <- data_claims %>% group_by(id_policy) %>% summarise(claim_nb = sum(claim_nb))

# Et de son log 

data_claims$Lclaim_amount <- log(data_claims$claim_amount)

# Création des bases de travail -------------------------------------------

data_freq <- left_join(data_policy, nb_claim, by = "id_policy") %>% 
  mutate(claim_nb = ifelse(is.na(claim_nb), 0, claim_nb))

data_tot <- merge(data_claims, data_policy, by = c("id_policy","id_client","id_vehicle","id_year"),
                  all = T)

# On supprime les sinistres ayant un montant négatif pour l'étude de la sévérité

data_claims <- merge(data_claims %>% filter(claim_amount > 0), 
                     data_policy, by = c("id_policy","id_client","id_vehicle","id_year"), all.x = T)


# Sauvergarde des bases ---------------------------------------------------

dir.create("./data", showWarnings = FALSE)


saveRDS(data_freq, "data/data_freq_year0.rds")
saveRDS(data_tot, "data/data_tot_year0.rds")
saveRDS(data_claims,"data/data_claims_year0.rds")



#round(prop.table(table(data_freq$drv_age1G3, data_freq$claim_nb, useNA = 'ifany'), 1),3)
#round(prop.table(table(data_freq$vh_makeG, data_freq$claim_nb, useNA = 'ifany'), 1),3)

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


# Préparation données YEAR 1 ----------------------------------------------

# Création d'une variable numéro du département ------------------------------

data_year_1 <- data_year_1 %>% 
  mutate(DEP = as.character(str_extract_all(pol_insee_code,"^\\d."))) 


# Ajout de la variable région ---------------------------------------------

data_year_1 <- data_year_1 %>% left_join(reg_dep, by = "DEP")

# Ajout de la variable densité de la population par département -----------

data_year_1 <- data_year_1 %>% 
  left_join(density_dep, by = "DEP") %>% 
  rename(densite = Densité,
         departement = Département)


# Variable catégorielle de pol_bonus --------------------------------------

data_year_1$bonus <- ifelse(data_year_1$pol_bonus < 1, "bonus", ifelse(data_year_1$pol_bonus == 1, "neutre", "malus"))


# Création de découpages pour les variables continues ---------------------

data_year_1$drv_age1_G1 <- cut(data_year_1$drv_age1, c(17, 2:8*10, 100))
data_year_1$drv_age1_G2 <- cut(data_year_1$drv_age1, c(17, seq(from = 25, to = 80, by = 5),100))
data_year_1$drv_age1_G3 <- cut(data_year_1$drv_age1, c(17, 25, 45,65,80,100))

data_year_1$drv_age_lic1_G <- cut(data_year_1$drv_age_lic1,c(-1,3,5,100))

data_year_1$drv_age2_G1 <- cut(data_year_1$drv_age2, c(17, 2:8*10, 100))
data_year_1$drv_age2_G2 <- cut(data_year_1$drv_age2, c(17, seq(from = 25, to = 80, by = 5),100))
data_year_1$drv_age2_G3 <- cut(data_year_1$drv_age2, c(17, 25, 45,65,80,100))

data_year_1$drv_age_lic2_G <- cut(data_year_1$drv_age_lic2,c(-1,3,5,100))

data_year_1$vh_age_G1 <- cut(data_year_1$vh_age,c(-1, 0:1*5, 100))
data_year_1$vh_age_G2 <- cut(data_year_1$vh_age,c(-1,10,20,100))
data_year_1$vh_age_G3 <- cut(data_year_1$vh_age,c(-1,0,1,2,3,4,5,6,7,8,9,10,100))

data_year_1$vh_value_G1 <- cut(data_year_1$vh_value,c(seq(from = 0, to = 100000, by = 10000),155498))
data_year_1$vh_value_G2 <- cut(data_year_1$vh_value,c(seq(from = 0, to = 100000, by = 50000),155498 +1))
data_year_1$vh_value_G3 <- cut(data_year_1$vh_value,c(seq(from = 0, to = 50000, by = 10000),155498+1))


# Création d'une variables risk_class -------------------------------------

data_year_1$risk_class <- (data_year_1$vh_din*0.736)*100/ (data_year_1$vh_weight + 75)
data_year_1$risk_class_G  <- cut(data_year_1$risk_class, 
                                 breaks=c(quantile(data_year_1$risk_class, probs = seq(0, 1, by = 0.20))),labels = 1:5)

data_year_1$vh_cyl_G <- cut(data_year_1$vh_cyl,c(-1,1500,Inf), labels = 1:2)

# Création d'une variable regroupant les marques de véhicules --------------

autre <- c("ACL", "APAL", "ARO", "DATSUN", "EBRO", "FSO", "GME", "IVECO", "LADA VAZ", "MORRIS",
           "OM", "PANHARD", "PIAGGIO", "PININFARINA", "SANTANA", "SAVIEM", "SIMCA", "SMART", "STEYR PUCH",
           "TALBOT", "TEILHOL", "UMM", "UNIC", "VD 4 ROUES", "WILLYS","AUTOBIANCHI","AUVERLAND","BEDFORD",
           "BREMACH","BUICK","DAF","DAIHATSU","MAHINDRA","DACIA","COURNIL")
allemande <- c("AUDI", "BMW", "MERCEDES BENZ", "MINI", "VW PORSCHE")
asian <- c("ASIA", "DAEWOO", "HONDA", "HYUNDAI", "ISUZU", "KIA", "MAZDA", "MITSUBISHI", 
           "SSANGYONG", "SUBARU", "SUZUKI", "TOYOTA")
sport <- c("ALPINE", "CORVETTE", "FERRARI", "LOTUS", "MASERATI", "PORSCHE")
americaine <- c("CADILLAC", "CHEVROLET", "CHRYSLER", "DODGE", "FORD", "HUMMER", "JEEP")
collection <- c("AUSTIN", "AUSTIN HEALEY", "BERTONE", "DAIMLER", "HOTCHKISS", "MG", "MORGAN", "PININFARINA", 
                "TRIUMPH","PONTIAC")
luxe <- c("BENTLEY", "JAGUAR", "LAND ROVER", "LEXUS", "ROVER")
europe <- c("OPEL", "SAAB", "SEAT", "VOLKSWAGEN", "VOLVO","SKODA","ALFA ROMEO","LANCIA")
RENAULT <-c("RENAULT","NISSAN")

data_year_1$vh_make_G <- ifelse(data_year_1$vh_make %in% autre, "autre",
                                ifelse(data_year_1$vh_make %in% allemande, "allemande",
                                       ifelse(data_year_1$vh_make %in% asian, "asian",
                                              ifelse(data_year_1$vh_make %in% sport, "sport",
                                                     ifelse(data_year_1$vh_make %in% americaine, "americaine",
                                                            ifelse(data_year_1$vh_make %in% collection, "collection",
                                                                   ifelse(data_year_1$vh_make %in% luxe,"luxe",
                                                                          ifelse(data_year_1$vh_make %in% europe,"europeenne",
                                                                                 ifelse(data_year_1$vh_make %in% RENAULT, "RENAULT",data_year_1$vh_make)))))))))


# Création du log des variables -------------------------------------------

data_year_1$Ldensite <- log(data_year_1$densite)
data_year_1$Lvh_value <- log(data_year_1$vh_value)
data_year_1$Lvh_weight <- log(data_year_1$vh_weight)
data_year_1$Lvh_cyl <- log(data_year_1$vh_cyl)


# Sauvergarde des bases ---------------------------------------------------


saveRDS(data_year_1, "data/data_complete_year1.rds")
