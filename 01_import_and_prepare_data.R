# Import packages ---------------------------------------------------------

library(tidyverse)
library(xlsx)

# Import data -------------------------------------------------------------

data_policy <- read_csv("data/ENSAE-15MAPA/15MAPA-PG_2017_YEAR0.csv")
data_claims <- read_csv("data/ENSAE-15MAPA/15MAPA-PG_2017_CLAIMS_YEAR0.csv")

density_dep <- read.xlsx("data/FPORSOC21-F41.xlsx", sheetIndex = 3, endRow = 104, colIndex = c(1,2,3), header = TRUE, startRow = 3) 

population_dep <- read.xlsx("data/ensemble.xlsx", sheetIndex = 2, startRow = 8, header = TRUE, colIndex = c(3,4,8,9))


# Création d'une variable numéro du département ------------------------------

data_policy <- data_policy %>% 
  mutate(DEP = as.character(str_extract_all(pol_insee_code,"^\\d.")))


# Ajout de la variable densité de la population par département -----------

data_policy <- data_policy %>% left_join(density_dep, by = "DEP")


# Variable catégorielle de pol_bonus --------------------------------------

data_policy$bonus <- ifelse(data_policy$pol_bonus < 1, "bonus", ifelse(data_policy$pol_bonus == 1, "neutre", "malus"))


# Création de découpages pour les variables continues ---------------------

data_policy$drv_age1G1 <- cut(data_policy$drv_age1, c(17, 2:8*10, 100))
data_policy$drv_age1G2 <- cut(data_policy$drv_age1, c(17, seq(from = 25, to = 80, by = 5),100))
data_policy$drv_age1G3 <- cut(data_policy$drv_age1, c(17, 25, 45,65,80,100))

data_policy$drv_age_lic1G <- cut(data_policy$drv_age_lic1,c(-1,3,5,100))

data_policy$drv_age2G1 <- cut(data_policy$drv_age2, c(17, 2:8*10, 100))
data_policy$drv_age2G2 <- cut(data_policy$drv_age2, c(17, seq(from = 25, to = 80, by = 5),100))
data_policy$drv_age2G3 <- cut(data_policy$drv_age2, c(17, 25, 45,65,80,100))

data_policy$drv_age_lic2G <- cut(data_policy$drv_age_lic2,c(-1,3,5,100))

data_policy$vh_age_G <- cut(data_policy$vh_age,c(-1, 0:1*5, 100))
data_policy$vh_age_G2 <- cut(data_policy$vh_age,c(-1,10,20,100))
data_policy$vh_age_G3 <- cut(data_policy$vh_age,c(-1,0,1,2,3,4,5,6,7,8,9,10,100))

data_policy$vh_value_G <- cut(data_policy$vh_value,c(seq(from = 0, to = 100000, by = 10000),155498))
data_policy$vh_value_G2 <- cut(data_policy$vh_value,c(seq(from = 0, to = 100000, by = 50000),155498 +1))
data_policy$vh_value_G3 <- cut(data_policy$vh_value,c(-1,20000,70000,155498+1))


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

data_policy$vh_makeG <- ifelse(data_policy$vh_make %in% autre, "autre",
                               ifelse(data_policy$vh_make %in% allemande, "allemande",
                                      ifelse(data_policy$vh_make %in% asian, "asian",
                                             ifelse(data_policy$vh_make %in% sport, "sport",
                                                    ifelse(data_policy$vh_make %in% americaine, "americaine",
                                                           ifelse(data_policy$vh_make %in% collection, "collection",
                                                                  ifelse(data_policy$vh_make %in% luxe,"luxe",
                                                                         ifelse(data_policy$vh_make %in% europe,"europeenne",
                                                                                ifelse(data_policy$vh_make %in% RENAULT, "RENAULT",data_policy$vh_make)))))))))


# Création du log des variables -------------------------------------------

data_policy$LDensité <- log(data_policy$Densité)
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

saveRDS(data_freq, "data_freq_year0.rds")
saveRDS(data_tot, "data_tot_year0.rds")
saveRDS(data_claims,"data_claims_year0.rds")



#round(prop.table(table(data_freq$drv_age1G3, data_freq$claim_nb, useNA = 'ifany'), 1),3)
#round(prop.table(table(data_freq$vh_makeG, data_freq$claim_nb, useNA = 'ifany'), 1),3)



