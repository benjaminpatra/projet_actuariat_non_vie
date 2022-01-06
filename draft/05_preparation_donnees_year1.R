# Import packages ---------------------------------------------------------

library(tidyverse)
library(xlsx)

# Import data -------------------------------------------------------------

data_year_1  <- read_csv("data/ENSAE-15MAPA/15MAPA-PG_2017_YEAR1.csv")

density_dep <- read.xlsx("data/FPORSOC21-F41.xlsx", sheetIndex = 3, endRow = 104, colIndex = c(1,2,3), header = TRUE, startRow = 3) 

population_dep <- read.xlsx("data/ensemble.xlsx", sheetIndex = 2, startRow = 8, header = TRUE, colIndex = c(3,4,8,9))

reg_dep <- read_csv("data/reg_dep.csv")

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