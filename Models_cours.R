##------------------------------------##
##    Applications numériques         ## 
##   Cours d'actuariat non vie        ##
##                                    ##
##------------------------------------##

### Libraries ----
require("xts")
#install.packages("CASdatasets", repos = "http://dutangc.perso.math.cnrs.fr/RRepository/pub/", type="source")
library(CASdatasets)
library(MASS)
library(ggplot2)
library(tidyverse)
library(xtable)
library(mvabund)
library(pscl)


### Data ----
data(freMTPLfreq)
data(freMTPLsev)

### Régression pour la fréquence ----
summary(freMTPLfreq)


# Catégorisation ----
freMTPLfreq$DriverAgeG <- cut(freMTPLfreq$DriverAge, c(17, 2:8*10, 100))
freMTPLfreq$CarAgeG <- cut(freMTPLfreq$CarAge, c(-1, 0:1*5, 100))
freMTPLfreq$LDensity <- log(freMTPLfreq$Density)


# Stats desc ----
print(ggplot(freMTPLfreq, aes(x = CarAge))+
  geom_histogram(binwidth=1))

hist(freMTPLfreq$CarAge, breaks = 100)

ggplot(freMTPLfreq %>% 
         group_by(CarAgeG,ClaimNb) %>% 
         summarise(n = n()) %>% 
         ungroup() %>% 
         mutate(claim_tot = n*ClaimNb)) +
  geom_bar(aes(x = CarAgeG, y = claim_tot, fill = ClaimNb), stat = "identity")


# Modele poisson ----
fpois2 <- glm(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
              +Gas+Region+Density, offset=log(Exposure),
              family=poisson("log"), data=freMTPLfreq)
summary(fpois2)
#xtable(coef(summary(fpois2)), digits=3) 
plot(fpois2)


# Modele Poisson sur dispersée (quasi poisson) ----
fqpois3 <- glm(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
               +Gas+Region+Density, offset=log(Exposure),
               family=quasipoisson("log"), data=freMTPLfreq)
summary(fqpois3)
summary(fqpois3)$dispersion #phi


# Modele binomiale negative ----
# Première méthode : fixer theta hat = phi en faisant une regression linéaire
phi <- 0.9864679
fnb4 <- glm(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
            +Gas+Region+Density, offset=log(Exposure),
            family=negative.binomial(phi), data=freMTPLfreq)
summary(fnb4)

# Deuxième méthode : estimer theta
fnb5 <- glm.nb(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
               +Gas+Region+Density+offset(log(Exposure)),
               data=freMTPLfreq)
summary(fnb5)


# Zero inflaté ----
fzip_logit <- zeroinfl(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
               +Gas+Region+Density+offset(log(Exposure)),
               data=freMTPLfreq, link = "logit")
summary(fzip_logit)

fzip_probit <- zeroinfl(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
                       +Gas+Region+Density+offset(log(Exposure)),
                       data=freMTPLfreq, link = "logit")
summary(fzip_probit)
