##------------------------------------##
##    Applications numériques         ## 
##   Cours d'actuariat non vie        ##
##                                    ##
##------------------------------------##

setwd("~/Documents/Education/ENSAE/3A/S1/Actuariat non vie")

# Libraries --------------------
require("xts")
#install.packages("CASdatasets", repos = "http://dutangc.perso.math.cnrs.fr/RRepository/pub/", type="source")
library(CASdatasets)
library(MASS)
library(ggplot2)
library(tidyverse)
library(xtable)
library(mvabund)
library(pscl)
library(plotly)
library(stats)
library(modi)

source("02_plot_function.R")

# Data --------------------
data(freMTPLfreq)
data(freMTPLsev)

### Travail sur les variables ----
freMTPLfreq$DriverAgeG <- cut(freMTPLfreq$DriverAge, c(17, 2:8*10, 100))
freMTPLfreq$CarAgeG <- cut(freMTPLfreq$CarAge, c(-1, 0:1*5, 100))
freMTPLfreq$LDensity <- log(freMTPLfreq$Density)
freMTPLsev$LClaimAmount <- log(freMTPLsev$ClaimAmount)

freMTPL <- merge(freMTPLfreq, freMTPLsev, by = "PolicyID", all = T) # base avec doublon de policy id
freMTPLsev <- merge(freMTPLsev, freMTPLfreq, by = "PolicyID", al.x = T) # base des claims

# Régression pour la fréquence ----------------
summary(freMTPLfreq)

### Stats desc ----
# 3.3 Car Age
fun_plot(freMTPLfreq, "CarAge", "ClaimNb")

# 3.4 Driver Age
fun_plot(freMTPLfreq, "DriverAge", "ClaimNb")

# 3.5 Power
fun_plot(freMTPLfreq, "Power", "ClaimNb")

# 3.8 – Relation moyenne / variance du nombre de sinistres
mean_variance(freMTPLfreq, 'DriverAge', 'ClaimNb')
mean_variance(freMTPLfreq, 'CarAge', 'ClaimNb')
mean_variance(freMTPLfreq, 'Power', 'ClaimNb')
mean_variance(freMTPLfreq, 'Region', 'ClaimNb')


### Modele poisson ----
fpois2 <- glm(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
              +Gas+Region+Density, offset=log(Exposure),
              family=poisson("log"), data=freMTPLfreq)
summary(fpois2)
#xtable(coef(summary(fpois2)), digits=3) 
plotgroupresiduals(fpois2) #, trim = F, m =1


### Modele Poisson sur dispersée (quasi poisson) ----
fqpois3 <- glm(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
               +Gas+Region+Density, offset=log(Exposure),
               family=quasipoisson("log"), data=freMTPLfreq)
summary(fqpois3)
summary(fqpois3)$dispersion #phi
plotgroupresiduals(fqpois3) #, trim = F, m =1


### Modele binomiale negative ----
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


### Zero inflaté ----
# fzip_logit <- zeroinfl(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
#                +Gas+Region+LDensity+offset(log(Exposure)),
#                data=freMTPLfreq, dist = "poisson", link = "logit")
# summary(fzip_logit)
# 
# fzip_probit <- zeroinfl(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
#                        +Gas+Region+LDensity+offset(log(Exposure)),
#                        data=freMTPLfreq, dist = "poisson", link = "probit")
# summary(fzip_probit)



# Régression pour la severite ----------------
freMTPL_filtered <- freMTPL %>% filter(ClaimAmount<=1125 | ClaimAmount>=1221)

### Stats desc ----
summary(freMTPL)
summary(freMTPL_filtered)

# 4.2 - Fonction de répartition empirique
plot(ecdf(freMTPL_filtered$ClaimAmount), xlim = c(0, 10000))
plot(ecdf(freMTPL_filtered$LClaimAmount), xlim = c(0, 15))

# 4.3 – Boxplot des montants et des log-montants
ggplot(freMTPL_filtered, aes(x="1", y=ClaimAmount)) + 
  geom_boxplot()
ggplot(freMTPL_filtered, aes(x="1", y=LClaimAmount)) + 
  geom_boxplot()

# 4.4 - Boxplot des montants de sinistre sur les caract ́eristiques du v ́ehicule
amountvsnb(freMTPLsev$ClaimAmount)
amountvsnb_grouped(freMTPLsev, "DriverAgeG", "ClaimAmount")
amountvsnb_grouped(freMTPLsev, "Power", "ClaimAmount")


# 4.5 et 4.6 - Boxplot bivariée ClaimAmount x Autre variable
ggplot(freMTPL_filtered, aes(x=as.factor(DriverAge), y=ClaimAmount, fill=as.factor(DriverAge))) + 
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = quantile(freMTPL_filtered$ClaimAmount, c(0.1, 0.9)))

ggplot(freMTPL_filtered, aes(x=as.factor(Region), y=ClaimAmount)) + 
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = quantile(freMTPL_filtered$ClaimAmount, c(0.1, 0.9)))

ggplot(freMTPL_filtered, aes(x=as.factor(CarAge), y=ClaimAmount)) + 
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = quantile(freMTPL_filtered$ClaimAmount, c(0.1, 0.9)))

ggplot(freMTPL_filtered, aes(x=as.factor(Power), y=ClaimAmount)) + 
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = quantile(freMTPL_filtered$ClaimAmount, c(0.1, 0.9)))

fun_boxplot(freMTPL_filtered, "Region", "ClaimAmount")
fun_boxplot(freMTPL_filtered, "CarAge", "ClaimAmount")
fun_boxplot(freMTPL_filtered, "Power", "ClaimAmount")
fun_boxplot(freMTPL_filtered, "DriverAge", "ClaimAmount")


### Modele Gamma lien log ----
fgamma <- glm(ClaimAmount ~ DriverAgeG+LDensity,
              family=Gamma("log"), data=freMTPL_filtered)
summary(fgamma)
plotgroupresiduals(fgamma, m=1, trim = F) #, trim = F, m =1

### Modele Inverse gaussienne lien log ----
fig_log <- glm(ClaimAmount ~ DriverAgeG+LDensity,
              family=inverse.gaussian("log"), data=freMTPL_filtered, maxit = 50)
summary(fig_log)
plotgroupresiduals(fig_log, m=1, trim = F) #, trim = F, m =1

### Modele log gamma ----
flgamma <- glm(LClaimAmount ~ DriverAgeG+LDensity,
               family=Gamma("identity"), data=freMTPL_filtered)
summary(flgamma)
plotgroupresiduals(flgamma, m=1, trim = F) #, trim = F, m =1

