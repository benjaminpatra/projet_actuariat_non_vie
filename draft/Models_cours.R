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
library(actuar)

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
freMTPL_filtered <- freMTPL %>% filter(ClaimAmount<=1125 | ClaimAmount>=1221)

#*******************************************************************#
# Régression pour la fréquence ----------------
#*******************************************************************#
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
results_model(fpois2)

### Modele Poisson sur dispersée (quasi poisson) ----
fqpois3 <- glm(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
               +Gas+Region+Density, offset=log(Exposure),
               family=quasipoisson("log"), data=freMTPLfreq)
summary(fqpois3)
summary(fqpois3)$dispersion #phi
results_model(fqpois3)



### Modele binomiale negative ----
# Première méthode : fixer theta hat = phi en faisant une regression linéaire
phi <- 0.9864679
fnb4 <- glm(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
            +Gas+Region+Density, offset=log(Exposure),
            family=negative.binomial(phi), data=freMTPLfreq)
summary(fnb4)
results_model(fnb4)

# Deuxième méthode : estimer theta
fnb5 <- glm.nb(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
               +Gas+Region+Density+offset(log(Exposure)),
               data=freMTPLfreq)
summary(fnb5)
results_model(fnb5)


### Zero inflaté (PAS BON) ----
# fzip_logit <- zeroinfl(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
#                +Gas+Region+LDensity+offset(log(Exposure)),
#                data=freMTPLfreq, dist = "poisson", link = "logit")
# summary(fzip_logit)
# 
# fzip_probit <- zeroinfl(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
#                        +Gas+Region+LDensity+offset(log(Exposure)),
#                        data=freMTPLfreq, dist = "poisson", link = "probit")
# summary(fzip_probit)

### Zero modifie ----
fzmP_logit <- hurdle(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
                     +Gas+Region+LDensity+offset(log(Exposure)), 
                     dist = "poisson", link = "logit",
                     data=freMTPLfreq)
summary(fzmP_logit) # Pas trop mal mais pas tt a fait comme dans le cours
results_model(fzmP_logit, m= 50, dev = F)


fzmP_probit <- hurdle(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
                     +Gas+Region+LDensity+offset(log(Exposure)), 
                     dist = "poisson", link = "probit",
                     data=freMTPLfreq)
summary(fzmP_probit) # Presque bon
results_model(fzmP_probit, m= 50, dev = F)



fzmP_clolog <- hurdle(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
                      +Gas+Region+LDensity+offset(log(Exposure)), 
                      dist = "poisson", link = "cloglog",
                      data=freMTPLfreq)
summary(fzmP_clolog) # Presque bon
results_model(fzmP_clolog, m= 50, dev = F)



fzmP_cauchit <- hurdle(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
                      +Gas+Region+LDensity+offset(log(Exposure)), 
                      dist = "poisson", link = "cauchit",
                      data=freMTPLfreq)
summary(fzmP_cauchit) # Presque bon
results_model(fzmP_cauchit, m= 50, dev = F)



fzmNB_logit <- hurdle(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
                     +Gas+Region+LDensity+offset(log(Exposure)), 
                     dist = "negbin", link = "logit",
                     data=freMTPLfreq)
summary(fzmNB_logit) # Presque bon
results_model(fzmNB_logit, m= 50, dev = F)



#*******************************************************************#
# Régression pour la severite ----------------
#*******************************************************************#

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

# 4.4 - Amount vs Nb
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
results_model(fgamma, m = 1, trim = F)

### Modele Inverse gaussienne lien log ----
fig_log <- glm(ClaimAmount ~ DriverAgeG+LDensity,
              family=inverse.gaussian(link = "log"), data=freMTPL_filtered, maxit = 50)
summary(fig_log)
results_model(fig_log, m = 1, trim = F)

### Modele log gamma ----
flgamma <- glm(LClaimAmount ~ DriverAgeG+LDensity,
               family=Gamma("identity"), data=freMTPLsev)
summary(flgamma)
results_model(flgamma, m = 1, trim = F)

### LogNormale ----
flnormale <- glm(LClaimAmount ~ DriverAgeG+LDensity,
                 family = gaussian("log"),
               data=freMTPL_filtered)
summary(flnormale)
results_model(flnormale, m = 1, trim = F)

flnormale2 <- lm(LClaimAmount ~ DriverAgeG+LDensity,
                 data=freMTPL_filtered)
summary(flnormale2)
results_model(flnormale2, m = 1, trim = F)



#*******************************************************************#
# Prise en compte des valeurs extremes : Ecretement ----------------
#*******************************************************************#

valeur_lim = 100000

charge_surcrete <- sum(freMTPL_filtered %>% mutate(surcrete = pmax(ClaimAmount-valeur_lim,0)) %>% select(surcrete))
freMTPL_filtered <- freMTPL_filtered %>% 
  mutate(ClaimAmount_ecrete = pmin(ClaimAmount, valeur_lim) + charge_surcrete / nrow(freMTPL_filtered),
         LClaimAmount_ecrete = log(ClaimAmount_ecrete))


charge_surcrete2 <- sum(freMTPLsev%>% mutate(surcrete = pmax(ClaimAmount-valeur_lim,0)) %>% select(surcrete))
freMTPLsev <- freMTPLsev %>% 
  mutate(ClaimAmount_ecrete = pmin(ClaimAmount, valeur_lim) + charge_surcrete2 / nrow(freMTPLsev),
         LClaimAmount_ecrete = log(ClaimAmount_ecrete))


### 1) gamma ----
fgamma_e <- glm(ClaimAmount_ecrete ~ DriverAgeG+LDensity,
              family=Gamma("log"), data=freMTPL_filtered)
summary(fgamma_e)
results_model(fgamma_e, m = 1, trim = F)


### 2) Log gamma (PAS BON)---- 
flgamma_e <- glm(LClaimAmount_ecrete ~ DriverAgeG+LDensity,
                family=Gamma("identity"), data=freMTPL_filtered)
summary(flgamma_e)
results_model(flgamma_e, m = 1, trim = F)


### 3) Inverse Gaussienne ----
fig_log_e <- glm(ClaimAmount_ecrete ~ DriverAgeG+LDensity,
               family=inverse.gaussian(link = "log"), data=freMTPL_filtered)
summary(fig_log_e)
results_model(fig_log_e, m = 1, trim = F)


### 4) Log Normale (PAS BON)----
flnormale_e <- glm(LClaimAmount ~ DriverAgeG+LDensity,
                 gaussian(link = "identity"), data=freMTPL_filtered)
summary(flnormale_e)
results_model(flnormale_e, m = 1, trim = F)



#*******************************************************************#
# Prise en compte des valeurs extremes : Séparation ----------------
#*******************************************************************#
freMTPL_inf <- freMTPL_filtered %>% filter(ClaimAmount <= valeur_lim)
freMTPL_sup <- freMTPL_filtered %>% filter(ClaimAmount > valeur_lim)

### Estimation de alpha d'une la loi de pareto1
m = mean(freMTPL_sup$ClaimAmount)
v = var(freMTPL_sup$ClaimAmount)
alpha_<-2/(1-m^2/v)

(24/11402)*(alpha*100000)/(alpha-1)
(24/413169)*(alpha*100000)/(alpha-1) # ici on utilise le nb d'observation en freq ???

freMTPL_inf <- freMTPL_inf %>% mutate(ClaimAmount_inflate = ClaimAmount +792.0578)

### 1) gamma (PRESQUE BON)----
fgamma_s <- glm(ClaimAmount ~ DriverAgeG+LDensity,
                family=Gamma("log"), data=freMTPL_inf)
summary(fgamma_s)
results_model(fgamma_s, m = 1, trim = F)


### 2) Log gamma (PAS BON)----
flgamma_s <- glm(LClaimAmount ~ DriverAgeG+LDensity,
                 family=Gamma("identity"), data=freMTPL_inf)
summary(flgamma_s)
results_model(flgamma_s, m = 1, trim = F)


### 3) Inverse Gaussienne (PRESQUE BON)----
fig_log_s <- glm(ClaimAmount ~ DriverAgeG+LDensity,
                 family=inverse.gaussian(link = "log"), data=freMTPL_inf, maxit = 50)
summary(fig_log_s)
results_model(fig_log_s, m = 1, trim = F)


### 4) Log Normale (PAS BON)----
flnormale_s <- lm(log(ClaimAmount) ~ DriverAgeG+Density,
                  data=freMTPL_inf)
summary(flnormale_s)
results_model(flnormale_s, m = 1, trim = F, dev = F)
