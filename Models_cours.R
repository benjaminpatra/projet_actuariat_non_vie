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


### Zero inflaté (PAS BON) ----
fzip_logit <- zeroinfl(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
               +Gas+Region+LDensity+offset(log(Exposure)),
               data=freMTPLfreq, dist = "poisson", link = "logit")
summary(fzip_logit)
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
AIC(fzmP_logit)
BIC(fzmP_logit)
plotgroupresiduals(fzmP_logit, m=50) #, trim = F, m =1


fzmP_probit <- hurdle(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
                     +Gas+Region+LDensity+offset(log(Exposure)), 
                     dist = "poisson", link = "probit",
                     data=freMTPLfreq)
summary(fzmP_probit) # Presque bon
AIC(fzmP_probit)
BIC(fzmP_probit)
plotgroupresiduals(fzmP_probit, m=50) #, trim = F, m =1


fzmP_clolog <- hurdle(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
                      +Gas+Region+LDensity+offset(log(Exposure)), 
                      dist = "poisson", link = "cloglog",
                      data=freMTPLfreq)
summary(fzmP_clolog) # Presque bon
AIC(fzmP_clolog)
BIC(fzmP_clolog)
plotgroupresiduals(fzmP_clolog, m=50) #, trim = F, m =1


fzmP_cauchit <- hurdle(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
                      +Gas+Region+LDensity+offset(log(Exposure)), 
                      dist = "poisson", link = "cauchit",
                      data=freMTPLfreq)
summary(fzmP_cauchit) # Presque bon
AIC(fzmP_cauchit)
BIC(fzmP_cauchit)
plotgroupresiduals(fzmP_cauchit, m=50) #, trim = F, m =1


fzmNB_logit <- hurdle(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
                     +Gas+Region+LDensity+offset(log(Exposure)), 
                     dist = "negbin", link = "logit",
                     data=freMTPLfreq)
summary(fzmNB_logit) # Presque bon
AIC(fzmNB_logit)
BIC(fzmNB_logit)
plotgroupresiduals(fzmNB_logit, m=50) #, trim = F, m =1


#*******************************************************************#
# Régression pour la severite ----------------
#*******************************************************************#
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
AIC(fgamma)
BIC(fgamma)
logLik(fgamma)
plotgroupresiduals(fgamma, m=1, trim = F) #, trim = F, m =1

### Modele Inverse gaussienne lien log ----
fig_log <- glm(ClaimAmount ~ DriverAgeG+LDensity,
              family=inverse.gaussian(link = "log"), data=freMTPL_filtered, maxit = 50)
summary(fig_log)
AIC(fig_log)
BIC(fig_log)
logLik(fig_log)
plotgroupresiduals(fig_log, m=1, trim = F) #, trim = F, m =1

### Modele log gamma ----
flgamma <- glm(LClaimAmount ~ DriverAgeG+LDensity,
               family=Gamma("identity"), data=freMTPLsev)
summary(flgamma)
AIC(flgamma)
BIC(flgamma)
logLik(flgamma)
plotgroupresiduals(flgamma, m=1, trim = F) #, trim = F, m =1

### LogNormale ----
flnormale <- glm(LClaimAmount ~ DriverAgeG+LDensity,
               gaussian(link = "identity"), data=freMTPL_filtered)
summary(flnormale)
AIC(flnormale)
BIC(flnormale)
logLik(flnormale)
plotgroupresiduals(flnormale, m=1, trim = F) #, trim = F, m =1



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
logLik(fgamma_e)

### 2) Log gamma (PAS BON)---- 
flgamma_e <- glm(LClaimAmount_ecrete ~ DriverAgeG+LDensity,
                family=Gamma("identity"), data=freMTPL_filtered)
summary(flgamma_e)
logLik(flgamma_e)

### 3) Inverse Gaussienne ----
fig_log_e <- glm(ClaimAmount_ecrete ~ DriverAgeG+LDensity,
               family=inverse.gaussian(link = "log"), data=freMTPL_filtered)
summary(fig_log_e)
AIC(fig_log_e)
BIC(fig_log_e)
logLik(fig_log_e)

### 4) Log Normale (PAS BON)----
flnormale_e <- glm(LClaimAmount ~ DriverAgeG+LDensity,
                 gaussian(link = "identity"), data=freMTPL_filtered)
summary(flnormale_e)
BIC(flnormale_e)
logLik(flnormale_e)


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
logLik(fgamma_s)

### 2) Log gamma (PAS BON)----
flgamma_s <- glm(LClaimAmount ~ DriverAgeG+LDensity,
                 family=Gamma("identity"), data=freMTPL_inf)
summary(flgamma_s)
logLik(flgamma_s)

### 3) Inverse Gaussienne (PRESQUE BON)----
fig_log_s <- glm(ClaimAmount ~ DriverAgeG+LDensity,
                 family=inverse.gaussian(link = "log"), data=freMTPL_inf, maxit = 50)
summary(fig_log_s)
BIC(fig_log_s)
logLik(fig_log_s)

### 4) Log Normale (PAS BON)----
flnormale_s <- glm(LClaimAmount ~ DriverAgeG+LDensity,
                  gaussian(link = "identity"), data=freMTPL_inf)
summary(flnormale_s)
BIC(flnormale_s)
logLik(flnormale_s)