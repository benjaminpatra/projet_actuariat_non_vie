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
library(plotly)


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
# Car Age
data_plot1 <- freMTPLfreq %>% 
  group_by(CarAge,ClaimNb) %>% 
  summarise(n = n()) %>% 
  ungroup()

data_plot2 <- freMTPLfreq %>% 
  group_by(CarAge) %>% 
  summarise(n_carAge = n()) %>% 
  ungroup()

data_plot <- merge(data_plot1, data_plot2, by = "CarAge")
data_plot$freq = data_plot$n/data_plot$n_carAge

plot_CarAge <- plot_ly(data_plot) %>%
  add_trace(x =~ CarAge, y =~ n, type = 'bar', color =~ as.factor(ClaimNb), opacity = 0.8) %>% 
  layout(barmode = 'stack',
         yaxis = list(title = "Absolute frquency"),
         title = "Claim Number by Car Age")
plot_CarAge

plot_CarAge_100 <- plot_ly(data_plot) %>%
  add_trace(x =~ CarAge, y =~ freq, type = 'bar', color =~ as.factor(ClaimNb), opacity = 0.8) %>% 
  layout(barmode = 'stack',
         yaxis = list(title = "proportion"),
         title = "Claim Number by Car Age")
plot_CarAge_100

# Driver Age
data_plot1 <- freMTPLfreq %>% 
  group_by(DriverAge,ClaimNb) %>% 
  summarise(n = n()) %>% 
  ungroup()

data_plot2 <- freMTPLfreq %>% 
  group_by(DriverAge) %>% 
  summarise(n_DriverAge = n()) %>% 
  ungroup()

data_plot <- merge(data_plot1, data_plot2, by = "DriverAge")
data_plot$freq = data_plot$n/data_plot$n_DriverAge

plot_DriverAge <- plot_ly(data_plot) %>%
  add_trace(x =~ DriverAge, y =~ n, type = 'bar', color =~ as.factor(ClaimNb)) %>% 
  layout(barmode = 'stack',
         yaxis = list(title = "Absolute frquency"),
         title = "Claim Number by Driver Age")
plot_DriverAge

plot_DriverAge_100 <- plot_ly(data_plot) %>%
  add_trace(x =~ DriverAge, y =~ freq, type = 'bar', color =~ as.factor(ClaimNb), opacity = 0.8) %>% 
  layout(barmode = 'stack',
         yaxis = list(title = "proportion"),
         title = "Claim Number by Driver Age")
plot_DriverAge_100


# Mean Variance Plot
plot(freMTPLfreq %>% group_by(Power) %>%
       summarise(m = weighted.mean(ClaimNb, Exposure),
                 v = weighted.var(ClaimNb, Exposure)))

plot(freMTPLfreq %>% group_by(Region) %>%
       summarise(m = mean(ClaimNb),
                 v = var(ClaimNb)))



# Modele poisson ----
fpois2 <- glm(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
              +Gas+Region+Density, offset=log(Exposure),
              family=poisson("log"), data=freMTPLfreq)
summary(fpois2)
#xtable(coef(summary(fpois2)), digits=3) 


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
               data=freMTPLfreq, dist = "poisson", link = "logit")
summary(fzip_logit)

fzip_probit <- zeroinfl(ClaimNb ~ Power+CarAgeG+DriverAgeG+Brand
                       +Gas+Region+Density+offset(log(Exposure)),
                       data=freMTPLfreq, dist = "poisson", link = "probit")
summary(fzip_probit)
