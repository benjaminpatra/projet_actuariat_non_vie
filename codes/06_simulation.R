# Import packages ---------------------------------------------------------

library(tidyverse)

# Import data and functions -----------------------------------------------

data_claims <- readRDS("./data/data_claims_year0.rds")
data_freq <- readRDS("./data/data_freq_year0.rds")
data_year_1_prime <- readRDS("./data/data_year_1_prime.rds")

source("codes/nonparam-bootstrap-aggclaim.R")

# Simulation taux de chargement -------------------------------------------

result_charge_simulation <- rclaimagg(1e4, data_freq, data_claims)

# Représentation des résultats 

hist(result_charge_simulation, # histogram
     col="grey80", # column color
     border="black",
     freq = FALSE, # show densities instead of frequencies
     xlab = "temp",
     main = "",
     nclass= 20)
lines(density(result_charge_simulation), # density plot
      lwd = 2, # thickness of line
      col = "firebrick3")
abline(v = mean(result_charge_simulation),
       col = "royalblue",
       lwd = 2)
abline(v = sum(data_year_1_prime$prime_pure),
       col = "green",
       lwd = 2)
abline(v = quantile(result_charge_simulation,0.95),
       col = "hotpink",
       lwd = 2,
       lty = 2)
abline(v = quantile(result_charge_simulation,0.99),
       col = "plum",
       lwd = 2,
       lty = 2)
abline(v = quantile(result_charge_simulation,0.995),
       col = "cyan",
       lwd = 2,
       lty = 2)
legend(x = "topright", # location of legend within plot area
       c("boot. agg. claim", "Mean","Pure premium sum","Quantile at 0.95","Quantile at 0.99","Quantile at 0.995"),
       col = c("firebrick3", "royalblue","green","hotpink","plum","cyan"),
       lwd = c(2, 2, 2),
       lty = c(1,1,1,2,2,2),
       cex = 0.6)

plot(ecdf(result_charge_simulation),main= "ecdf charge des sinistres aggrégés")

# Determination du taux de chargement

kappa <- (quantile(result_charge_simulation, 0.95)- sum(data_year_1_prime$prime_pure))/sum(data_year_1_prime$prime_pure)
print(kappa)

# Détermination de la prime commerciale

data_year_1_prime <- data_year_1_prime %>% 
  mutate(prime_commerciale = (1 + kappa)*prime_pure)

# Sauvegarde la prime commerciale

data_TARIFCOM <- data_year_1_prime %>% 
  select(id_client,id_vehicle,id_policy,prime_commerciale) %>% 
  rename(premium = prime_commerciale)

write.csv(data_TARIFCOM,"./TARIFCOM.csv")

# Data pour graphique et tableau
vec <- seq(0.20,1,0.01)

data_kappa<- as.data.frame(vec)

data_kappa <- data_kappa %>% 
  mutate(valeur = quantile(result_charge_simulation, vec),
         ecart = 100*(quantile(result_charge_simulation, vec)- sum(data_year_1_prime$prime_pure))/sum(data_year_1_prime$prime_pure))

plot(x = data_kappa$vec, y =data_kappa$ecart,type = "l",
     main = "taux de chargement",
     xlab = "taux de charg. kappa (%)",
     ylab = "niveau de confiance")


vec_tableau <- c(0.95,0.99,0.995)

data_kappa_tab<- as.data.frame(vec_tableau)

data_kappa_tab <- data_kappa_tab %>% 
  mutate(valeur = quantile(result_charge_simulation, vec_tableau),
         ecart = 100*(quantile(result_charge_simulation, vec_tableau)- sum(data_year_1_prime$prime_pure))/sum(data_year_1_prime$prime_pure))

ecdf(result_charge_simulation)(mean(result_charge_simulation))
ecdf(result_charge_simulation)(sum(data_year_1_prime$prime_pure))
  
  
