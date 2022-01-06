library(tidyverse)


data_policy <- read_csv("data/ENSAE-15MAPA/15MAPA-PG_2017_YEAR0.csv")
data_claims <- read_csv("data/ENSAE-15MAPA/15MAPA-PG_2017_CLAIMS_YEAR0.csv")

names(data)

summary(data)


data_freq <- data_policy %>% left_join(data_claims %>% group_by(id_policy) %>% summarise(nb_claim = sum(claim_nb)) %>% ungroup(), by = "id_policy") %>% 
  mutate(nb_claim = ifelse(is.na(nb_claim),0,nb_claim))


# Car Age
data_plot1 <- data_freq %>% 
  group_by(vh_age,nb_claim) %>% 
  summarise(n = n()) %>% 
  ungroup()

data_plot2 <- data_freq %>% 
  group_by(vh_age) %>% 
  summarise(n_carAge = n()) %>% 
  ungroup()

data_plot <- merge(data_plot1, data_plot2, by = "vh_age")
data_plot$freq = data_plot$n/data_plot$n_carAge

plot_CarAge <- plot_ly(data_plot) %>%
  add_trace(x =~ vh_age, y =~ n, type = 'bar', color =~ as.factor(nb_claim), opacity = 0.8) %>% 
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


