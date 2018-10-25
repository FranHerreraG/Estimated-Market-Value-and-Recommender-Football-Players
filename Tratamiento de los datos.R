library(readr)
library(dplyr)

df <- read_delim("Data/Fichajes18.csv", ";", 
                 escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                 trim_ws = TRUE)

df$ultimoClub=as.factor(df$ultimoClub)
df$nuevoClub=as.factor(df$nuevoClub)
df$mercado=as.factor(df$mercado)

summary(df)

ventas <- df %>% 
  group_by(ultimoClub) %>% 
  mutate(coste = coste/1000000) %>% 
  summarise(media = mean(coste), mediana = median(coste),total = sum(coste),jugadores = n()) %>% 
  arrange(desc(total))

compras <- df %>% 
  group_by(nuevoClub) %>% 
  mutate(coste = coste/1000000) %>% 
  summarise(media = mean(coste), mediana = median(coste),total = sum(coste),jugadores = n()) %>% 
  arrange(desc(total))

#Vemos que Equipos como Monaco y Roma son los que más han ingresado y Liverpool y Juventus los que más han gastado







