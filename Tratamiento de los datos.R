library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tabplot) #mirar cran tableplot(fifa18[,1:10], cex = 1.8)

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





