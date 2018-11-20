list.of.packages <- c("tidyverse", "FNN", "tibble", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Cargamos las librerias
library(tidyverse)
library(FNN)
library(tibble)
library(reshape2)

df2 <- read.csv2("Data/df2.csv",encoding = "ISO-8859-1",dec = ",")
IDS = c()
Jugadores = c()
i=1

#Elegimos unos jugadores con su ID
Nombres = c("20801 Cristiano Ronaldo", "158023 L. Messi", "176580 L. Suárez")
#Sacamos un vector con los nombres de los jugadores y otro cons sus ids
for (i in 1:length(Nombres)) {
  AuxIds <- c(strsplit(Nombres, " ")[[i]][1])
  AuxJugadores <-
    paste(c(strsplit(Nombres, " ")[[i]])[-1], collapse = " ")
  IDS = append(IDS, AuxIds)
  Jugadores = append(Jugadores, AuxJugadores)
}

#Recomendador
Variables <- c("rs", "rw", "rf", "ram", "rcm", "rm", "rcb", "rb", "rwb", "st", "lw","cam",
               "lm", "cdm", "cb", "lb", "lwb", "lf", "lcm", "ldm", "lcb", "gk")

i=1
for(i in 1:length(Variables)){
  Variables[i] =  paste("prefers_",Variables[i],sep = "")
}
Variables

#Cambiar Logical a Number
converter <- function(var){
  return(as.numeric(as.logical(var)))
}

#Convertimos en 0 y 1 las variables booleanas
for(i in 1:length(Variables)){
  n= which(colnames(df2)==Variables[i])
  a=paste("df2$",Variables[i],sep="")
  df2[,n] = converter(eval(parse( text=a )) )
}

#Filtramos en TRAIN y TEST los jugadores elegidos
TEST <- df2 %>%
  filter(ID %in% IDS)
TRAIN <- df2 %>% 
  filter(!ID %in% IDS)

#Dejamos solo las variables numericas
TEST <- (TEST[,-c(2,3,4,7,8)])
TRAIN <- (TRAIN[,-c(2,3,4,7,8)])

#Buscamos las posiciones preferidas del primer jugador elegido
j=1
k=0
Prefers=c()
for(j in 1:length(Variables)){
  if(TEST[c(Variables)][j]==1){
    k=k+1
    Prefers[k]=names(TEST[c(Variables)][j])
  }
}
Prefers

#Encontramos el numero de variable a la que corresponde
i=1
POS=c()
for(i in 1:length(Prefers)){
  POS[i]=grep(paste("^",Prefers[i],"$",sep=""), colnames(TRAIN))
}
POS

#Filtramos segun el numero de posiciones preferidas que tengan los jugadores elegidos
for(i in 1) {
  if (length(POS == 1)) {
    TRAIN2 <- TRAIN %>%
      filter(TRAIN[POS[1]] == 1)
  }
  else if (length(POS == 2)) {
    TRAIN2 <- TRAIN %>%
      filter(TRAIN[POS[1]] == 1 | TRAIN[POS[2]] == 1)
  }
  else if(length(POS == 3)) {
    TRAIN2 <- TRAIN %>%
      filter(TRAIN[POS[1]] == 1 | TRAIN[POS[2]] == 1 |
               TRAIN[POS[3]] == 1)
  }
}
#Filtramos por presupuesto
Presupuesto = 75
TRAIN2 <- TRAIN2 %>% 
  filter(ValorMdo<=Presupuesto)

#modelo donde nos dice que jugadores estan más cerca
KNN_Model = knn(train = TRAIN2[,-c(1)],test = TEST[,-c(1)],cl = TRAIN2$prefers_gk,5)
attr(KNN_Model, "nn.index")


#Filtramos buscando el Id por posicion
IDr<- TRAIN2$ID[as.vector(attr(KNN_Model, "nn.index"))]
AUX <- df2 %>% 
  filter(ID==IDr[1]|ID==IDr[2]|ID==IDr[3]|ID==IDr[4]|ID==IDr[5]|
           ID==IDr[6]|ID==IDr[7]|ID==IDr[8]|ID==IDr[9]|ID==IDr[10]|
           ID==IDr[11]|ID==IDr[12]|ID==IDr[13]|ID==IDr[14]|ID==IDr[15]) %>% 
  select(ID,name,age,club,overall,ValorMdo,pac,sho,pas,dri,def,phy,POR) %>% 
  top_n(6)
AUX
#FIN Recomendador

#Grafico
VdM<- AUX %>%
  filter(ID %in% IDr) %>% 
  select(ValorMdo)
AUX <- AUX %>% 
  mutate(name=apply(AUX[ ,c(2,5)] , 1 , paste , collapse = "-" )) %>% 
  select(c(name, pac, sho, pas, dri, def, phy)) %>% 
  as.data.frame() %>% 
  melt(id.vars ="name")

AUX <- AUX %>% 
  mutate(VdM=rep(paste(round(VdM[[1]],2),"M€"),6))

se <- function(x){
  sqrt(var(x) / length(x))
}
ggplot(AUX, aes(variable, value, fill = variable)) +
  geom_bar(width = 1,
           stat = "identity",
           color = "white") +
  scale_y_continuous(breaks = 0:nlevels(AUX$variable)) +
  theme_gray() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.line = element_blank()
  ) +
  coord_polar()+ facet_wrap(~name+VdM)



