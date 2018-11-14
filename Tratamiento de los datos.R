library(tidyverse)
library(FNN)

df2 <- read.csv2("Data/df2.csv",encoding = "ISO-8859-1",dec = ",")

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

for(i in 1:length(Variables)){
  n= which(colnames(df2)==Variables[i])
  a=paste("df2$",Variables[i],sep="")
  df2[,n] = converter(eval(parse( text=a )) )
}


#Elegir jugadores por posicion
TEST <- (df2[c(5,13),-c(2,3,4,7,8)])
TRAIN <- (df2[-c(5,13),-c(2,3,4,7,8)])

#Buscamos las posiciones preferidas de los jugadores elegidos
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

i=1
POS=c()
for(i in 1:length(Prefers)){
POS[i]=grep(paste("^",Prefers[i],"$",sep=""), colnames(TRAIN))
}
POS

#Filtramos segun el numero de posiciones preferidas que tengan los jugadores elegidos
if(length(POS==1)){
TRAIN2<- TRAIN %>% 
  filter(TRAIN[POS[1]]==1)
}
if(length(POS==2)){
  TRAIN2<- TRAIN %>% 
    filter(TRAIN[POS[1]]==1 | TRAIN[POS[2]]==1)
}
if(length(POS==3)){
  TRAIN2<- TRAIN %>% 
    filter(TRAIN[POS[1]]==1 | TRAIN[POS[2]]==1 | TRAIN[POS[3]]==1)
}

#modelo donde nos dice que jugadores estan más cerca
KNN_Model = knn(train = TRAIN2[,-c(1)],test = TEST[,-c(1)],cl = TRAIN2[,43],5)
attr(KNN_Model, "nn.index")


#Filtramos buscando el Id por posicion
IDS<- TRAIN2$ID[as.vector(attr(KNN_Model, "nn.index"))]
df2 %>% 
  filter(ID==IDS[1]|ID==IDS[2]|ID==IDS[3]|ID==IDS[4]|ID==IDS[5]) %>% 
  select(ID,name,age,club,overall,ValorMdo,pac,sho,pas,dri,def,phy,POR)

