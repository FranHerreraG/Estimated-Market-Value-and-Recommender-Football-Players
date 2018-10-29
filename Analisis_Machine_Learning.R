
library(readr)
library(dplyr)
library(lubridate)

Raw <- read_delim("Data/Raw.csv", ";", 
                 escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                 trim_ws = TRUE)

#Quitamos Columnas que no son útiles y calculamos la edad
Raw <- Raw[,!names(Raw) %in% c("flag","club_logo","photo")]
dia="2018-8-31" #Ultimo día del mercdo de fichajes 
Raw$age = as.numeric(lapply(Raw$birth_date,function(X){floor(interval(X, dia) / duration(num = 1, units = "years"))}))
#Formateo de variables
Raw$body_type=as.factor(Raw$body_type)
Raw$work_rate_att=as.factor(Raw$work_rate_att)
Raw$work_rate_def=as.factor(Raw$work_rate_def)
Raw$preferred_foot=as.factor(Raw$preferred_foot)
Raw$mercado=as.factor(Raw$mercado)

#Separamos en Train y Test
set.seed(1234)
train <- Raw %>% 
  filter(!is.na(mercado)) %>% 
  sample_frac(.784)
length(train[[1]])
test  <- anti_join(Raw%>%filter(!is.na(mercado)), train, by = 'ID')
length(test[[1]])

#Tenemos 800 futbolistas para entrenar y 220 para testear
summary(train$coste)

#Guardamos las tablas por si las necesitasemos recuperar.
write.csv2(train,"Data/Machine_Learning/Train.csv", row.names=FALSE)
write.csv2(test,"Data/Machine_Learning/Test.csv", row.names=FALSE)

nousar=c()

modeloLogit=glm(coste~ mercado, data=train,family=poisson())
summary(modeloLogit)

test_aux = test


