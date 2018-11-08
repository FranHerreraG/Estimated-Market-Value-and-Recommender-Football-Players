
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)

Raw <- read_delim("Data/Raw.csv", ";", 
                 escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                 trim_ws = TRUE)

#Quitamos Columnas que no son útiles y calculamos la edad
Raw <- Raw[,!names(Raw) %in% c("flag","club_logo","photo")]
dia="2018-8-31" #Ultimo día del mercdo de fichajes 
Raw$age = as.numeric(lapply(Raw$birth_date,function(X){floor(interval(X, dia) / duration(num = 1, units = "years"))}))
rm(dia)

#Formateo de variables
Raw$body_type=as.factor(Raw$body_type)
Raw$work_rate_att=factor(Raw$work_rate_att,levels=c("Low","Medium","High"))
Raw$work_rate_def=factor(Raw$work_rate_def,levels=c("Low","Medium","High"))
Raw$preferred_foot=as.factor(Raw$preferred_foot)
Raw$mercado=as.factor(Raw$mercado)

summary(Raw[,c("body_type","work_rate_att","work_rate_def","preferred_foot","mercado")])


Variables1 <- c("special", "age", "height_cm", "weight_kg", "eur_value", "overall", "potential", "pac",
                "sho", "pas", "dri", "def", "phy", "international_reputation", "skill_moves", "crossing",
                "finishing", "heading_accuracy", "short_passing", "volleys", "dribbling", "curve",
                "free_kick_accuracy", "long_passing", "ball_control", "acceleration", "sprint_speed",
                "agility", "reactions", "balance", "shot_power", "jumping", "stamina", "strength",
                "long_shots", "aggression", "interceptions", "positioning", "vision", "penalties",
                "composure", "marking", "standing_tackle", "sliding_tackle", "gk_diving", "gk_handling",
                "gk_kicking", "gk_positioning", "gk_reflexes", "coste")
Variables <- c("rs", "rw", "rf", "ram", "rcm", "rm", "rdm", "rcb", "rb", "rwb", "st", "lw", "cf","cam",
                "cm", "lm", "cdm", "cb", "lb", "lwb", "ls", "lf", "lam", "lcm", "ldm", "lcb", "gk")


Raw[,Variables]= as.matrix(Raw[,Variables]) %>% replace_na(0)

RawA <- Raw[,c(Variables1,Variables)]

CorA <- cor(RawA, method = c("pearson", "kendall", "spearman"))

write.csv2(CorA,"Data/Machine_Learning/CorA.csv", row.names=FALSE)

#Elegimos las variables relevantes y las que tienen una alta correlación entre si y creamos nuevas variables que se integren.
rm(RawA,CorA)

Raw = Raw %>% 
  rowwise() %>% 
  mutate(POR = mean(gk_diving,gk_handling,gk_kicking,gk_positioning,gk_reflexes,gk_flat_kick_trait,
                             gk_long_throw_trait,gk_up_for_corners_trait),
         DEF = mean(def,aggression,interceptions,marking,standing_tackle,sliding_tackle,
                          heading_accuracy,stamina),
         SPE = mean(acceleration,sprint_speed,agility,balance,pac,stamina,positioning),
         BAL = mean(short_passing,volleys,dribbling,curve,free_kick_accuracy,long_passing,
                          ball_control,shot_power,penalties,long_shots,crossing,finishing,vision,
                          composure,stamina,positioning),
         MEN = mean(sho,pas,dri,reactions),
         FUE = mean(phy,strength),
         IMC = weight_kg/(height_cm/100)^2,
         WORK_ATK = as.numeric(work_rate_att),
         WORK_DEF = as.numeric(work_rate_def),
         coste = coste/1000000)

df = Raw %>% select(ID,special,age,overall,potential,international_reputation,skill_moves,weak_foot,
                    WORK_ATK,WORK_DEF,jumping,POR,DEF,SPE,BAL,MEN,FUE,IMC,coste)

#Separamos en Train y Test
set.seed(1234)
train <- df %>% 
  filter(!is.na(coste)) %>% 
  sample_frac(.784)
length(train[[1]])
test  <- anti_join(df%>%filter(!is.na(coste)), train, by = 'ID')
length(test[[1]])

test = select(test,-one_of(c("ID")))
train = select(train,-one_of(c("ID")))

#Tenemos 800 futbolistas para entrenar y 220 para testear
summary(train$coste)


#Guardamos las tablas por si las necesitasemos recuperar.
write.csv2(train,"Data/Machine_Learning/Train.csv", row.names=FALSE)
write.csv2(test,"Data/Machine_Learning/Test.csv", row.names=FALSE)

#Modelos


modelo1=glm(coste~ ., data=train[,-1],family=gaussian(link = "identity"))
summary(modelo1)

modeloStep=step(modelo1,direction="both",trace=1)
anova(modelo1,modeloStep)

#Nos quedamos con el modelo eficientado por la funcion Step
modeloTop=glm(coste ~ age + overall + international_reputation + WORK_DEF + POR + BAL + MEN,
                  data=train,family=gaussian(link = "identity"))
summary(modeloTop)

#Quitamos WORK_DEF porque no sale significativa
modeloTop=glm(coste ~ age + overall + international_reputation + POR + BAL + MEN,
              data=train,family=gaussian(link = "identity"))
summary(modeloTop)


#Creamos una tabla con todas las predicciones
Predicciones= data.frame(test$coste)
colnames(Predicciones)="coste"

#Comprobamos el MSE(mean square error) de cada modelo
Predicciones$PredictM1=round(predict(modelo1,test),4)
(MSEM1 =mean((Predicciones$coste - Predicciones$PredictM1)^2))

Predicciones$PredictMStep=round(predict(modeloStep,test),4)
(MSEMStep =mean((Predicciones$coste - Predicciones$PredictMStep)^2))

Predicciones$PredictMTop=round(predict(modeloTop,test),4)
(MSEMTop =mean((Predicciones$coste - Predicciones$PredictMTop)^2))

#Si Ploteamos una regresion polinomica podemos encontrar un modelo que ajuste mejor con nuestros datos

VarPlot = c("special","overall","potential","POR","DEF","BAL","MEN","SPE","IMC")
repcoste=rep(train$coste,length(VarPlot))

train[,(colnames(train)%in%VarPlot)] %>% 
  keep(is.numeric) %>% 
  gather() %>% 
  mutate(coste=repcoste) %>% 
  ggplot(aes(value,coste)) +
  facet_wrap(~ key, scales = "free") +
  geom_point(colour = c("firebrick3")) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 3), color = "black")

#Variamos el numero de puntos del polinomio
train[,(colnames(train)%in%VarPlot)] %>% 
  keep(is.numeric) %>% 
  gather() %>% 
  mutate(coste=repcoste) %>% 
  ggplot(aes(value,coste)) +
  facet_wrap(~ key, scales = "free") +
  geom_point(colour = c("firebrick3")) +
  stat_smooth(method = "lm", formula = y ~ poly(x, 4), color = "black")

#vemos que con 4 mejora y que las mejores opciones son para las variables overall MEN ,special y potential

#Modelo poly para MEN
modeloPolyMEN = lm(coste ~ poly(MEN,4),data = train)
summary(modeloPolyMEN)
Predicciones$PredictMPMEN=round(predict(modeloPolyMEN,test),4)
(MSEMEN =mean((Predicciones$coste - Predicciones$PredictMPMEN)^2))

#Modelo poly para overall
modeloPolyOVE = lm(coste ~ poly(overall,4),data = train)
summary(modeloPolyOVE)
Predicciones$PredictMPOVE=round(predict(modeloPolyOVE,test),4)
(MSEoverall =mean((Predicciones$coste - Predicciones$PredictMPOVE)^2))

#Modelo poly para special
modeloPolySPE = lm(coste ~ poly(special,4),data = train)
summary(modeloPolySPE)
Predicciones$PredictMPSPE=round(predict(modeloPolySPE,test),4)
(MSEspecial =mean((Predicciones$coste - Predicciones$PredictMPSPE)^2))

#Modelo poly para potential
modeloPolyPOT = lm(coste ~ poly(potential,4),data = train)
summary(modeloPolyPOT)
Predicciones$PredictMPPOT=round(predict(modeloPolyPOT,test),4)
(MSEpotential = mean((Predicciones$coste - Predicciones$PredictMPPOT)^2))

#Vemos que el MSE (mean square error) más pequeños nos lo da el modelo Poly con la variable potential
data.frame(MSEMEN,MSEspecial,MSEoverall,MSEM1,MSEMStep,MSEMTop,MSEpotential)

