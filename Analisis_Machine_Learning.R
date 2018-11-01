
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

#Elegimos las variables relevantes y las que tienen una alta correlación entre si las unificamos.
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
         WORK_DEF = as.numeric(work_rate_def))

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

modeloLogit=glm(coste~ ., data=train,family=poisson())
summary(modeloLogit)


