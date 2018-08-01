library(readr)
library(dplyr)
fifa18<- read_csv("data/FIFA18v2.csv")

fifa18 %>% 
  group_by(Club) %>% 
  summarise(meanValue = sum(Value), maxValue = max(Value)) %>% 
  arrange(desc(meanValue))


fifa18 <- as.data.frame(fifa18[,!(colnames(fifa18) %in% c("Photo", "Flag", "Club Logo"))])
str(fifa18)
head(fifa18)
summary(fifa18)

fifa18$Nationality=as.factor(fifa18$Nationality)
fifa18$Club=as.factor(fifa18$Club)

summary(fifa18)

fifa18[fifa18$Nationality=="France",]

modeloLogit=glm(Potential~Age+Overall+Acceleration, data=fifa18,family=poisson)
summary(modeloLogit)
