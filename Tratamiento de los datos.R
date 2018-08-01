library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tabplot) #mirar cran tableplot(fifa18[,1:10], cex = 1.8)


fifa18<- read_csv("data/FIFA18v2.csv")

c <-fifa18 %>% 
  group_by(Club) %>% 
  summarise(meanValue = sum(Value), maxValue = max(Value)) %>% 
  arrange(desc(meanValue))

c <- as.data.frame(head(c,15))

k2 <- ggplot(c,aes(x=c$"Club",y=c$"meanValue")) + 
  geom_point()
ss2 <- tableGrob(c)
#Arrange them as you want with grid.arrange
jpg(file=mypath)
mytitle = paste("my title is", names[i])
plot(x,y, main = mytitle)
dev.off()
grid.arrange(k2,ss2)


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
