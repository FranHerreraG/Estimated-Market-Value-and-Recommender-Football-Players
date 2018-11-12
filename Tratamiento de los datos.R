library(tidyverse)

df2 <- read_delim("Data/df2.csv", ";", escape_double = FALSE, 
                       locale = locale(encoding = "ISO-8859-1"), 
                       trim_ws = TRUE)

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

KNN_Model = knn(df[])

#EJEMPLO A REPPLICAR
library(FNN)
KNN_Model = knn(ma[-3,-5],ma[3,-5],ma[-3,4],k=5)
attr(KNN_Model, "nn.index")
ma

ma = cbind(ma,seq(c(1,2,3,4,5,6)))
ma[,4] = rep(c(T,F,T),2)
ma







