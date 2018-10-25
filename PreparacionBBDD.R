#Cargamos las librerias
library(readr)
library(dplyr)


#Cargamos los Excel de los datos de Fichajes y de FIFA

fut2018 <- read_delim("Data/fut2018.csv", 
                      ";", escape_double = FALSE, col_types = cols(Coste = col_number()), 
                      locale = locale(encoding = "ISO-8859-1"), 
                      trim_ws = TRUE)
FIFA18v1 <- read_csv("Data/FIFA18v1.csv", 
                     col_types = cols(birth_date = col_date(format = "%Y-%m-%d")))

#Añadimos un ID a la tabla de Fichajes
fut2018$IDF <- seq.int(nrow(fut2018))

#Buscamos fichajes duplicados, ya que puede que un jugador se mueva varias veces en el mercado.
duplicados <- table(fut2018$name)[table(fut2018$name)>1]
names(duplicados)

AuxDuplicados = fut2018[c("IDF","name","Último club", "Nuevo club")] %>%
  filter(name %in% names(duplicados))

head(AuxDuplicados %>% 
  arrange(name))

#Nos quedamos solo con el ultimo movimiento para cada jugador. Estos son los IDF que desapareceran
VectorBorrar <- c(784,774,855,1244,1362,1153,633,701,587,914,347,1174,707,1374,1211,978,1360,
                  731,510,1149,243,69,427,85,312,111,612,142,1129)

fut <- fut2018 %>%
          filter(!IDF %in% VectorBorrar)

#SanityCheck
length(fut[[1]])==length(fut2018[[1]])-length(VectorBorrar)

fut2018 <- fut2018 %>%
  filter(!IDF %in% VectorBorrar)

rm(fut,VectorBorrar,duplicados,AuxDuplicados)
###################


#Guardamos los nombres de los jugadores de las dos tablas
FIFANAMES = FIFA18v1[c("ID","name","full_name","club")]
FUTNAMES = fut2018[c("name","IDF")]
(N_Fichajes = length(FUTNAMES[[1]]))
(N_Jugadores = length(FIFANAMES[[1]]))

df <- data.frame(matrix(ncol = 4, nrow = N_Fichajes))
colnames(df) = c("IDF","ID","Name","Grupo")
i=1
j=1
for(i in 1:N_Fichajes){
  for(j in 1:N_Jugadores){
    if (FUTNAMES[[1]][i] == FIFANAMES[[2]][j]){
      df[i,1] = FUTNAMES[[2]][i]
      df[i,2] = FIFANAMES[[1]][j]
      df[i,3] = FUTNAMES[[1]][i]
      df[i,4] = 1
    }
    else if (FUTNAMES[[1]][i] == FIFANAMES[[3]][j]){
      df[i,1] = FUTNAMES[[2]][i]
      df[i,2] = FIFANAMES[[1]][j]
      df[i,3] = FUTNAMES[[1]][i]
      df[i,4] = 2
    }
    if(j==length(FIFANAMES[[2]])& is.na(df[i,2])){
        df[i,1] = FUTNAMES[[2]][i]
        df[i,2] = 0
        df[i,3] = FUTNAMES[[1]][i]
        df[i,4] = 3 
    }
  }
  print(FUTNAMES[[1]][i])
}

df$Grupo=as.factor(df$Grupo)
summary(df$Grupo)

#Tenemos 491 fichajes perdidos, que puede que esten por otro nombre o no esten en la base de datos FIFA

perdidos = df[c("IDF","Name")]%>%
            filter(df$Grupo==3)
head(perdidos)

df2=data.frame("IDF"=perdidos$IDF,"ID"=0, "Name"=perdidos$Name,"Real"=0)
(N_Perdidos = length(perdidos[[1]]))

i=1
j=1
k=1
for (i in 1:N_Perdidos){
  jugador = strsplit(perdidos[[2]]," ")[[i]]
  for (j in 1:length(jugador)){
    h=4
    for(k in 1:N_Jugadores){
      if(grepl(jugador[j],FIFANAMES[[2]][k])&h<20){
        df2[i,h] = paste(FIFANAMES[[1]][k],FIFANAMES[[3]][k],"/",FIFANAMES[[4]][k])
        h=h+1
      }
    }
    print(perdidos[[2]][i])
  }
}

df2[1:4,1:5]
#Pasamos los perdidos a un Excel donde le otorgaremos los ID que veamos que corresponde
write.csv2(df2,"Data/Perdidos.csv")

Encontrados <- read_delim("Data/Encontrados.csv", 
                         ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                         trim_ws = TRUE)
sum(Encontrados$ID>0)
## 85 Encontrados en la primera ronda

#Sanity Check
table(Encontrados$ID)[table(Encontrados$ID)>1]
## 0 Duplicados en los Encontrados

#Añadimos los perdidos que hemos encontrado al df

df_aux =    df%>%
            left_join(Encontrados, by = c("IDF"="IDF")) %>% 
            mutate(ID.x = replace(ID.x, ID.x==0, ID.y[!is.na(ID.y)]))

#sanity Check
count(df) - count(df_aux)
head(df_aux);tail(df_aux)

df = df_aux[c("IDF","ID.x","Name.x","Grupo")]
colnames(df)=c("IDF","ID","Name","Grupo")

rm(df_aux,Encontrados,df2,perdidos)

##Volvemos a buscar perdidos de los que aún no hemos encontrado pareja
perdidos_2 = df[c("IDF","Name")]%>%
  filter(df$ID==0)
head(perdidos_2)

df2=data.frame("IDF"=perdidos_2$IDF,"ID"=0, "Name"=perdidos_2$Name,"Real"=0)
(N_Perdidos = length(perdidos_2[[1]]))
#Ahora tenemos 406 IDFs Perdidos

i=1
j=1
k=1
for (i in 1:N_Perdidos){
  jugador = strsplit(perdidos_2[[2]]," ")[[i]]
  for (j in 1:length(jugador)){
    h=4
    for(k in 1:N_Jugadores){
      if(grepl(jugador[j],FIFANAMES[[3]][k])&h<20){
        df2[i,h] = paste(FIFANAMES[[1]][k],FIFANAMES[[3]][k],"/",FIFANAMES[[4]][k])
        h=h+1
      }
    }
    print(perdidos_2[[2]][i])
  }
}

df2[1:4,1:5]

#Vemos que hay algunos jugadores como Kepa que aún podemos encontrar su ID
#Pasamos a un Excel para hacer el mismo procedimiento anterior
write.csv2(df2,"Data/Perdidos.csv")

Encontrados_2 <- read_delim("Data/Encontrados_2.csv", 
                            ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                            trim_ws = TRUE)
sum(Encontrados_2$ID>0)

#Añadimos 65 Perdidos más
#Sanity Check
table(Encontrados_2$ID)[table(Encontrados_2$ID)>1]


#Añadimos los encontrados al DF

df_aux =    df%>%
  left_join(Encontrados_2, by = c("IDF"="IDF")) %>% 
  mutate(ID.x = replace(ID.x, ID.x==0, ID.y[!is.na(ID.y)]))

#Sanity Check
count(df) - count(df_aux)
head(df_aux);tail(df_aux)

df = df_aux[c("IDF","ID.x","Name.x","Grupo")]
colnames(df)=c("IDF","ID","Name","Grupo")

rm(df_aux,Encontrados_2,df2,perdidos_2)

#Tenemos disponibles 1023 fichajes de Verano/Invierno
sum(df$ID>0)

#Juntamos df con fut2018 para tener el ID en los Fichajes que despues juntaremos con los datos FIFA


df_aux = fut2018 %>%
  left_join(df[c("ID","IDF")], by = c("IDF"="IDF")) %>%
  filter(ID>0)

#Sanity Check
sum(table(df_aux$ID)>1)

table(df_aux$ID)[table(df_aux$ID)>1]

duplicados <- as.numeric(names(table(df_aux$ID)[table(df_aux$ID)>1]))
df_aux %>%
  filter(ID %in% duplicados)

#Tenemos 6 jugadores a los que se les ha asignado un ID que no corresponder por compartir Nombre

Fifa_aux = FIFA18v1[,1:4] %>%
  filter(ID %in% duplicados)

IDF_malos <- c(14,300,390,561,1166,1245,1280,1341)

#Buscamos manualmente los IDs correspondientes a los mal catalogados IDFs
#El ID de Jorginho (14) está mal y aprovechamos para cambiarlo aqui.
Encontrados <- data.frame(IDF=c(300,390,1166,1280,14),ID=c(178311,182882,189795,205654,205498))
IDF_Borrar <- c(561,1245,1341) #IDFs no identificados

#Corregimos los IDs con los buenos.

df_aux2 = df_aux %>%
  left_join(Encontrados, by = c("IDF"="IDF")) %>%
  filter(!IDF %in% IDF_Borrar) %>%
  mutate(ID.x = replace(ID.x, !is.na(ID.y), ID.y[!is.na(ID.y)]))

#Sanity Check
count(df_aux) - count(df_aux2) #Los tres IDFs que hemos borrado.

df_aux2 %>%
  filter(IDF %in% IDF_malos) %>%
  top_n(7)

sum(table(df_aux2$ID.x)>1)
table(df_aux2$ID.x)[table(df_aux2$ID.x)>1]
#Ya no hay IDs asignados a más de un jugador

## 
df = df_aux2[c("name", "Último club", "Nuevo club", "Amount", "Mercado", "IDF", "ID.x")]
colnames(df)=c("nombre", "ultimoClub", "nuevoClub", "coste", "mercado", "IDF", "ID")

sum(df$ID>0)
#Tenemos 1020 fichajes

rm(df_aux,df_aux2,Encontrados,Fifa_aux,duplicados,FIFANAMES,FUTNAMES)
rm(h,i,IDF_Borrar,IDF_malos,j,jugador,k,N_Fichajes,N_Jugadores,N_Perdidos)

write.csv2(df,"Data/Fichajes18.csv", row.names=FALSE)

#Juntamos toda la info en una sola tabla RAW

Raw <- FIFA18v1 %>% 
  left_join(df[c("ultimoClub", "nuevoClub", "coste", "mercado", "ID")], by = c("ID"="ID"))


summary(Raw[c("club","age","overall","potential","coste","eur_value","eur_release_clause")])
write.csv2(Raw,"Data/Raw.csv", row.names=FALSE)

#Creamos una tabla con los campos potenciamente importantes para visualizar en TABLEAU

Raw_tableau = Raw %>% 
filter(!is.na(mercado))

Raw_tableau <- Raw[c("ID","name","club","league","birth_date","height_cm","weight_kg","nationality","eur_value","eur_wage",
         "eur_release_clause","overall","potential","pac","sho","pas","dri","def","phy","international_reputation",
         "weak_foot","work_rate_att","work_rate_def","preferred_foot","crossing","finishing","heading_accuracy",
         "short_passing","volleys","dribbling","curve","free_kick_accuracy","long_passing","ball_control","acceleration",
         "sprint_speed","agility","reactions","balance","shot_power","jumping","stamina","strength","long_shots",
         "aggression","interceptions","positioning","vision","penalties","composure","marking","standing_tackle",
         "sliding_tackle","gk_diving","gk_handling","gk_kicking","gk_positioning","gk_reflexes","rs","rw","rf","ram",
         "rcm","rm","rdm","rcb","rb","rwb","st","lw","cf","cam","cm","lm","cdm","cb","lb","lwb","ls","lf","lam","lcm",
         "ldm","lcb","gk","ultimoClub","nuevoClub","coste","mercado")]

write.csv2(Raw_tableau,"Data/Tableau/RawT.csv", row.names=FALSE)

#Creamos dos tablas Test y Train para los procesos ML

set.seed(1234)
train <- Raw %>% 
  filter(!is.na(mercado)) %>% 
  sample_frac(.70)
sum(train$coste)
test  <- anti_join(Raw%>%filter(!is.na(mercado)), train, by = 'ID')
sum(test$coste)

