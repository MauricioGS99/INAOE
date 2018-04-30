library(tidyverse)

# El problema dice lo siguiente
# Un paciente puede tener la enfermedad A con proba .4
# y la enfermedad B con .6 
# En cualquier caso, si no se hace nada morirá con proba 0.8 y no le pasa nada con .2
# El medico tiene 3 posibilidades: nada, farmaco, cirugia.
# Puede morir durante cirugía con proba 0.5 y de alergia con proba 0.2
# Si sobrevive a alergia o cirugía:
# Si tenia la enfermedad A, entonces la cirugía lo medicina con 0.9 o con .1 le aplica el caso de no hacer nada
# Si tenia la enfermedad B, la medicina con proba 0.1 lo cura y con 0.9 lo manda a nada
# Si el paciente sobrevive la cirugía, entonces se cura con proba 0.5 si tenia A y sin efecto si no
# Si tenia la enfermedad B, la cirugía lo cura con 0.6 y sin efecto con 0.4

proba_vivir<-function(p){
  ## probabilidad de vivir
  p <- runif(1)
  if(p<0.8){
    res<-"vivir"
  }
  else {
    res<-"morir"
  }
  return(res)
}

asigna_enfermedad<-function(x){
  p <- runif(1)
  if (p < 0.6){
    res<-"A"
  }
  else {
    res<-"B"
  }
  return(res)
}

asigna_tratamiento<-function(x){
  p<-runif(1)
  if (p < 0.3){
    res<-"farmaco"
  }
  else {
    if (p<0.66){
      res<-"cirugia"
    }
    else
      res<-"nada"
  }
  return(res)
}


asigna_reaccion<-function(x){
  if (x == "farmaco"){
    p <- runif(1)
    if (p<0.2){
      res<-"alergia_mortal"
    }
    else{
      res<-"sobrevive_farmaco"
    }
    return(res)
  }
  
  if (x=="cirugia"){
    p<-runif(1)
    if (p<0.5){
      res<-"cirugia_fatal"
    }
    else{
      res<-"sobrevive_cirugia"
    }
    return(res)
  }
  
  if(x=="nada"){
    res<-"sin_tratamiento"
  }
  return(res)
}



prob_vivir<-function(p){
  z <- runif(1)
  if(z<p){
    res<-"vivir"
  }
  else{
    res<-"morir"
  }
  return(res)
}
n<-10

# Asignación de enfermedad #
enfermedad <- vector(mode="character", length=n)
enfermedad<-unlist(lapply(enfermedad, asigna_enfermedad))

# Asignación de Tratamiento por parte del médico
tratamiento<-vector(mode="character",length=n)
tratamiento<-unlist(lapply(tratamiento,asigna_tratamiento))

#Posible reacción al tratamiento
reaccion<-vector(mode="character", length=n)
reaccion<-unlist(lapply(tratamiento,asigna_reaccion))

#Construyo data_frame
df<-data_frame(enfermedad,tratamiento,reaccion)


# Para la enfermedad A,
#                      si sobrevives fármaco, entonces te curas con 0.9 o con 0.1 sin efecto
#                      si sobrevives cirugía, entonces te curas con 0.5 y 0.5 sin efecto
#
# Para la enfermedad B,
#                      si sobrevives fármaco, sin efecto alguno
#                      si sobrevives cirugía, te curas con 0.6 y 0.4 sin efecto

df_1<-df %>% filter(enfermedad=="A") %>%
       filter(tratamiento=="farmaco")%>%
       filter(reaccion=="alergia_mortal")%>%
       mutate(vivir="morir")

df_2<-df %>% filter(enfermedad=="A") %>% 
       filter(tratamiento=="farmaco") %>%
       filter(reaccion=="sobrevive_farmaco") %>%
       mutate(vivir=ifelse(runif(1)<0.9,"vivir",proba_vivir(0.2)))


df_3<-df %>% filter(enfermedad=="A") %>% 
       filter(tratamiento=="cirugia") %>% 
       filter(reaccion=="sobrevive_cirugia") %>%
       mutate(vivir=ifelse(runif(1)<0.5,"vivir",proba_vivir(0.2)))

df_4<-df %>% filter(enfermedad=="A") %>%
       filter(tratamiento=="cirugia") %>%
       filter(reaccion=="cirugia_fatal") %>%
       mutate(vivir="morir")

df_5<-df %>% filter(enfermedad=="A") %>%
       filter(tratamiento=="nada") %>%
       mutate(vivir=proba_vivir(0.2))

df_6<-df %>% filter(enfermedad=="B") %>%
       filter(tratamiento=="farmaco")%>%
       filter(reaccion=="alergia_mortal")%>%
       mutate(vivir="morir")


df_7<-df %>% filter(enfermedad=="B") %>% 
       filter(tratamiento=="farmaco") %>% 
       filter(reaccion=="sobrevive_farmaco") %>%
       mutate(vivir=ifelse(runif(1)<0.001,"vivir",proba_vivir(0.2)))

df_8<-df %>% filter(enfermedad=="B") %>% 
       filter(tratamiento=="cirugia") %>%
       filter(reaccion=="sobrevive_cirugia") %>%
       mutate(vivir=ifelse(runif(1)<0.6,"vivir",proba_vivir(0.2)))

df_9<-df %>% filter(enfermedad=="B") %>%
       filter(tratamiento=="cirugia") %>%
       filter(reaccion=="cirugia_fatal") %>%
       mutate(vivir="morir")

df_10<-df %>% filter(enfermedad=="B") %>%
       filter(tratamiento=="nada") %>%
       mutate(vivir=proba_vivir(0.2))

bind_rows(df_1,df_2,df_3,df_4,df_5,df_6,df_7,df_8,df_9,df_10)


                                                                             