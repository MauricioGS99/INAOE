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
      res<-"muere"
    }
    else{
      res<-"nada"
    }
  }
  
  if (x=="cirugia"){
    p<-runif(1)
    if (p<0.5){
      res<-"muere"
    }
    else{
      res<-"nada"
    }
  }
  
  if(x=="nada"){
    p<-runif(1)
    if(p<0.8){
      res<-"muere"
    }
    else{
      res<-"vive"
    }
  }
  return(res)
}

n<-1000

# Asignación de enfermedad #
enfermedad <- vector(mode="character", length=n)
enfermedad<-unlist(lapply(enfermedad, asigna_enfermedad))

# Asignación de Tratamiento por parte del médico
tratamiento<-vector(mode="character",length=n)
tratamiento<-unlist(lapply(tratamiento,asigna_tratamiento))

#Posible reacción al tratamiento
reaccion<-vector(mode="character", length=n)
reaccion<-unlist(lapply(tratamiento,asigna_reaccion))
