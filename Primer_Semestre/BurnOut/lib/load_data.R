setwd("~/datos_Adi")
data<-read.csv("diabetes_marked_datos_sim.csv")

data$paciente<-as.factor(data$paciente)
tapply(data$presc.code, data$paciente,rle)
by(data,data$paciente,rle)

test<-c(1, 1, 1, 2, 3, 4, 4, 4, 4, 5, 5, 6, 6, 6, 6)

duraciones<-rle(test)[1]
