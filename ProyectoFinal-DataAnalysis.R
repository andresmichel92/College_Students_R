#consolidar 5 variables

setwd("D:\\Downloads")


#Variables:
#  I  - Perfil Alumnos   #
# Descripcion: Informacion de cada alumno. 
# Genero: 2 es hombre, 1 es mujer
# Examen de admision Verbal (1-100): admision.letras
# Examen de admision Matematicas (1-100): admision.numeros
# Evaluacion socioeconomica: 1 es el mas alto, 4 es el mas bajo
# Counducta, 20 es el mejor. 

load("perfilAlumnos.R")
head(perfil.alumnos,5)  #2 en genero significa masculino
summary(perfil.alumnos)
genero<-perfil.alumnos[1]
socioeconomica<-perfil.alumnos[6]
perfil.alumnos.sin.genero<-perfil.alumnos[,-1]
perfil.alumnos.sin.socioeconomica<-perfil.alumnos.sin.genero[,-5]

perfil.alumnos.2<-cbind(perfil.alumnos.sin.socioeconomica,genero,socioeconomica)

head(perfil.alumnos.2,3)

#  II - Resultados Examenes  #
# Una matriz por alumno, 2 examenes por materia. 12 materias seran consideradas por alumno, pues
# el analisis es por 1 año. En este caso obtendremos el promedio.
# entre mas alto mejor

load("ResultadosExamenes.R")
head(resultados.examenes.totales)

resultados.examenes.promedio<-matrix(nrow=1000, ncol=1)
for (i in 1:length(resultados.examenes.totales)){
  resultados.examenes.promedio[i] <- mean(resultados.examenes.totales[[i]][,1:12])
}
length(resultados.examenes.promedio)


# III - Resultados Trabajos  #
# entre mas alto mejor

load("ResultadoTrabajos.R")
head(resultados.trabajos.totales ,2)

resultados.trabajos.promedio<-matrix(nrow=1000, ncol=1)
for (i in 1:length(resultados.trabajos.totales)){
  resultados.trabajos.promedio[i] <- mean(resultados.trabajos.totales[[i]][,1:12])
}
length(resultados.trabajos.promedio)

#  VI - Uso Biblioteca    #
# total de consultas, entre mas alto mejor

load("UsoBiblioteca.R")
head(uso.biblioteca.totales,1)

uso.biblioteca.promedio<-matrix(nrow=1000, ncol=1)
for (i in 1:length(uso.biblioteca.totales)){
  uso.biblioteca.promedio[i] <- mean(uso.biblioteca.totales[[i]][,1:12])
}
length(uso.biblioteca.promedio)


#   V - Uso Plataforma    #
load("UsoPlataforma.R")
head(uso.plataforma.totales,1)


#   obtener matriz con el promedio de uso de los primeras 12 materias, es decir, primer año
uso.plataforma.promedio<-matrix(nrow=1000, ncol=1)
for (i in 1:length(uso.plataforma.totales)){
  uso.plataforma.promedio[i] <- mean(uso.plataforma.totales[[i]][,1:12])
}
length(uso.plataforma.promedio)




#=================================
#   apartado de libros
load("ApartadoDeLibros.R")
apartado.libros.promedio<-matrix(nrow=1000, ncol=1)
for (i in 1:length(separacion.libros.totales)){
  apartado.libros.promedio[i] <- mean(separacion.libros.totales[[i]][,1:12])
}

#   Asistencias Totales 
load("AsistenciasTotales.R")
asistencias.totales.promedio<-matrix(nrow=1000, ncol=1)
for (i in 1:length(asistencias.totales)){
  asistencias.totales.promedio[i] <- mean(asistencias.totales[[i]][,1:12])
}

#   Becas
load("Becas.R")
distribucion.becas


#   CambioCarrera
load("CambioCarrera.R")
cambio.carrera


#   Historial Pagos
load("HistorialPagos.R")
registro.pagos
registro.pagos.promedio<-matrix(nrow=1000, ncol=1)
for (i in 1:length(registro.pagos)){
  registro.pagos.promedio[i] <- mean(registro.pagos[[i]][,1:2])
}
registro.pagos.promedio



# ===============  I  ================== #
# ====================================== #
# Generar data frame de datos integrados

datos.integrados <- cbind(apartado.libros.promedio,asistencias.totales.promedio,registro.pagos.promedio, uso.plataforma.promedio, 
                          uso.biblioteca.promedio,resultados.trabajos.promedio,resultados.examenes.promedio,
                          perfil.alumnos.2,distribucion.becas,cambio.carrera)

head(datos.integrados,5)

#colnames(datos.integrados)[1]<-"apartado.libros.promedio"


summary(datos.integrados)

save(datos.integrados, file="datos.integrados.R")
#load("datos.integrados.R")


# ================= II =================== #
# ====================================== #
# Exploracion de los Datos #

View(datos.integrados)
summary(datos.integrados)
str(datos.integrados)
head(datos.integrados,10)

# =============== III =================== #
# ====================================== #
# Particionar datos 900/100 #

ind<-sample(x=c(1,2), size=nrow(datos.integrados), replace=TRUE, prob=c(0.9, 0.1))

training.set<-datos.integrados[ind==1,]
test.set<-datos.integrados[ind==2,]

head(training.set,1)
head(test.set,1)

#scale data
training.scaled <- scale(training.set[1:12])

# ====================================== #
# Metodo del Codo #
wss <- vector()
wss
for (i in 1:15) {
  set.seed(1234)
  wss[i] <- sum(kmeans(training.set, 
                       centers=i)$withinss)
}
wss
# PLOT 1
plot(1:15, wss, type="b", xlab="Numero de clusters",
     ylab="Error Standard")

# ====================================== #
# K Means #
fitk <- kmeans(training.scaled, 4)
fitk

fitk.unscaled<- kmeans(training.set[1:12],3)
fitk.unscaled

plot(training.set[1:12], col=fitk.unscaled$cluster)

# ===============ETIQUETAR KMEANS================== #
fitk.enriched<-training.set
fitk.enriched$cluster <- fitk.unscaled$cluster

#head(fitk.enriched,5)

fitk.enriched$classification<-fitk.unscaled$cluster
fitk.enriched[fitk.enriched$cluster==2,]$classification<-1
fitk.enriched[fitk.enriched$cluster==1 | fitk.enriched$cluster==3,]$classification<-0

head(fitk.enriched,5)

# ====================================== #
# Red Neuronal #
# ===============OBTENER CORRELACION PARA GENERAR PLOT================== #
#install.packages("corrplot")
library(corrplot)

# Analisis de correlacion
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(fitk.enriched)

col<- colorRampPalette(c("red", "white", "blue"))(20)

# Particionar Data set para red Neuronal
ind<-sample(x=c(1,2), size=nrow(fitk.enriched), replace=TRUE, prob=c(0.7, 0.3))
ind


new.training.set<-fitk.enriched[ind==1,]
new.test.set<-fitk.enriched[ind==2,]
new.test.set[18]
new.training.set[18]
head(new.training.set)
head(new.test.set)

#install.packages("neuralnet")
library(neuralnet)

# Create sub training, only with valuable variables (as per corrplot)
sub.training.set<-cbind(new.training.set[8:12],new.training.set[18])


# Test sets:
x.test.set<-new.test.set[8:12]
y.test.set<-new.test.set[18]


set.seed(1234)
neural.net.1<-neuralnet(formula=classification~., 
                        data=sub.training.set, 
                        hidden=c(6,4), 
                        threshold=0.12, 
                        stepmax=1e+05, 
                        lifesign="full", linear.output=FALSE)

str(x.test.set)

ypred=neuralnet::compute(neural.net.1, x.test.set)

ypred.numeric <- data.frame(apply(ypred$net.result, 2, function(x) as.numeric(as.character(x))))

yhat <- round(ypred.numeric)


y.test.set.2 <- data.frame(apply(y.test.set, 2, function(x) as.numeric(as.character(x))))

nn.result <- compute(x=neural.net.1, covariate=x.test.set)

compare.output <-
  cbind(x.test.set, y.test.set.2, 
        yhat)

head(compare.output,5)
colnames(compare.output)[7] <-"yhat"
compare.output
compare.output$error <- 
  compare.output$classification - compare.output$yhat
compare.output
accuracy=(nrow(compare.output)+sum(compare.output$error))/nrow(compare.output)
accuracy


# # ==========Etiquetado de 100 alumnos, para aplicar algoritmo gen ====

#str(test.set[1:12])
Alumnos.calcular.riesgo<-compute(x=neural.net.1, covariate=test.set[1:12])


Alumnos.etiquetados <- round(data.frame(apply(Alumnos.calcular.riesgo$net.result,2, function(x) as.numeric(x))))
head(Alumnos.etiquetados,5)

alumnos.etiquetados.dataset<-cbind(test.set,Alumnos.etiquetados)
colnames(alumnos.etiquetados.dataset)[17]<-"en.riesgo"
head(alumnos.etiquetados.dataset,5)
View(alumnos.etiquetados.dataset$en.riesgo)

save(alumnos.etiquetados.dataset, file="alumnos.etiquetados.dataset.R")

load("alumnos.etiquetados.dataset2.R")

alumnos.en.riesgo<-alumnos.etiquetados.dataset[alumnos.etiquetados.dataset$en.riesgo==1,]
alumnos.en.riesgo

alumnos.etiquetados.dataset[alumnos.etiquetados.dataset$en.riesgo==1,]

Nalumnos.en.riesgo <- NROW(alumnos.en.riesgo$en.riesgo)

Nalumnos.en.riesgo
save(Nalumnos.en.riesgo, file="Nalumnos.en.riesgo.R")
# ====================================== #
# Algoritmo Genetico #
# ====================================== #

library(genalg)


#definir variables y dataset del algoritmo genetico

presupuesto.limite<-10000

items <- data.frame(
  
  item = rep(c("beca","vale.transporte","mentor","consulta.psicologo","evento.integracion",
               "asesor.individual","cursos.remediales","visita.empresa","examen.extratemporaneo",
               "platica.motivacional","viaje.recreativo","grupo.estudio.asesorado","curso.verano.intensivo"), Nalumnos.en.riesgo),
  
  puntos.ayuda = rep(c(60,15,50,40,30,
                       60,60,10,35,25,10,45,60), Nalumnos.en.riesgo),
  
  costo = rep(c(500,100,200,400,40,250,
                2500,50,100,50,10,50,2000), Nalumnos.en.riesgo)
  
)

# funcion fitness
  # se castigo dar vales de transporte al nivel socioeconomico algo
  # se castigo dar becas a personas que ya tenian beca
  # se impulso dar asesoria psicologica a personas con baja conducta

fitness.function <- function(x) {
  
  items.costo <- x %*% items$costo
  items.p.a <- x %*% items$puntos.ayuda
  if (items.costo > presupuesto.limite) 
  {
    return(1000)
  }
  else
  {
    partial.result<-items.p.a
    init=1
    
    for(i in 1:Nalumnos.en.riesgo)
    {
      if (i==1){
        apoyos <- sum(x[i:(i*13)])
      }else{
        init<-(((i-1)*13)+1)
        apoyos <- sum(x[init:(i*13)])
      }
      
      if (((alumnos.en.riesgo[i,]$distribucion.becas== x[init]) ||
           (alumnos.en.riesgo[i,]$registro.pagos.promedio > 1.85))
          && (x[init]==1) ){
        partial.result <- partial.result - 150
      }
      if (x[2]==1 && alumnos.en.riesgo[i,]$evalucion.socioeconomica <= 2){
        partia.result <- partial.result -500
      }
      if (x[4]==1 && alumnos.en.riesgo[i,]$nota.conducta<15.00){
        partial.result <- partial.result + 200
      }
      else{
        partial.result <- partial.result -200
        
      }
    }
}
    
    return(-partial.result)
    
  }
  

# inicializar algoritmo genetico

ga.tree <- rbga.bin(size=13*Nalumnos.en.riesgo, popSize=200,
                    mutationChance=.01,
                    elitism=4, iters=500, 
                    evalFunc=fitness.function,
                    verbose=T)

#obtener recomendacion optima, minimizando el costo

best <- ga.tree$population[ga.tree$evaluations == min(ga.tree$best),][1,]
save(best,file="best.R")


# Separar recomendacion por alumno, en una lista

recomendacion.alumnos<-split(best, ceiling(seq_along(best)/13))

recomendacion.alumnos
# convertir lista de recomendaciones por alumno a Data Frame y dar nombre a columnas

recomendacion.alumnos.df= as.data.frame(t(as.data.frame(recomendacion.alumnos)))

colnames(recomendacion.alumnos.df)<-c("beca","vale.transporte","mentor","consulta.psicologo","evento.integracion",
                             "asesor.individual","cursos.remediales","visita.empresa","examen.extratemporaneo",
                             "platica.motivacional","viaje.recreativo","grupo.estudio","curso.verano")
head(recomendacion.alumnos.df,5)
save(recomendacion.alumnos.df, file="recomendacion.alumnos.R")
