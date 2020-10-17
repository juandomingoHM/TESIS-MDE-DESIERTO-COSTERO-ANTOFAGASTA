#TRABAJO DE TITULO: "DISTRIBICION POTENCIAL DE ESPECIES ENDEMICAS CON PROBLEMAS DE CONSERVACION EN EL DESIERTO COSTERO DE ANTOFAGASTA"
#NOMBRE: JUAN DOMINGO HUENUQUEO MARILLAN
#FECHA: 15/10/2020
#TEMA: SELECCION DE VARIABLES CON MENOR CORRELACION

#Librerias utilizadas
library(raster) 
library(HH) #Variance Inflation Factor
library(rgeos) 
library(dismo) #Operaciones y funciones para modelacion de distribucion potencial
library(rgdal) 
library(ggplot2) #para graficar
library(ggdendro) #para graficar dendrogramas
library(dplyr) #maninulacion de dataframe

# 13 ANALISIS DE CORRELACION ENTRE VARIABLES CANDIDATAS

#Listado de variables estandarizadas
lista.variables <- list.files(path="~/ensayos/variables",pattern='*.asc', full.names=TRUE)
lista.variables

#Generacion de un stack para el listado de variables
variables <- stack(lista.variables)
names(variables) #nombre de las variables
variables
plot(variables[[1]]) #visualizar una variable

#Consulta de resolucion de la variable
res.grados<-xres(variables)
res.km<-res.grados*111.19 #calculo de resolucion por cuadricula en kilometros
res.km

#Consulta de proyeccione espacial de una variable
projection(variables[[2]])

#Convertir las variables estandarizadas en matrices (dataframe)
variables.tabla<-as.data.frame(variables)

#Eliminar los valores nulos (NA) de las variables
variables.tabla<-na.omit(variables.tabla)

#Generar correlacion entre matrices de las variables
variables.correlacion<-cor(variables.tabla)
variables.correlacion
write.csv(variables.correlacion, file = "~/presencias/correlación_bio's.csv") #guardar correlacion entre variables

#Aplicar reduccion de distancias para eliminar correlaciones negativas para pasarlas a valores absolutos 
variables.dist<-as.dist(abs(variables.correlacion)) #deja solo valores absolutos
variables.dist

#Generacion de dendrograma de cluster (menor distancia = mayor correlacion)
variables.cluster<-hclust(1-variables.dist)
variables.cluster
plot(variables.cluster, hang = -1) #invertir la correlación (por eso es 1-variable.cluster)

#visualización, interpretacion y seleccion de grupos en dendrograma de correlacion
rect.hclust(variables.cluster, 16) #se eligen un numero de clases o grupos de acuerdo al numero de variable y su interpretacion grafica
abline(h = 0.28, col = 'blue') #dada la interpretacion, se selecciona un punto de corte en los 0,72 (0,28 ya que el grafico esta invertido)
hcd <- as.dendrogram(variables.cluster)

#visaulizacion de dendrograma cluster con un punto de corte 0,72
plot(hcd, type = "rectangle", ylab = "Height")
plot(hcd, xlim = c(1, 27), ylim = c(0,1)) #27 = N° variables
plot(hcd,  ylab = "Correlación invertida", edgePar = list(col = 2:3, lwd = 2:1),
     main= "Correlación Variables")


# 14 SELECCION DE VARIABLES PARA LA MODELACION

#Eliminacion manual de variables que superan la correlacion de 0,72 dada la interpretacion del cluster dendrograma
variables.tabla2<-variables.tabla #tabla que contiene las 27 variables candidatas
variables.tabla2 <- select(variables.tabla2, -a.acum, -tri, -temp.max, -ind.ari, -pp.mean, -temp.min, -dir.pend.ew, -dir.pend.ns,
                           -dir.dren, -hills, -relief, -aspec, -evi.max, -curv, ipt) #eliminacion de variables con alta correlacion > 0,72
variables.tabla2

#Segundo filtro de seleccion utlizando el VIF
resultado.vif<-vif(variables.tabla2) #este filtro sirve para descartar variables de comportarmiento lineal que podrian alterar la modelacion
resultado.vif #se descarta aquellas con alta correlacion dado el calculo (en mi caso no fue necesario)

#Compacto de variables seleccionadas aplicados los filtros
variables.seleccionadas<-names(resultado.vif) #seleccion final de variables 
variables.seleccionadas
variables.seleccionadas<-names(variables.tabla2) #seleccion final 
variables.seleccionadas

#Se crea un brick con las variables seleccionadas
variables<-brick(variables[[variables.seleccionadas]])
variables
plot(variables) #resultado y visualizacion de variables seleccionadas


