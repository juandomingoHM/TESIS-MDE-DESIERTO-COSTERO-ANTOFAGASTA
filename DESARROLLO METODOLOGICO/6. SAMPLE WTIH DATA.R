#TRABAJO DE TITULO: "DISTRIBICION POTENCIAL DE ESPECIES ENDEMICAS CON PROBLEMAS DE CONSERVACION EN EL DESIERTO COSTERO DE ANTOFAGASTA"
#NOMBRE: JUAN DOMINGO HUENUQUEO MARILLAN
#FECHA: 16/10/2020
#TEMA:  TECNICA DE MODELACION SAMPLE WITH DATA

#PREPARACION MODELACION: SAMPLE WITH DATA (SWD)

#Librerias
library(ggplot2) #para graficar mapas
library(maps)   #mapas base
library(rasterVis) #manipulacion y visualizacion raster
library(raster)

# 17 BASE DE DATOS DE PRESENCIAS Y VARIABLES CON GENERACION DE BACKGROUND

occs <- read.csv("~/especies/adesmia_melanocaulos/adesmia_melanocaulos_2.csv") #Datos de presencia
layers <- stack(list.files("C:/Users/juand/Desktop/variables2","asc",full.names=TRUE)) #Datos de las variables seleccionadas
names(layers) #nombres de las variables

#Matrices de presencias y variables
pres<-extract(layers, occs[,2:3]) #variables asociadas a las presencias por medio de función "extract"
pres.coord <- (occs[,2:3]) #coordenadas de las presencias "x" e "y"
pres<- cbind(pres.coord, pres) #agregar coordenadas
pres<-na.omit(pres) #eliminar datos nulos (NA)
pres<-unique(pres) #eliminar datos duplicados en caso de que existan

#Matriz de background aleatorio
bkg.coord <- randomPoints(mask=layers, n=nrow(pres)*417) # background con 10000 puntos aleatorios (*417 es un número multiplicado x la cantidad de presencias)
bkg.coord <- data.frame(bkg.coord) #crear matriz para el background
bkg <- data.frame(extract(layers, bkg.coord)) #presencias y variables asociadas a los puntos aleotorios de background
bkg <- cbind(bkg.coord, bkg) #agregar coordenadas a los puntos aleatorios de background de acuerdo a los datos de presencias y variables

#Union de matrices de presencias y variables con background (datos binarios 1 y 0)
pres$presencia <- 1 #presencias y variables = 1. Agregar nueva columna "presencia" (presencias)
bkg$presencia <- 0 #background = 0. Agregar a la columna "presencia" (ausencias)
pres.bkg<-rbind(pres,bkg) #union de matrices por medio de coordenadas de presencias (presencias y variables) con ausencias (background)

# 18 REPRESENTACIÓN ESPACIAL DE PRESENCIAS Y BACKGROUND

#seleccion entre 0 y 1 (presencia y variables, background) a partir de la tabla anterior
pres1 <-pres.bkg[pres.bkg$presencia == "1",] #selección de presencias y variables
bkg1 <- pres.bkg[pres.bkg$presencia == "0",] #selección de background

#seleccion solo coordenadas entre presencias y variables, background
pres1 <- (pres1[,1:2]) #coordenadas presencias y variables
bkg1 <- (bkg1[,1:2]) #coordenadas background

#Mapa de presencias y variables, background
ggplot() +
  geom_point(data = pres1, aes(x = x, y = y), color = "red",
             alpha = 0.4, size = 1) +
  geom_point(data = bkg1, aes(x = x, y = y), color = "blue",
             alpha = 0.2, size = 0.2) +
  labs(title = "Presencias y variables con Background", x = "longitud", y = "latitud") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.2),
        legend.title = element_text(colour="blue", size=10, 
                                    face="bold")) +
  coord_fixed() +
  scale_x_continuous(limits = c(-72, -67)) +
  scale_y_continuous(limits = c(-30, -20))