library(TSP)
library(RCurl)
library(RJSONIO)
library(leaflet)
library(sp)
library(geosphere)
library(shiny)
library(dplyr)
library(shinyTime)
library(ggmap)
library(knitr)
library(DT)
library(ggmap)

setwd("E:/Martin/99.Personal/KSCHOOL/20. TFM/MyStopOver")
key <- "AIzaSyDketUNiJG3LMyMygdlofsXmOsDrdR0_5k"

########################### ALGORITMO PARA EL CÁLUCLO DE LA RUTA  ############################
##############################################################################################

##### Cargo los Datos y Funciones que hemos ido creando
load("Data_MyStopOver.RData")
source("GeoCode.R")

##### Meto unos datos de entrada de Ejemplo

### Fijamos el punto de inicio y fin de la ruta:
# Ponemos siempre la palabra "Madrid" detrás para descartar ambigüedades de localizaciones
ptoOrigen <- geoCode("Puerta del Sol")
ptoOrigen[4]
ptoOrigen <- geoCode(paste("Puerta del Sol", "Madrid", sep = ", "))
ptoFinal <- geoCode(paste("Plaza de Colon", "Madrid", sep = ", "))

### Fijamos las horas de inicio y fin de la ruta:
horaInicio <- 8.0
horaFinal <- 17.0
horaComida <- 14.5
tiempoComer <- 0

# Para sacar el tiempo disponible tenemos en cuenta si la hora de fin es posterior a las 00:00 y le sumamos 24h
if (horaFinal < horaInicio) horaFinal <- horaFinal + 24

#Si en el tiempo que queremos hacer la ruta, coincide con la hora de la comida, metemos una parada para comer, de 1h de duración
if (horaInicio < horaComida & horaFinal > horaComida + 1) tiempoComer <- 1

### Fijamos el tipo de transporte
tipoTrans <- "caminando"

### Elegimos la tipología de lugares a visitar y el rango de puntuación de los mismos
tipoLugar <- c("Museos")
puntuacionMinima <- c(4.0,5)

### Elegimos qué tipo de comida queremos, puntuación y rango de precios:
puntuacionRestaurante <- c(4.5,5)
precioRestaurante <- 2
estiloComida <- "Espanola"

### Preparo mis matrices de tiempos
coord <- COORD
tiempoCaminando <- TIEMPOCAMINANDO
tiempoCoche <- TIEMPOCOCHE
restFinal <- RESTAURANTES
M30 <- M30

# El tiempo disponible para realizar la ruta
tiempoDisponible <- horaFinal - horaInicio - tiempoComer

# Reducimos las posibilidades con las selecciones del usuario
lugaresFiltrados <- which(coord$rating >= puntuacionMinima[1] & coord$rating <= puntuacionMinima[2] & coord$Agrupacion %in% tipoLugar)
coord <- coord[lugaresFiltrados, ]
tiempoCaminando <- tiempoCaminando[lugaresFiltrados,lugaresFiltrados]
tiempoCoche <- tiempoCoche[lugaresFiltrados,lugaresFiltrados]
rm(lugaresFiltrados)

### Cálculo de distancias del origen y final a la BBDD
# Para ello utilizo creo una la función *'tiempo_origen_final tiempoOF'* que transforma 
# calcula las ditancias entre las coordenadas del punto inicial y final respecto a las coordenadas de todos los puntos de interés. 
# A continuación transfomamos estas distancias en tiempos, a partir de los modelos que hemos creado.

tiempo_origen_final <- function(coord, punto, origen = "origen"){
  if (origen == "origen") {
    coord$origenCoche <- coord$origen <- coord$dentroM30 <- NA 
    for (i in 1:nrow(coord)){
      distAux <- distHaversine(coord[i, c("lon", "lat")], punto[2:1])/1000
      # Caminando
      coord$origen[i] <- distAux*coefModCaminando/60  # Minutos
      # Coche
      coord$dentroM30[i] <- point.in.polygon(coord$lat[i], coord$lon[i], M30$lon, M30$lat) + point.in.polygon(punto[1], punto[2], M30$lon, M30$lat)
      coord$origenCoche[i] <- distAux
      coord$origenCoche[i] <- coefModCoche[1]*coord$origenCoche[i] + coefModCoche[2]*coord$origenCoche[i]^2
      if (coord$dentroM30[i] == 0) coord$origenCoche[i] <- coord$origenCoche[i] + coefModCoche[3]
      if (coord$dentroM30[i] == 1) coord$origenCoche[i] <- coord$origenCoche[i] + coefModCoche[4]
      if (coord$dentroM30[i] == 2) coord$origenCoche[i] <- coord$origenCoche[i] + coefModCoche[5]
      coord$origenCoche[i] <- coord$origenCoche[i]/60  # Minutos
    }
    coord$origen <- round(coord$origen,2)
    coord$origenCoche <- round(coord$origenCoche,2)
  } 
  
  if (origen == "final") {
    coord$finalCoche <- coord$final <- coord$dentroM30 <-NA
    for (i in 1:nrow(coord)){
      distAux <- distHaversine(coord[i, c("lon", "lat")], punto[2:1])/1000
      # Caminando
      coord$final[i] <- distAux*coefModCaminando/60  # Minutos
      # Coche
      coord$dentroM30[i] <- point.in.polygon(coord$lat[i], coord$lon[i], M30$lon, M30$lat) + point.in.polygon(punto[1], punto[2], M30$lon, M30$lat)
      coord$finalCoche[i] <- distAux
      coord$finalCoche[i] <- coefModCoche[1]*coord$finalCoche[i] + coefModCoche[2]*coord$finalCoche[i]^2
      # Coef del modelo en función de si ambos puntos están dentro de la M30, uno dentro y otro fuera o los dos fuera
      if (coord$dentroM30[i] == 0) coord$finalCoche[i] <- coord$finalCoche[i] + coefModCoche[3]
      if (coord$dentroM30[i] == 1) coord$finalCoche[i] <- coord$finalCoche[i] + coefModCoche[4]
      if (coord$dentroM30[i] == 2) coord$finalCoche[i] <- coord$finalCoche[i] + coefModCoche[5]
      coord$finalCoche[i] <- coord$finalCoche[i]/60  # Minutos
      
    }
    coord$final <- round(coord$final,2)
    coord$finalCoche <- round(coord$finalCoche,2)
  }
  return(coord)
}


# Calculamos los tiempos desde todos los posibles puntos de la ruta al punto de inicio y de final de la ruta
coord <- tiempo_origen_final(coord, as.numeric(ptoOrigen[1:2]))
coord <- tiempo_origen_final(coord, as.numeric(ptoFinal[1:2]), "final")

#### ¿En qué me baso para elegir los puntos de la ruta?
### Criterios de selección de Lugares
# sumaTiempos: calcula para cada punto el tiempo que tarda en ir hasta allí desde el origen y llegar hasta el final
coord$sumaTiempos <- coord$origen + coord$final

# multiplicador: Ponderamos las distacias para darle mayor importancia a los puntos que están más cerca y menos a los más lejanos. De esta manera intentará siempre ir al más cercano. EL multiplicador dará valor 0 al punto más cercano del inicio y final, y valor 1 al punto más alejado. Le añadimos +0.5 al final para que el multiplicador no pueda quedarse a 0.
coord$multiplicador <- (coord$sumaTiempos - min(coord$sumaTiempos)) / (max(coord$sumaTiempos) - min(coord$sumaTiempos)) + 0.5

# ImpTiempo: Constante de importancia: en función del rating del punto, del número de opiniones (raíz cuadrada para igualar) y de las distancias.
#hist(coord$opiniones)
#hist(coord$opiniones^0.5)
coord$ImpTiempo <- coord$rating*coord$opiniones^0.5 / coord$multiplicador

head(coord)

# Ahora reordenamos las matrices de tiempos teniendo en cuenta la importancia de los puntos, según los criterios anteriores.
tiempoCaminando <- tiempoCaminando[order(coord$ImpTiempo, decreasing = T),order(coord$ImpTiempo, decreasing = T)]
dim(tiempoCoche)

tiempoCoche <- tiempoCoche[order(coord$ImpTiempo, decreasing = T),order(coord$ImpTiempo, decreasing = T)]
dim(tiempoCaminando)

# También reordamos nuestros puntos de interés y reseteamos el índice
coord <- coord[order(coord$ImpTiempo, decreasing = T), ]
head(coord, 10)
row.names(coord) <- NULL

#############################
### Calculamos la ruta óptima
#############################

# Primero, creamos la función
ampliarMatrizYRutaTSPfin <- function(coord, tiempos, tipoTransporte = "caminando"){
  if (tipoTransporte == "caminando"){
    origenAux <- coord$origen
    finalAux <- coord$final
  }
  if (tipoTransporte == "coche"){
    origenAux <- coord$origenCoche
    finalAux <- coord$finalCoche
  }
  if (length(tiempos) == 1) {
    matrizAmpliada <- matrix(c(0,origenAux,origenAux,0),2)
  } else {
    matrizAmpliada <- cbind(origenAux, tiempos)
    matrizAmpliada <- rbind(c(0,origenAux), matrizAmpliada)
  }
  matrizAmpliada <- cbind(matrizAmpliada, c(0,finalAux))
  matrizAmpliada <- rbind(matrizAmpliada, c(0,finalAux,0))
  
  colnames(matrizAmpliada) <- rownames(matrizAmpliada) <- NULL
  
  tour <- TSP(matrizAmpliada)
  ruta <- solve_TSP(tour)
  return(ruta)
}


# Segundo, la función 
calcularRutaFin <- function(coord, matriz_tiempos, tiempo_restante, tipoTransporte = "caminando"){
  if (nrow(matriz_tiempos) != nrow(coord)) {
    cat("Error, la matriz de tiempos debe tener el mismo tamaño que la de lugares") 
    return()
  }
  tiempo <- 0
  i <- 1
  while (tiempo < tiempo_restante*60){
    ruta <- ampliarMatrizYRutaTSPfin(coord[1:i, ], matriz_tiempos[1:i,1:i], tipoTransporte)
    tiempo <- tour_length(ruta) + sum(coord$tiempoNecesario[1:i]*60)
    i <- i + 1
  }
  i <- i - 2
  ruta <- ampliarMatrizYRutaTSPfin(coord[1:i, ], matriz_tiempos[1:i,1:i], tipoTransporte)
  return(ruta)
}


### Ahora ya podemos calcular la ruta
if (tipoTrans == "caminando") aux <- tiempoCaminando
if (tipoTrans == "coche") aux <- tiempoCoche

rutaTSP <- calcularRutaFin(coord, aux, tiempoDisponible, tipoTrans)
rutaTSP
View(rutaTSP)
rm(aux)

### Ponemos un orden a la ruta
ordenRuta <- as.numeric(rutaTSP)-1
if (ordenRuta[1] != 0) ordenRuta <- ordenRuta[c(which(ordenRuta == 0):length(ordenRuta),1:(which(ordenRuta == 0)-1))]

### Nos quedamos con los puntos a visitar en la ruta
destinosFinales <- coord[ordenRuta[-c(1,which(length(ordenRuta)-1 == ordenRuta))], ] # Quitamos el punto inicial y final
destinosFinales$id <- 1:nrow(destinosFinales)

# Ordenamos al revés
if (which(length(ordenRuta)-1 == ordenRuta) != 2) destinosFinales <- destinosFinales[order(destinosFinales$id,
                                                                                           decreasing =T),]
### Creamos nuestra ruta con las coordenadas
puntosFin <- rbind(as.numeric(ptoFinal[1:2]), destinosFinales[1:2], as.numeric(ptoOrigen[1:2])) # Metemos el punto final y inicial

# Mapa de la ruta
leaflet() %>% 
  addTiles()  %>% 
  setView(lng= -3.693575, lat= 40.416826 , zoom=14) %>%
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012", group="NASA") %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite", group = "Blanco y Negro") %>%
  addCircleMarkers(puntosFin$lon, puntosFin$lat, 
                   fillOpacity = 0.7, color="red", radius=10, stroke=FALSE,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addPolylines(data = puntosFin, lng = puntosFin$lon, lat = puntosFin$lat, color = "red") %>%
  addLayersControl(
    baseGroups = c("NASA", "Blanco y Negro"),
    options = layersControlOptions(collapsed = F)
  )


# Añadimos el resto de atributos a la ruta
puntosFin$nombre <- c(ptoFinal[4], destinosFinales$nombre, ptoOrigen[4])
puntosFin$tipo <- c("Fin", destinosFinales$tipo, "Inicio")
puntosFin$rating <- c("--", destinosFinales$rating, "--")
puntosFin$opiniones <- c("--", destinosFinales$opiniones, "--")
puntosFin$tiempoNecesario <- c(0, destinosFinales$tiempoNecesario, 0)

View(puntosFin)

### Agregamos los tiempos a destino
# Añadimos en cada punto el tiempo que se tarda en llegar a este desde el punto anterior

puntosFin$tiempoCaminando <- puntosFin$tiempoCoche <- NA

aux <- which(puntosFin$lat[2] == coord$lat & puntosFin$lon[2] == coord$lon)
puntosFin$tiempoCaminando[1] <- coord$final[aux]
puntosFin$tiempoCoche[1] <- coord$finalCoche[aux]

if (nrow(puntosFin) > 3) {
  for (i in 2:(nrow(puntosFin)-2)){
    aux1 <- which(puntosFin$lat[i] == coord$lat & puntosFin$lon[i] == coord$lon)
    aux2 <- which(puntosFin$lat[i+1] == coord$lat & puntosFin$lon[i+1] == coord$lon)
    puntosFin$tiempoCaminando[i] <- tiempoCaminando[aux1, aux2]
    puntosFin$tiempoCoche[i] <- tiempoCoche[aux1, aux2]
  }
  rm(i)
}

aux <- which(puntosFin$lat[nrow(puntosFin)-1] == coord$lat & puntosFin$lon[nrow(puntosFin)-1] == coord$lon)
puntosFin$tiempoCaminando[nrow(puntosFin)-1] <- coord$origen[aux]
puntosFin$tiempoCoche[nrow(puntosFin)-1] <- coord$origenCoche[aux]

rm(aux, aux1, aux2)

puntosFin$tiempoCaminando <- round(puntosFin$tiempoCaminando)
puntosFin$tiempoCoche <- round(puntosFin$tiempoCoche)

puntosFin$id <- 1:nrow(puntosFin)
puntosFin <- puntosFin[order(puntosFin$id, decreasing = T), ]

View(puntosFin)

### Vamos acumulando los tiempos a lo largo de la ruta
puntosFin$tiempoAcuCaminando <- 0
puntosFin$tiempoAcuCoche <- 0

for (i in 2:nrow(puntosFin)){
  puntosFin$tiempoAcuCaminando[i] <- puntosFin$tiempoNecesario[i]*60 + puntosFin$tiempoCaminando[i] + puntosFin$tiempoAcuCaminando[i-1]
  puntosFin$tiempoAcuCoche[i] <- puntosFin$tiempoNecesario[i]*60 + puntosFin$tiempoCoche[i] + puntosFin$tiempoAcuCoche[i-1]
}
rm(i)

View(puntosFin)

cat("Ruta preliminar OK \n")

#### Agregamos lógica de restaurantes
### Añadimos el restaurante

# Marcamos los puntos entre los que cuadraría la hora de la comida
puntosFin$ComidaCami <- puntosFin$ComidaCoche <- F
puntosFin$ComidaCami[which(puntosFin$tiempoAcuCaminando > (horaComida - horaInicio)*60)] <- T
puntosFin$ComidaCoche[which(puntosFin$tiempoAcuCoche > (horaComida - horaInicio)*60)] <- T

View(puntosFin)

# Filtramos los restaurantes según el estilo elegido
if (estiloComida != "Todos"){
  auxEstilo <- which(names(restFinal) == estiloComida)
  restFinal <- restFinal[restFinal[auxEstilo] == 1, ]
  rm(auxEstilo)
}
nrow(restFinal)

# Filtramos los restaurantes según el precio y rating
restFinal <- restFinal %>% filter(precio == precioRestaurante,
                                  rating >= puntuacionRestaurante[1],
                                  rating <= puntuacionRestaurante[2])
nrow(restFinal)

# Filtramos los puntos de la ruta donde debería caer la ruta, nos quedamos el primero por orden.
# aux: la posición en la ruta
if (tipoTrans == "caminando") {
  aux <- which(puntosFin$ComidaCami)[1]
} else{
  aux <- which(puntosFin$ComidaCoche)[1]
}

if (!is.na(aux)) {
  cat("Entra en if length(aux) \n")
  
  # Si ha hay candidatos a restaurante en función de la elección del usuario
  if (nrow(restFinal) > 0){ 
    # Metemos la distancia caminando desde ese punto a todos los restaurantes con opciones
    for (i in 1:nrow(restFinal)){
      if (aux == 1){
        restFinal$distLugCami[i] <- distHaversine(restFinal[i,2:3], puntosFin[aux,1:2]) + distHaversine(restFinal[i,2:3], puntosFin[aux,1:2])
      } else {
        restFinal$distLugCami[i] <- distHaversine(restFinal[i,2:3], puntosFin[aux,1:2]) + distHaversine(restFinal[i,2:3], puntosFin[aux-1,1:2])
      }
      restFinal$distLugCami[i] <- restFinal$distLugCami[i]/1000*795/60 #Minutos
    }
    rm(i)
    
    # Nos quedamos con los restaurantes que estén a una distancia similar a la distancia que hay al siguiente punto + un retraso
    # Si no encontramos nada a esa distancia, vamos aumentando el retraso 1min hasta un máximo de 30min, para poder encontrar un sitio
    auxRestFinal <- restFinal
    ind <- T
    retraso <- 5
    while (ind & retraso < 30) {
      restFinal <- auxRestFinal
      restFinal <- restFinal %>% filter(distLugCami < puntosFin$tiempoCaminando[aux] + retraso)
      if (!is.na(restFinal[1,2])) ind <- F
      retraso <- retraso + 1
    }
  }


  if (!is.na(restFinal[1,2])){
    cat("Entra en if is.null(restFinal) \n")
    
    # Criterio para restaurantes
    restFinal$criterio <- restFinal$opiniones^0.1*restFinal$rating
    
    restFinal <- restFinal[order(restFinal$criterio, decreasing = T), ]

    print(restFinal[1,2:3]) # Nos quedamos con el de mayor criterio
    
    # Metemos el restaurante después del punto de referencia (aux)
    poligono <- rbind(puntosFin[1:(aux-1),1:2], restFinal[1,2:3], puntosFin[aux:nrow(puntosFin),1:2])
    
    cat("Ruta final con restaurantes\n")
    
  } else{
    poligono <- rbind(puntosFin[1:2])
    
    cat("Ruta final sin restaurantes\n")
    
  }

  } else {
  poligono <- rbind(puntosFin[1:2])
  
  cat("Ruta final sin restaurantes \n")
  }

View(poligono)

# Mapa de la ruta final con restaurante
leaflet() %>% 
  addTiles()  %>% 
  setView(lng= -3.693575, lat= 40.416826 , zoom=14) %>%
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012", group="NASA") %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite", group = "Blanco y Negro") %>%
  addCircleMarkers(poligono$lon, poligono$lat, 
                   fillOpacity = 0.7, color="red", radius=10, stroke=FALSE,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addCircleMarkers(poligono$lon[aux], poligono$lat[aux], 
                   fillOpacity = 1, color="green", radius=10, stroke=FALSE,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addPolylines(data = poligono, lng = poligono$lon, lat = poligono$lat, color = "red") %>%
  addLayersControl(
    baseGroups = c("NASA", "Blanco y Negro"),
    options = layersControlOptions(collapsed = F)
  )


##############################################################################################
############################################ THE END  ########################################
##############################################################################################









