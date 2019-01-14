library(maps)
library(mapproj)
library(dplyr)
library(descr)
library(rvest)
library(stringr)
library(RCurl)
library(RJSONIO)

setwd("E:/Martin/99.Personal/KSCHOOL/20. TFM/MyStopOver")

########################################### FUNCIONES ########################################
##############################################################################################

# Función para obtener longitud y latitud de un punto a partir de la API de Google Maps
url <- function(address, return.call = "json") {
  root <- "https://maps.googleapis.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address,"&key=", key, sep = "")
  return(URLencode(u))
}

geoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
  } else {
    return(c(NA,NA,x$error_message, x$status))
  }
}


# Función que dado un data frame con coordenadas te devuelve una matriz de tiempo caminando
matrizTiempoCaminando <- function(coord){
  tiempoCaminando <- matrix(NA, nrow(coord), nrow(coord))
  for (i in 1:nrow(coord)){
    for (j in 1:nrow(coord)){
      if (i <= j) {
        tiempoCaminando[i,j] <- distHaversine(coord[i,],coord[j,])/1000*coefModCaminando/60  # Minutos
      }
    }
  }
  rm(i, j)
  tiempoCaminando[lower.tri(tiempoCaminando)] <- t(tiempoCaminando)[lower.tri(tiempoCaminando)]
  return(tiempoCaminando)
}

# Función que dado un data frame con coordenadas te devuelve una matriz de tiempo en coche
matrizTiempoCoche <- function(coord){
  tiempoCoche <- matrix(NA, nrow(coord), nrow(coord))
  dentroM30 <- matrix(NA, nrow(coord), nrow(coord))
  for (i in 1:nrow(coord)){
    for (j in 1:nrow(coord)){
      if (i <= j) {
        dentroM30[i,j] <- point.in.polygon(coord$lat[i], coord$lon[i], M30$lon, M30$lat) + point.in.polygon(coord$lat[j], coord$lon[j], M30$lon, M30$lat)
        tiempoCoche[i,j] <- distHaversine(coord[i,],coord[j,])/1000
        tiempoCoche[i,j] <- coefModCoche[1]*tiempoCoche[i,j] + coefModCoche[2]*tiempoCoche[i,j]^2
        if (dentroM30[i,j] == 0) tiempoCoche[i,j] <- tiempoCoche[i,j] + coefModCoche[3]
        if (dentroM30[i,j] == 1) tiempoCoche[i,j] <- tiempoCoche[i,j] + coefModCoche[4]
        if (dentroM30[i,j] == 2) tiempoCoche[i,j] <- tiempoCoche[i,j] + coefModCoche[5]
        tiempoCoche[i,j] <- tiempoCoche[i,j]/60  # Minutos
      }
    }
  }
  rm(i, j)
  tiempoCoche[lower.tri(tiempoCoche)] <- t(tiempoCoche)[lower.tri(tiempoCoche)]
  return(tiempoCoche)
}

### Creamos la matriz de tiempos para nuestra BBDD
tiempoCaminando <- matrizTiempoCaminando(coord[c("lon", "lat")])
tiempoCoche <- matrizTiempoCoche(coord[c("lon", "lat")])
TIEMPOCAMINANDO <- tiempoCaminando
TIEMPOCOCHE <- tiempoCoche


##############################################################################################
############################################ THE END  ########################################
##############################################################################################









