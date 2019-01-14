library(googleway)
library(combinat)
library(leaflet)
library(sp)
library(geosphere)
library(maps)
library(mapproj)
library(shiny)
library(dplyr)
library(descr)
library(rvest)
library(stringr)

setwd("E:/Martin/99.Personal/KSCHOOL/20. TFM/MyStopOver")

# Autorización a API Google

key <- "AIzaSyDketUNiJG3LMyMygdlofsXmOsDrdR0_5k"

################################### LIMPIEZA Y ORDEN DE DATOS ################################
################################ AÑADIMOS INFO DE GOOGLE PLACES ##############################
##############################################################################################

############### PUNTOS DE INTERÉS

# Lugares de interés extraídos de TripAdvisor por webscraping
pdi <- read.csv2("Data/tripadvisorPDI.csv", stringsAsFactors = F)
pdi$busqueda <- paste(pdi$nombre, "Madrid", sep = ", ")
pdi$busqueda <- chartr('??????????????','aeiounuAEIOUUN', pdi$busqueda) 

# Echamos un vistazo
hist(pdi$opiniones)
hist(pdi$opiniones, breaks = 1000, xlim = c(0,1000))

# Nos quedamos solo con los puntos de interés de los que tenemos al menos 100 opiniones
pdi <- pdi[order(pdi$opiniones, decreasing = TRUE),]
pdi100 <- sum(pdi$opiniones>=100)
pdi100

# De Google Places obtenemos: formatted_address, geometry, icon, id, name, open_now, photos, place_id, rating, types, price_level 
# Además de las coordenadas: latitud y longitud
# YA NO FUNCIONA la función google_places! Han cambiado la API! En su momento se extrajeron correctamente los datos
for (i in 1:nrow(pdi)){
  places <- google_places(search_string = pdi$busqueda[i],
                          key = key)
  results <- places$results
  
  # Parches para que no de errores en caso de que no encuentre el sitio o que tenga algunos valores NULL
  if (is.null(dim(results))) {
    results <- data.frame(formatted_address = NA, lat = NA, lon = NA, icon = NA, id = NA, name = NA, 
                          open_now = NA, photos = NA, place_id = NA, rating = NA, types = NA, price_level = NA)
  } else {
    if (is.null(results$price_level)) results$price_level <- NA
    if (is.null(results$opening_hours$open_now)) results$open_now <- NA else results$open_now <- results$opening_hours$open_now
    if (is.null(results$photos)) results$photos <- NA
    
    # Seleccionamos solamente las variables que nos interesan  
    results <- select(results, formatted_address, geometry, icon, id, name, open_now, photos, place_id, rating, types, price_level)
    
    # Se genera otra variable para quitar el dataframe dentro del dataframe en las coordenadas
    results2 <- data.frame(results[1], lat = results$geometry$location$lat, lon = results$geometry$location$lng,
                           results[3:11])
  }
  
  # Apendizamos los resultados
  if (i == 1) {
    pdiGoogle <- results2[1, ]
  } else {
    pdiGoogle <- rbind(pdiGoogle, results2[1, ])
  }
}

# Quitamos variables que ya no usaremos
rm(i, results, results2, places) 

# DF final con los puntos de interés
# Juntamos la información que ta teniamos del webscraper.io con la información de la API de Google.
LUGARES <- cbind(pdi[1:158, ], pdiGoogle)

# Añadimos tiempo, tipo de lugar y corregimos rating de lugares

# Agrupación Lugares: Hay demasiadas categorías. Reducimos agrupando
agrupLugar <- read.csv2("Data/tipoLugar.csv", stringsAsFactors = F)
LUGARES <- left_join(LUGARES, agrupLugar[c(1,4,5)])
LUGARES$Agrupacion[40] <- "Entretenimiento"  # Teleferico de Madrid
rm(agrupLugar)

coord <- LUGARES[c("lat","lon","rating","types", "nombre", "tipo", "opiniones", "Agrupacion", "tiempoNecesario")]
coord[111,1:2] <- c(40.416190, -3.707469)   # Cambiamos las coordenadas de Calle Mayor manualmente (geolocalizada en Burgos)
row.names(coord) <- NULL

# Asignamos un tiempo necesario para visitar el lugar.
# coord$tiempoNecesario <- .5

COORD <- coord

# Asigamos la media de puntuación a los que no tienen puntuación (quitando a los que no tienen puntuación para la media)
COORD$rating[which(COORD$rating == 0)] <- mean(COORD$rating[which(COORD$rating != 0)])


############### RESTAURANTES

# Restaurantes extraídos de TripAdvisor por webscraping
rest <- read.csv2("Data/tripadvisorRest.csv", stringsAsFactors = F)
# Se han generado duplicados durante el scraping. Los quitamos.
rest <- rest[!duplicated(rest), ]
# Nos quedamos con los restaurantes con más de 100 opiniones y que tengan una valoración de precio
rest <- rest %>% filter(opiniones >= 100, precio == "€" | precio == "€€ - €€€" | precio == "€€€€")

# Creamos una variable de precio en númerico
rest$precioNum <- 1
rest$precioNum[which(rest$precio == "€€ - €€€")] <- 2
rest$precioNum[which(rest$precio == "€€€€")] <- 3

# Tenemos que agrupar por restaurante porque tenemos una observación por cada estilo de comida del restaurante.
# Como hay restaurantes franquiciados, sacamos una nota, precio y opiniones medias
restAgrup <- rest %>% group_by(nombre) %>% summarise(opiniones =  round(mean(opiniones)), rank = min(rank), precio = round(mean(precioNum))) %>% ungroup()

# Creamos la búsqueda y limpiamos
restAgrup$busqueda <- paste(restAgrup$nombre, "Madrid", sep = ", ")
restAgrup$busqueda <- chartr('áéíóúñüÁÉÍÓÚÑÜ&òèàìù','aeiounuAEIOUNUyoeaiu', restAgrup$busqueda) 
restAgrup$busqueda <- gsub("'", " ", restAgrup$busqueda )

nrow(restAgrup)

# Creamos una función para extraer información de la API de Google Places
# YA NO FUNCIONA la función google_places! Han cambiado la API! En su momento se extrajeron correctamente los datos
# De Google Places obtenemos: formatted_address, geometry, icon, id, name, open_now, photos, place_id, rating, types, price_level
# Además de las coordenadas: latitud y longitud

leerRestaurante <- function(restaurante, key){
  places <- google_places(search_string = restaurante, key = key)
  results <- places$results 
  
  # Parches para que no de errores en caso de que no encuentre el restaurante o que tenga algunos valores NULL
  if (is.null(dim(results))) {
    results2 <- data.frame(formatted_address = NA, lat = NA, lon = NA, icon = NA, id = NA, name = NA, 
                           open_now = NA, photos = NA, place_id = NA, rating = NA, types = NA, price_level = NA)
  } else {
    if (is.null(results$price_level)) results$price_level <- NA
    if (is.null(results$opening_hours$open_now)) results$open_now <- NA else results$open_now <- results$opening_hours$open_now
    if (is.null(results$photos)) results$photos <- NA
    
    results <- select(results, formatted_address, geometry, icon, id, name, open_now, photos, place_id, rating, types, price_level)
    
    results2 <- data.frame(results[1], lat = results$geometry$location$lat, lon = results$geometry$location$lng,
                           results[3:11])
  }
  
  # En el caso de los restaurantes podemos encontrarnos con varios restaurantes de la misma cadena.
  # La API de Google nos muestra información de hasta 3 páginas de 20 resultados (60 en total).
  restFinal <- results2  # Guardamos la primera página
  
  cont <- 1  # Inicializamos contador del numero de páginas que ha leído
  
  # Si la página que ha leído tiene 20 resultados quiere decir que queda más información por leer
  # Si ya ha leído 3 páginas ya no puede seguir leyendo
  while (nrow(results2) == 20 & cont < 3){
    # Lectura de página con comprobación y pausa para que no haya errores.
    places[["status"]] <- "INVALID REQUEST"
    while(places[["status"]] != "OK"){
      Sys.sleep(2)
      places <- google_places(search_string = restaurante, key = key, page_token = places$next_page_token)
    }
    results <- places$results
    
    # Se repite lo anterior pero apendizando en restFinal
    if (is.null(dim(results))) {
      results2 <- data.frame(formatted_address = NA, lat = NA, lon = NA, icon = NA, id = NA, name = NA, 
                             open_now = NA, photos = NA, place_id = NA, rating = NA, types = NA, price_level = NA)
    } else {
      if (is.null(results$price_level)) results$price_level <- NA
      if (is.null(results$opening_hours$open_now)) results$open_now <- NA else results$open_now <- results$opening_hours$open_now
      if (is.null(results$photos)) results$photos <- NA
      
      results <- select(results, formatted_address, geometry, icon, id, name, open_now, photos, place_id, rating, types, price_level)
      
      results2 <- data.frame(results[1], lat = results$geometry$location$lat, lon = results$geometry$location$lng,
                             results[3:11])
    }
    cont <- cont + 1
    restFinal <- rbind(restFinal, results2)
  }
  return(restFinal)
}

# Pasamos la función por los Restaurantes
for (i in 1:nrow(restAgrup)){
  cat("Leyendo",i,"de",nrow(restAgrup),"\n")
  cat("Completado el", round(i/nrow(restAgrup)*100,1),"% \n")
  restAux <- leerRestaurante(restAgrup$busqueda[i], key)   # Lectura del restaurante
  
  # Juntamos la información de la API con la de webscraping
  restAux <- cbind(restAux, restAgrup[i, ])
  
  # Apendizamos
  if (i == 1) {
    restGoogle <- restAux
  } else {
    restGoogle <- rbind(restGoogle, restAux)
  }
  cat("\014")
}
rm(i, restAux)

# DF Final de Restaurantres
RESTAURANTES <- restGoogle 

# Limpieza Restaurantes

RESTAURANTES$esRestaurante <- F
for (i in 1:nrow(RESTAURANTES)) {
  RESTAURANTES$esRestaurante[i] <- "restaurant" %in% RESTAURANTES$types[[i]]
}

RESTAURANTES <- RESTAURANTES %>% filter(rating > 0, esRestaurante)
RESTAURANTES <- RESTAURANTES[!duplicated(RESTAURANTES$formatted_address), ]

# Agrupaciones Restaurantes
# Agrupamos los resturantes para tener menos categorías

agrupRest <- read.csv2("Data/estiloRest.csv", stringsAsFactors = F)

rest <- left_join(rest, agrupRest[c(1,4)])
rm(agrupRest)
restEstilos <- rest %>% group_by(nombre, opiniones, rank, precio, precioNum, Agrupacion) %>% summarise() %>% ungroup()
restEstilos <- filter(restEstilos, Agrupacion != "")

restEstilos$FastFood <- restEstilos$Asiatica <- restEstilos$Arabe <- restEstilos$Americana <- restEstilos$Africana <- 0
restEstilos$Mediterranea <- restEstilos$Italiana <- restEstilos$India <- restEstilos$Europea <- restEstilos$Espa?ola <- 0
restEstilos$Veggie <- restEstilos$Sudamericana <- restEstilos$Mexicana <- 0

restEstilos$Africana[which(restEstilos$Agrupacion == "Africana")] <- 1
restEstilos$Americana[which(restEstilos$Agrupacion == "Americana")] <- 1
restEstilos$Arabe[which(restEstilos$Agrupacion == "Arabe")] <- 1
restEstilos$Asiatica[which(restEstilos$Agrupacion == "Asiatica")] <- 1
restEstilos$FastFood[which(restEstilos$Agrupacion == "Comida Rápida")] <- 1
restEstilos$Espa?ola[which(restEstilos$Agrupacion == "Española")] <- 1
restEstilos$Europea[which(restEstilos$Agrupacion == "Europea")] <- 1 
restEstilos$India[which(restEstilos$Agrupacion == "India")] <- 1
restEstilos$Italiana[which(restEstilos$Agrupacion == "Italiana")] <- 1
restEstilos$Mediterranea[which(restEstilos$Agrupacion == "Mediterranea")] <- 1
restEstilos$Mexicana[which(restEstilos$Agrupacion == "Mexicana")] <- 1
restEstilos$Sudamericana[which(restEstilos$Agrupacion == "Sudamericana")] <- 1
restEstilos$Veggie[which(restEstilos$Agrupacion == "Veggie")] <- 1

restEstilos <- restEstilos %>% group_by(nombre) %>% 
  summarise(Africana = max(Africana), Americana = max(Americana), Arabe = max(Arabe), Asiatica = max(Asiatica),
            FastFood = max(FastFood), Espa?ola = max(Espa?ola), Europea = max(Europea), India = max(India),
            Italiana = max(Italiana), Mediterranea = max(Mediterranea), Mexicana = max(Mexicana),
            Sudamericana = max(Sudamericana), Veggie = max(Veggie)) %>% ungroup()

RESTAURANTES <- left_join(RESTAURANTES, restEstilos)


##############################################################################################
############################################ THE END  ########################################
##############################################################################################


