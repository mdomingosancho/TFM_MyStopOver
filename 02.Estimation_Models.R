library(TSP)
library(googleway)
library(combinat)
library(leaflet)
library(sp)
library(geosphere)
library(maps)
library(mapproj)
library(dplyr)
library(descr)
library(rvest)
library(stringr)

setwd("E:/Martin/99.Personal/KSCHOOL/20. TFM/MyStopOver")

# Autorización a API Google

key <- "AIzaSyDketUNiJG3LMyMygdlofsXmOsDrdR0_5k"

##############################################################################################
################################ MODELOS DE ESTIMACIÓN DE TIEMPOS ############################
##############################################################################################


################################ GENERAMOS PUNTOS PARA LOS MODELOS ###########################
##############################################################################################

# OTROS DATOS: Polígonos Madrid y M30

# Nos creamos nuestro polígono de Madrid
madrid <- data.frame(lon = c(40.478892, 40.519362, 40.501121, 40.453995, 40.382643, 40.359017, 40.381766, 40.422815, 40.478892),
                     lat = c(-3.751073, -3.675882, -3.594368, -3.567252, -3.625568, -3.761211, -3.787429, -3.741065, -3.751073))

# Nos creamos nuestro polígono de la M30 (esto nos valdrá para complicar nuestro modelo de estimación de tiempos)
M30 <- data.frame(lon = c(40.401326, 40.420407, 40.473193, 40.485508, 40.483421, 40.440550, 40.419117, 40.388270, 40.401326),
                  lat = c(-3.720949, -3.721653, -3.750572, -3.695251, -3.672566, -3.659507, -3.658477, -3.685932, -3.720949))

# Ahora creamos ubicaciones ficticias alrededor Madrid 
ubic <- data.frame(lon = rep(seq(40.359017,40.519362,.001), each = length(seq(-3.787429, -3.567252, 0.001))), lat = seq(-3.787429, -3.567252, 0.001))

# Nos quedamos con los puntos que estan dentro del polígono 
#install.packages("sp")
#library(sp)
#?point.in.polygon

ubic$dentro <- point.in.polygon(ubic$lon, ubic$lat, madrid$lon, madrid$lat)
ubic <- dplyr::filter(ubic, dentro == 1)

# Nos quedamos con una muestra de 1000 puntos
set.seed(12345)
muestra <- ubic[sample(nrow(ubic), 1000), ]

# Mapa de los puntos finales
leaflet() %>% 
  addTiles()  %>% 
  setView(lng= -3.703575, lat= 40.416826 , zoom=12) %>%
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012", group="NASA") %>%
  addProviderTiles("OpenStreetMap.BlackAndWhite", group = "Blanco y Negro") %>%
  addCircleMarkers(madrid$lat, madrid$lon, 
                   fillOpacity = 0.5, color="black", radius=10, stroke=FALSE,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addCircleMarkers(muestra$lat, muestra$lon, 
                   fillOpacity = 0.7, color="blue", radius=2, stroke=FALSE,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
  ) %>%
  addPolylines(data = madrid, lng = madrid$lat, lat = madrid$lon, color = "black") %>%
  addPolylines(data = M30, lng = M30$lat, lat = M30$lon, color = "red") %>%
  addLayersControl(
    baseGroups = c("NASA", "Blanco y Negro"),
    options = layersControlOptions(collapsed = F)
  )

# Colocamos los puntos en parejas
muestra$lon2 <- NA
muestra$lon2[1:500] <- muestra$lon[501:1000]
muestra$lat2 <- NA
muestra$lat2[1:500] <- muestra$lat[501:1000]
muestra <- muestra[1:500, -3]

# Calculamos la Distancia entre los pares de puntos
muestra$dist <- distHaversine(muestra[1:2],muestra[3:4])/1000

muestra$tiempos2 <- muestra$distGoogle2 <- muestra$tiempos <- muestra$distGoogle <- NA

################################ MODELOS DE ESTIMACIÓN DE TIEMPOS ############################
##############################################################################################

####################
# Modelo caminando #
####################

# Distancia tiempo API Google caminando
# [La función google_distance ya no funciona! Cuando se hizo funcinaba]
for (i in 1:500){
  aux <- google_distance(muestra[i,1:2], muestra[i,3:4], mode = "walking", units = "metric", key = key)
  if (aux[["rows"]][["elements"]][[1]][["status"]] == 'OK'){
    muestra$distGoogle[i] <- aux[["rows"]][["elements"]][[1]][["distance"]][["value"]]
    muestra$tiempos[i] <- aux[["rows"]][["elements"]][[1]][["duration"]][["value"]]
    muestra$distGoogle2[i] <- aux[["rows"]][["elements"]][[1]][["distance"]][["text"]]
    muestra$tiempos2[i] <- aux[["rows"]][["elements"]][[1]][["duration"]][["text"]]
  } else {
    muestra$distGoogle2[i] <- aux[["rows"]][["elements"]][[1]][["status"]]
    muestra$tiempos2[i] <- aux[["rows"]][["elements"]][[1]][["status"]]
  }
}

plot(muestra$dist, muestra$tiempos)

plot(muestra$distGoogle, muestra$tiempos)

# Modelo caminando
modCaminando <- lm(tiempos ~ -1 + dist, muestra)
coefModCaminando <- coef(modCaminando)

###################
# Modelo en coche #
###################

# Muestra Coche
muestraCoche <- muestra

# Creamos un indicador de los puntos dentro de la M30
muestraCoche$dentroM30 <- point.in.polygon(muestraCoche$lon, muestraCoche$lat, M30$lon, M30$lat) + point.in.polygon(muestraCoche$lon2, muestraCoche$lat2, M30$lon, M30$lat)


# Distancia tiempo API Google conduciendo
# Sacamos los datos a las 18:30 pm
for (i in 1:500){
  aux <- google_distance(muestraCoche[i,1:2], muestraCoche[i,3:4], mode = "driving", units = "metric", key = key)
  if (aux[["rows"]][["elements"]][[1]][["status"]] == 'OK'){
    muestraCoche$distGoogle[i] <- aux[["rows"]][["elements"]][[1]][["distance"]][["value"]]
    muestraCoche$tiempos[i] <- aux[["rows"]][["elements"]][[1]][["duration"]][["value"]]
    muestraCoche$distGoogle2[i] <- aux[["rows"]][["elements"]][[1]][["distance"]][["text"]]
    muestraCoche$tiempos2[i] <- aux[["rows"]][["elements"]][[1]][["duration"]][["text"]]
  } else {
    muestraCoche$distGoogle2[i] <- aux[["rows"]][["elements"]][[1]][["status"]]
    muestraCoche$tiempos2[i] <- aux[["rows"]][["elements"]][[1]][["status"]]
  }
}

muestraCoche$dentroM30 <- as.factor(muestraCoche$dentroM30)

# Sacamos ahora una muestra a las 11:30 am
muestraCoche1130 <- muestraCoche

# Distancia tiempo API Google conduciendo
# Sacamos los datos a las 11:30 am
for (i in 1:500){
  aux <- google_distance(muestraCoche1130[i,1:2], muestraCoche1130[i,3:4], mode = "driving", units = "metric", key = key)
  if (aux[["rows"]][["elements"]][[1]][["status"]] == 'OK'){
    muestraCoche1130$distGoogle[i] <- aux[["rows"]][["elements"]][[1]][["distance"]][["value"]]
    muestraCoche1130$tiempos[i] <- aux[["rows"]][["elements"]][[1]][["duration"]][["value"]]
    muestraCoche1130$distGoogle2[i] <- aux[["rows"]][["elements"]][[1]][["distance"]][["text"]]
    muestraCoche1130$tiempos2[i] <- aux[["rows"]][["elements"]][[1]][["duration"]][["text"]]
  } else {
    muestraCoche1130$distGoogle2[i] <- aux[["rows"]][["elements"]][[1]][["status"]]
    muestraCoche1130$tiempos2[i] <- aux[["rows"]][["elements"]][[1]][["status"]]
  }
}


# Juntamos las 2 muestras y hacemos el modelo
muestraTotal <- rbind(muestraCoche[1:10], muestraCoche1130)
muestraTotal$hora <- "1830"
muestraTotal$hora[501:1000] <- "1130"

# Modelo en coche
set.seed(12345)
fold.test <- sample(nrow(muestraTotal), nrow(muestraTotal) / 5)
test  <- muestraTotal[fold.test, ]
train <- muestraTotal[-fold.test, ]
rm(fold.test)

set.seed(12345)
fold.test <- sample(nrow(muestraTotal), nrow(muestraTotal) / 5)
test  <- muestraTotal[fold.test, ]
train <- muestraTotal[-fold.test, ]
rm(fold.test)

modCoche <- lm(tiempos ~ -1 + dist + I(dist^2) + dentroM30, train)
summary(modCoche)
coefModCoche <- coef(modCoche)

test$prediccion <- predict(mod, test, interval = "prediction")
prediccion <- predict(mod, test, interval = "prediction")
test <- test[-(13:15)]
test <- cbind(test, prediccion)

# Prueba algoritmo para ir al punto más cercano
prueba <- data.frame ( Punt = c(5,4,4.3,3,2.1,4.8,4.5), dist = c(3.2,1.3,1.5,3.5,.5,3,5.2) , tiempoAtrac = c(1,1,1.5,.5,2,2,.5))
prueba$tiempoLug <- prueba$dist*coefModCaminando/3600
prueba$ratio <- prueba$Punt/(prueba$tiempoAtrac+prueba$tiempoLug)


##############################################################################################
############################################ THE END  ########################################
##############################################################################################

