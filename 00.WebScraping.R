library(dplyr)
library(stringr)

setwd("E:/Martin/99.Personal/KSCHOOL/20. TFM/MyStopOver")

##################################### OBTENCiÓN DE LOS DATOS #################################
########################################### WEBSCRAPING ######################################
##############################################################################################

############### PUNTOS DE INTERÉS

# Obtenemos información de las principales atracciones de Madrid:
# Nombre de sitio, posición en el ranking, número de opiniones y tipología del sitio.
# CAMBIO de Web desde que se obtuvieron los datos!!!
# Ahora no se puede obtener directamente la información sobre el ranking y tipología del sitio.

# Como la primera pagina no carga el enlace es oa30, sino hubiese sido oa00.
urlPDI <- "https://www.tripadvisor.es/Attractions-g187514-Activities-oa30-Madrid.html"

# Creamos una función que lee los enlaces de los Puntos de Interés (PDI) en una sola página
leerEnlacesPDI <- function(urlPDI){
  page <- read_html(urlPDI)
  PDI <- page %>% html_nodes("div.listing_title a") %>% html_attrs() # Enlaces
  
  # Pasamos la información a data.frame
  for (i in 1:length(PDI)){
    enlaceAux <- PDI[[i]][["href"]]
    if (i == 1){
      enlace <- enlaceAux
    } else{
      enlace <- c(enlace, enlaceAux)
    }
  }
  
  # Creamos la url completa
  enlace <- paste("https://www.tripadvisor.es", enlace, sep = "")
  
  nombre <- page %>% html_nodes("div.listing_title a") %>% html_text()  # Nombre del Punto de Interés
  numOpi <- page %>% html_nodes("span.more a") %>% html_text()  # Número de opiniones
  # ranking: Ya no funciona!
  # tipología: Ya no funciona!
  
  enlace <- data.frame(nombre, numOpi, enlace)  # Juntamos todo
  enlace <- enlace %>% mutate_if(is.factor, as.character)
  return(enlace)
}

# La web tiene varias páginas con 30 puntos de interés cada una
# Creamos una función que lee todos los enlaces de todas las páginas de los puntos de interés
leerAllLink <- function(urlPDI){
  page <- read_html(urlPDI)
  numPag <- page %>% html_nodes("a.pageNum") %>% html_attrs()  # Obtenemos el total de páginas a scrapear
  numPag <- as.numeric(numPag[[6]][["data-page-number"]])
  
  # Creamos los enlaces a visitar. La web muestra de 30 en 30 con la estructura "Activities-oa$" donde '$' es un múltiplo de 30
  paginas <- (1:(numPag-1))*30
  pagsPDI <- str_split(urlPDI, "Activities-oa30-")
  pagsPDI <- paste(pagsPDI[[1]][1],"Activities-oa",paginas,"-",pagsPDI[[1]][2], sep = "")
  pagsPDI <- c(urlPDI, pagsPDI)
  
  # Entramos a cada enlace y obtenemos la información con la función que hemos creado antes
  for (i in 1:(numPag-2)){
    dataAux <- leerEnlacesPDI(pagsPDI[i])
    if (i == 1) {
      data <- dataAux
    } else {
      data <- rbind(data, dataAux)
    }
    cat("Completada pagina",i,"de",numPag,"\n")
  }
  return(data)
}

# Obtenemos todos los enlaces de los puntos de interés, el nombre y número de opiniones
enlacesPDI <- leerAllLink(urlPDI)

nrow(enlacesPDI)

# Damos formato a la salida
enlacesPDI$nombre <- gsub("\n","",enlacesPDI$nombre)
enlacesPDI$numOpi <- gsub("\n","",enlacesPDI$numOpi)
enlacesPDI$numOpi <- gsub(" opiniones","",enlacesPDI$numOpi)
enlacesPDI$numOpi <- gsub(" opinión","",enlacesPDI$numOpi)
enlacesPDI$numOpi <- gsub("\\.","",enlacesPDI$numOpi)
enlacesPDI$numOpi <- as.numeric(enlacesPDI$numOpi)

head(enlacesPDI)

# write.csv2(enlacesPDI,"Data/tripadvisorPDI.csv")

# Ahora replicar para cualquier ciudad es posible. Por ejemplo: París
urlParis <- "https://www.tripadvisor.es/Attractions-g187147-Activities-oa30-Paris_Ile_de_France.html"
PDIParis <- leerEnlacesPDI(urlParis)
head(PDIParis)


############### RESTAURANTES

# Obtenemos información de todos los restaurantes de Madrid.
# Nombre de restaurante, posición en el ranking, número de opiniones, precio y tipología de comida.
# CAMBIO de Web desde que se obtuvieron los datos!!!
# Ahora sólo se puede leer la primera página. 
# Antes la url hacía referencia a la página y ahora no "https://www.tripadvisor.es/Restaurants-g187514-Madrid.html#EATERY_OVERVIEW_BOX"

urlRest <- "https://www.tripadvisor.es/Restaurants-g187514-Madrid.html#EATERY_OVERVIEW_BOX"

# Creamos una función que lee los enlaces de los Restaurantes en una sola página
leerEnlacesRest <- function(urlRest){
  page <- read_html(urlRest)
  REST <- page %>% html_nodes("a.property_title") %>% html_attrs() # Enlaces
  
  # Pasamos la información a data.frame
  for (i in 1:length(REST)){
    enlaceAux <- REST[[i]][["href"]]
    if (i == 1){
      enlace <- enlaceAux
    } else{
      enlace <- c(enlace, enlaceAux)
    }
  }
  
  # Creamos la url completa
  enlace <- paste("https://www.tripadvisor.es", enlace, sep = "")
  
  nombre <- page %>% html_nodes("a.property_title") %>% html_text()  # Nombre del Punto de Interés
  numOpi <- page %>% html_nodes("div.rating.rebrand") %>% html_text() # Número de opiniones
  precio <- page %>% html_nodes("span.item.price") %>% html_text()  # Precio del restaurante
  tipo <- page %>% html_nodes("a.item.cuisine") %>% html_text()  # Tipo de comida del restaurante. 
  ranking <- page %>% html_nodes("div.popIndex.rebrand.popIndexDefault") %>% html_text() 
  
  
  enlace <- data.frame(nombre, numOpi, precio, ranking, enlace)  # Juntamos todo #tipo: YA no está codificado como estaba!
  enlace <- enlace %>% mutate_if(is.factor, as.character)
  return(enlace)
}

# Obtenemos el DF con los restaurantes
enlacesREST <- leerEnlacesRest(urlRest)

# Como hemos comentado arriba, el formato de las url ha cambiado y ahora solo podemos obtener los resultados de la primera página
# 30 Restaurantes de los 10k que hay.
nrow(enlacesREST)

# En su momento obtuvimos todos, guardados en:
# write.csv2(enlacesREST,"Data/tripadvisorRest.csv")

##############################################################################################
############################################ THE END  ########################################
##############################################################################################
