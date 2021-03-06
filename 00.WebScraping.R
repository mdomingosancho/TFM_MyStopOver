library(dplyr)
library(stringr)

setwd("E:/Martin/99.Personal/KSCHOOL/20. TFM/MyStopOver")

##################################### OBTENCi�N DE LOS DATOS #################################
########################################### WEBSCRAPING ######################################
##############################################################################################

############### PUNTOS DE INTER�S

# Obtenemos informaci�n de las principales atracciones de Madrid:
# Nombre de sitio, posici�n en el ranking, n�mero de opiniones y tipolog�a del sitio.
# CAMBIO de Web desde que se obtuvieron los datos!!!
# Ahora no se puede obtener directamente la informaci�n sobre el ranking y tipolog�a del sitio.

# Como la primera pagina no carga el enlace es oa30, sino hubiese sido oa00.
urlPDI <- "https://www.tripadvisor.es/Attractions-g187514-Activities-oa30-Madrid.html"

# Creamos una funci�n que lee los enlaces de los Puntos de Inter�s (PDI) en una sola p�gina
leerEnlacesPDI <- function(urlPDI){
  page <- read_html(urlPDI)
  PDI <- page %>% html_nodes("div.listing_title a") %>% html_attrs() # Enlaces
  
  # Pasamos la informaci�n a data.frame
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
  
  nombre <- page %>% html_nodes("div.listing_title a") %>% html_text()  # Nombre del Punto de Inter�s
  numOpi <- page %>% html_nodes("span.more a") %>% html_text()  # N�mero de opiniones
  # ranking: Ya no funciona!
  # tipolog�a: Ya no funciona!
  
  enlace <- data.frame(nombre, numOpi, enlace)  # Juntamos todo
  enlace <- enlace %>% mutate_if(is.factor, as.character)
  return(enlace)
}

# La web tiene varias p�ginas con 30 puntos de inter�s cada una
# Creamos una funci�n que lee todos los enlaces de todas las p�ginas de los puntos de inter�s
leerAllLink <- function(urlPDI){
  page <- read_html(urlPDI)
  numPag <- page %>% html_nodes("a.pageNum") %>% html_attrs()  # Obtenemos el total de p�ginas a scrapear
  numPag <- as.numeric(numPag[[6]][["data-page-number"]])
  
  # Creamos los enlaces a visitar. La web muestra de 30 en 30 con la estructura "Activities-oa$" donde '$' es un m�ltiplo de 30
  paginas <- (1:(numPag-1))*30
  pagsPDI <- str_split(urlPDI, "Activities-oa30-")
  pagsPDI <- paste(pagsPDI[[1]][1],"Activities-oa",paginas,"-",pagsPDI[[1]][2], sep = "")
  pagsPDI <- c(urlPDI, pagsPDI)
  
  # Entramos a cada enlace y obtenemos la informaci�n con la funci�n que hemos creado antes
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

# Obtenemos todos los enlaces de los puntos de inter�s, el nombre y n�mero de opiniones
enlacesPDI <- leerAllLink(urlPDI)

nrow(enlacesPDI)

# Damos formato a la salida
enlacesPDI$nombre <- gsub("\n","",enlacesPDI$nombre)
enlacesPDI$numOpi <- gsub("\n","",enlacesPDI$numOpi)
enlacesPDI$numOpi <- gsub(" opiniones","",enlacesPDI$numOpi)
enlacesPDI$numOpi <- gsub(" opini�n","",enlacesPDI$numOpi)
enlacesPDI$numOpi <- gsub("\\.","",enlacesPDI$numOpi)
enlacesPDI$numOpi <- as.numeric(enlacesPDI$numOpi)

head(enlacesPDI)

# write.csv2(enlacesPDI,"Data/tripadvisorPDI.csv")

# Ahora replicar para cualquier ciudad es posible. Por ejemplo: Par�s
urlParis <- "https://www.tripadvisor.es/Attractions-g187147-Activities-oa30-Paris_Ile_de_France.html"
PDIParis <- leerEnlacesPDI(urlParis)
head(PDIParis)


############### RESTAURANTES

# Obtenemos informaci�n de todos los restaurantes de Madrid.
# Nombre de restaurante, posici�n en el ranking, n�mero de opiniones, precio y tipolog�a de comida.
# CAMBIO de Web desde que se obtuvieron los datos!!!
# Ahora s�lo se puede leer la primera p�gina. 
# Antes la url hac�a referencia a la p�gina y ahora no "https://www.tripadvisor.es/Restaurants-g187514-Madrid.html#EATERY_OVERVIEW_BOX"

urlRest <- "https://www.tripadvisor.es/Restaurants-g187514-Madrid.html#EATERY_OVERVIEW_BOX"

# Creamos una funci�n que lee los enlaces de los Restaurantes en una sola p�gina
leerEnlacesRest <- function(urlRest){
  page <- read_html(urlRest)
  REST <- page %>% html_nodes("a.property_title") %>% html_attrs() # Enlaces
  
  # Pasamos la informaci�n a data.frame
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
  
  nombre <- page %>% html_nodes("a.property_title") %>% html_text()  # Nombre del Punto de Inter�s
  numOpi <- page %>% html_nodes("div.rating.rebrand") %>% html_text() # N�mero de opiniones
  precio <- page %>% html_nodes("span.item.price") %>% html_text()  # Precio del restaurante
  tipo <- page %>% html_nodes("a.item.cuisine") %>% html_text()  # Tipo de comida del restaurante. 
  ranking <- page %>% html_nodes("div.popIndex.rebrand.popIndexDefault") %>% html_text() 
  
  
  enlace <- data.frame(nombre, numOpi, precio, ranking, enlace)  # Juntamos todo #tipo: YA no est� codificado como estaba!
  enlace <- enlace %>% mutate_if(is.factor, as.character)
  return(enlace)
}

# Obtenemos el DF con los restaurantes
enlacesREST <- leerEnlacesRest(urlRest)

# Como hemos comentado arriba, el formato de las url ha cambiado y ahora solo podemos obtener los resultados de la primera p�gina
# 30 Restaurantes de los 10k que hay.
nrow(enlacesREST)

# En su momento obtuvimos todos, guardados en:
# write.csv2(enlacesREST,"Data/tripadvisorRest.csv")

##############################################################################################
############################################ THE END  ########################################
##############################################################################################
