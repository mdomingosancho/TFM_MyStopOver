# TFM_MyStopOver

---
title: "MyStopOver"
subtitle: "TFM KSCHOOL"
author: "Martín Domingo Sancho"
date: "2 de noviembre de 2018"
output: html_document
runtime: shiny
---

```{r WorkingDirectory, echo=FALSE}
setwd("E:/Martin/99.Personal/KSCHOOL/20. TFM/MyStopOver")
```

```{r Logo, echo=FALSE, out.width = '40%'}
knitr::include_graphics('Images/Logo_MyStopOver.png')
```

# 0. Antecendentes

>Blablablababablalbaba

# 1. Objetivo

El objetivo del trabajo es establecer una interfaz que muestre rutas personalizadas a los usuarios en base a:

 * Punto de partida y llegada\n
 
 * Hora de salida y llegada\n
 
 * Modo de transporte (a pie, coche, bici,…)\n
 
 * Preferencias del usuario (museos, tiendas, espectáculos, lugres de interés,…)
 
 * Presupuesto y Rating

# 2. Datos

```{r Carga_Datos, echo=FALSE,warning=FALSE,message=FALSE,error=FALSE}
load("Data_MyStopOver.RData")
library(dplyr)
library(kableExtra)
```

```{r, echo=FALSE,warning=FALSE,message=FALSE,error=FALSE}
coord <- COORD
tiempoCaminando <- TIEMPOCAMINANDO
tiempoCoche <- TIEMPOCOCHE
restFinal <- RESTAURANTES
M30 <- M30
```   

### 2.1. Obetención de Datos
En un primer intento se obtuvieron los datos de la API de *Google Places*\n

Dado que una gran parte de los resultados obtenidos provenían de 'places' autoeditados por los usuarios y no se disponía del número de reviews de cada lugar, se decidió hacer web-scraping de www.tripadvisor.com\n

De aquí obtuvimos las referencias de **767 puntos de interés** asociados a Madrid y más de **2.600 restaurantes** de la misma ciudad.

Ver *00.WebScraping.R*

La información disponible era:

```{r head PDI , echo=FALSE}
headPDI <- head(read.csv2('Data/tripadvisorPDI.csv'),5)
kable(headPDI,caption="Muestra información obtenida de los puntos de interés")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive", "condensed"), full_width = F, position = "left")
```

```{r head Restaurantes , echo=FALSE}
headRest <- sample_n(read.csv2('Data/tripadvisorRest.csv'),size = 5)
kable(headRest,caption="Muestra información obtenida de los restaurantes")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive", "condensed"), full_width = F, position = "left")
```

### 2.2. Limpieza y Enriquecimiento
Tanto para los puntos de interés como para los restaurantes se exigió que tuvieran un mínimo de 100 reviews para poder contar con información robusta, y además para los restaurantes que contaran con una valoracíon económica.

Con esto redujimos los **puntos de interés a 158** y los **restaurantes a 2.000**

Para poder ubicar todos estos puntos, calcular distancias y tiempos entre ellos, se recurrió a la API de GoogleMaps, a partir de la cual se obtuvieron sus coordenadas **latitud** y **longitud**. Además de otras características como el rating, dirección,...

```{r head PDI_ok , echo=FALSE}
headPDIok <- sample_n(coord[c(5,1,2,6,8,7,3)],size=5)
kable(headPDIok,caption="Muestra de la información final sobre Puntos de Interés")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive", "condensed"), full_width = F, position = "left")
```

```{r head Restaurantes_ok , echo=FALSE}
headRestok <- sample_n(restFinal[c(13,1,2,3,14,15,16,10)],size=5)
kable(headRestok,caption="Muestra de la información final sobre restaurantes")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive", "condensed"), full_width = F, position = "left")
```

Ver *01.Working_with_Data.R*

```{r, echo=FALSE,warning=FALSE,message=FALSE,error=FALSE}
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
library(kableExtra)
library(DT)
library(ggmap)
```


```{r, echo=FALSE,warning=FALSE,message=FALSE,error=FALSE}
key <- "AIzaSyDketUNiJG3LMyMygdlofsXmOsDrdR0_5k"
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
```

Por ejemplo: Las coordenadas de las aulas de KSCHOOL serían

```{r, echo=FALSE,warning=FALSE,message=FALSE,error=FALSE,comment=""}
kschool <- "Avda. de Brasil 6, Madrid"
sprintf("Latitud: %s , Longitud: %s", geoCode(kschool)[1], geoCode(kschool)[2])   
```

### 2.3. Datos de entrada *(elección del usuario)*

* **Fijamos el punto de inicio y fin de la ruta:**

    + Ponemos siempre la palabra "Madrid" detrás para descartar ambigüedades de localizaciones. Por ejemplo si meto "Puerta del Sol",
    me devuelve coordenadas de Irvine, USA

```{r, echo=FALSE,warning=FALSE,message=FALSE,error=FALSE,comment=""}
ptoOrigen <- geoCode(paste("Puerta del Sol", "Madrid", sep = ", "))
ptoFinal <- geoCode(paste("Plaza de Colon", "Madrid", sep = ", "))
```

* **Fijamos las horas de inicio y fin de la ruta:**

    + Si pasa de XXhoras le sumamos 24,...
    + Si en el tiempo que queremos hacer la ruta, coincide con la hora de la comida, metemos una parada para comer, de 1h de duración
```{r hora inicio y fin, echo=FALSE,warning=FALSE,message=FALSE,error=FALSE,comment=""}
    horaInicio <- 8.0
    horaFinal <- 17.0
```

* **Fijamos el tipo de transporte:**

    + Podemos elegir entre hace la ruta andando si queremos ir dando un paseo o en coche si tenemos que recorrer largas distancias (por ejemplo si venimos desde el Aeropuerto)
```{r tipo transporte, echo=FALSE,warning=FALSE,message=FALSE,error=FALSE,comment=""}
    tipoTrans <- "caminando"
```    

* **Elegimos la tipología de lugares a visitar y el rango de puntuación de los mismos:**

    + Podemos elegir entre...
```{r lugares a visitar, echo=FALSE,warning=FALSE,message=FALSE,error=FALSE,comment=""}
    tipoLugar <- c("Museos")
    puntuacionMinima <- c(4.0,5)
```    

```{r lista lugares a visitar, echo=FALSE,warning=FALSE,message=FALSE,error=FALSE,comment=""}
  listaPlaces <- unique(COORD$Agrupacion)
  kable(listaPlaces)%>%
  kable_styling(bootstrap_options = c("condensed"), full_width = F, position = "center", font_size = 12)    
```    


* **También podemos elegir qué tipo de comida queremos, puntuación y rango de precios:**

    + Podemos elegir entre...
```{r tipo comida, echo=FALSE,warning=FALSE,message=FALSE,error=FALSE,comment=""}
    estiloComida <- "Española"
    puntuacionRestaurante <- c(4.5,5)
    precioRestaurante <- 2
```    

```{r lista tipo comida, echo=FALSE,warning=FALSE,message=FALSE,error=FALSE,comment=""}
  listaRest <- unique(restFinal$estilo)
  kable(listaRest)%>%
  kable_styling(bootstrap_options = c("condensed"), full_width = F, position = "center", font_size = 12)    
```    


### Resumen de la elección (Ejemplo):
```{r, echo=FALSE,warning=FALSE,message=FALSE,error=FALSE,comment=""}
a1 <- sprintf("Punto Inicio: %s , Punto Final: %s","Puerta del Sol, Madrid","Plaza de Colon, Madrid")
a2 <- sprintf("Hora Inicio: %s , Hora Final: %s",horaInicio,horaFinal)
a3 <- sprintf("Modo de Transporte: %s",tipoTrans)
a4 <- sprintf("Tipo de lugares a visitar: %s con una puntuación entre: %s y %s",tipoLugar,puntuacionMinima[1],puntuacionMinima[2])
a5 <- sprintf("Tipo de comida: %s con una puntuación entre: %s y %s, y un precio (1 a 3) de: %s", estiloComida, puntuacionRestaurante[1], puntuacionRestaurante[2], precioRestaurante)
Ejemplo <- c(a1,a2,a3,a4,a5)
Ejemplo_df <- data.frame(Ejemplo)
kable(Ejemplo_df,caption="Ejemplo de elección del usuario")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive", full_width = F))
```    

# 3. Modelos de estimación de tiempos
Dado que tenemos unos 2.200 puntos entre lugares de interés y restaurantes, si quisiéramos obtener todas las distancias entre los puntos necesitaríamos una matriz con casi 5millones de combinaciones. Trabajar con un matriz de distancias haría que el proceso de cálculo fuera lento y nos obligaríamos a relizar un número muy elevado de consultas a la API de Google Maps con su correspondiente coste.

Para ello, vamos a crear dos modelos de estimación de tiempos en base a las distancias y tiempos obtenidos mediante la API de Google Maps de ciertos puntos aleatorios:

**Modelo caminando:** estimado el tiempo caminando entre dos puntos, a partir de sus coordenadas.

**Modelo en coche:** estimado el tiempo en coche entre dos puntos, a partir de sus coordenadas, de la franja horaria (simplificada en mañana y tarde) y si de trata de un desplazamiento dentro de la M30, fuera o mixto.

Ver *02.Estimation_Models.R*

# 4. Matrices de tiempos
### Filtro de los lugares y matrices de tiempos
Dado que trabajar con matrices de tiempos puede resultar algo lento si se trabaja con todas las posibles combinaciones, filtramos de antemano las selecciones que haya hecho el usuario, reduciendo la cantidad de combinaciones posibles y el tiempo de cálculo.

### Cálculo de distancias del origen y final a la BBDD
Para ello utilizo creo una la función *'tiempo_origen_final'* que calcula las ditancias entre las coordenadas del punto inicial y final de la ruta que hemos planteado respecto a las coordenadas de todos los puntos de interés. 

A continuación transfomamos estas distancias en tiempos, a partir de los modelos que ya hemos creado en el punto **3. Modelos de estimación de tiempos**.

Ver *03.Functions.R*

# 5. Cálculo de la Ruta Óptima
### 5.1 Criterio de selección de Lugares
**sumaTiempos:** calcula para cada punto el tiempo que tarda en ir hasta allí desde el origen y llegar hasta el final

**multiplicador:** Ponderamos las distacias para darle mayor importancia a los puntos que están más cerca y menos a los más lejanos. De esta manera intentará siempre ir al más cercano. EL multiplicador dará valor 0 al punto más cercano del inicio y final, y valor 1 al punto más alejado. Le añadimos +0.5 al final para que el multiplicador no pueda quedarse a 0.

**ImpTiempo:** Constante de importancia: en función del rating del punto, del número de opiniones (raíz cuadrada para igualar) y de las distancias.

### 5.2 Algoritmo para el cálculo de la ruta Óptima
El problema al que nos enfrentamos es un **problema clásico de la Estadística**.

Se trata del problema del Vendedor Viajante (Travel Salesman Problem)
* [Travel Salesman Problem (Wikipedia)](https://en.wikipedia.org/wiki/Travelling_salesman_problem)
* [Paquete TSP para R](https://cran.r-project.org/package=TSP/TSP.pdf)

### 5.3 Preparo los datos para aplicar el Algoritmo TSP
El tiempo disponible para realizar la ruta será la diferencia entre la hora final y la inicial, menos el tiempo que dediquemos a comer *(si aplica)*    
Ahora reordenamos las matrices de tiempos teniendo en cuenta la importancia de los puntos, según los criterios anteriores.

También reordamos nuestros puntos de interés y reseteamos el índice

    
#### 5.3.1 Calculamos la ruta óptima

Primero, creamos la función *'ampliarMatrizYRutaTSPfin'* de la siguiente forma:

Para eso, creamos la función *'calcularRutaFin'* de la siguiente forma:

#### 5.3.2 Ponemos un orden a la ruta

```{r Ejemplo DataFrame Puntos finales 1, echo=FALSE}
Ej_1 <- read.csv2('Ejemplo_df1.csv')
Ej_1$tiempoNecesario <- Ej_1$tiempoNecesario*60
kable(Ej_1,caption="En verde el punto inicial en rojo el el final")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"), full_width = F, position = "left")%>%
  row_spec(1,background = "lightcoral")%>% 
  row_spec(5,background = "palegreen")
```


#### 5.3.3 Agregamos los tiempos a destino
Añadimos el tiempo que se tarda en llegar desde cada punto al siguiente:

```{r Ejemplo DataFrame Puntos finales 2, echo=FALSE}
Ej_2 <- read.csv2('Ejemplo_df2.csv')
#Ej_2$nombre <- paste(substr(Ej_2$nombre,1,9),"...")
Ej_2$tiempoNecesario <- Ej_2$tiempoNecesario*60
Ej_2ok <- Ej_2[c(-5,-6,-7)]
kable(Ej_2ok,caption="Ruta Óptima ...")%>%
  kable_styling(bootstrap_options = c("striped","hover", "responsive"), full_width = F, position = "left") %>% 
  row_spec(5,background = "lightcoral")%>% 
  row_spec(1,background = "palegreen")
```

#### 5.3.4 Agregamos lógica de restaurantes
Después de acumular los tiempos, vemos qué tramo es el idóneo para añadir el restaurante para comer.
Para ello nos quedamos con los restaurantes que cumplen con la selección del usuario, y se prioriza en función del resto de variables **(criterio)**: Distancia al punto anterior y siguiente, número de opiniones y rating. La idea es quedarnos con un restaurante que se encuentre en el 'camino' entre los puntos para no tener que desviarnos.

```{r Ejemplo DataFrame Restaurante, echo=FALSE}
Ej_rest2 <- read.csv2('Ejemplo_rest2.csv')
kable(Ej_rest2,caption="Restaurante elegido según criterios y algoritmo:")%>%
  kable_styling(bootstrap_options = c("striped","hover", "responsive"), full_width = F, position = "left")
```

***
### 5.3.5 Añadimos el restaurante a la ruta

```{r Ejemplo DataFrame Puntos finales 5, echo=FALSE}
Ej_5 <- read.csv2('Ejemplo_df5.csv')
#Ej_5$nombre <- paste(substr(Ej_5$nombre,1,9),"...")
kable(Ej_5,caption="En amarillo hemos incluido el restaurante en la ruta")%>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"), full_width = F, position = "left") %>% 
  row_spec(6,background = "lightcoral")%>%
  row_spec(4,background = "khaki")%>% 
  row_spec(1,background = "palegreen")
```


### 6. Ouput Final

# Imagen

# 7. ShinyApp

El resultado final es un ShinyApp donde el usuario puede crear su propia ruta en base a sus preferencias

https://mdomingosancho.shinyapps.io/AppMyStopover


Ver *05.MyStopOverApp.R*


***
