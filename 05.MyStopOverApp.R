library(TSP)
library(leaflet)
library(sp)
library(geosphere)
library(shiny)
library(dplyr)
library(shinyTime)

load("Data_MyStopOver.RData")
source("GeoCode.R")
key <- "AIzaSyDketUNiJG3LMyMygdlofsXmOsDrdR0_5k"
setwd("E:/Martin/99.Personal/KSCHOOL/20. TFM/MyStopover/Images")


myPoiIcon <- makeIcon(
  iconUrl = "poi_marker.png",
  iconWidth = 35, iconHeight = 50,
  iconAnchorX = 18, iconAnchorY = 49
)

myRestIcon <- makeIcon(
  iconUrl = "restaurant_icon.png",
  iconWidth = 50, iconHeight = 50,
  iconAnchorX = 20, iconAnchorY = 20
)


myStartIcon <- makeIcon(
  iconUrl = "start.png",
  iconWidth = 50, iconHeight = 50,
  iconAnchorX = 25, iconAnchorY = 45
)


myEndIcon <- makeIcon(
  iconUrl = "flag.png",
  iconWidth = 50, iconHeight = 50,
  iconAnchorX = 7, iconAnchorY = 48
)

origen <- "Hotel NH Collection Madrid Eurobuilding"
final <- "Estacion de Atocha"
ptoOrigen <- c("40.4583051", "-3.685973", "ROOFTOP", "Calle de Padre Damián, 23, 28036 Madrid, Spain")
ptoFinal <- c( "40.4067477", "-3.691094", "GEOMETRIC_CENTER", "Madrid Atocha Railway Station, Madrid, Spain")


# APLICACIÓN! -------------------------------------------------------------

ui <- fluidPage(
    titlePanel(
      img(height = 150, width = 450, src="E:/Martin/99.Personal/KSCHOOL/20. TFM/MyStopover/Images/Logo_MyStopOver.png"),
      tags$head(tags$link(rel = "icon", type = "image/png", href = "street-marker.png"), tags$title("My Stopover"))
  ),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText(h4("Choose your preferences
             to plan your trip")),
      
      selectInput("typeTrans", "Transportation:",
                  choices = c("Walking" = "caminando", "Car" = "coche")),
      
      checkboxGroupInput("typePlace", 
                         label = "Choose a type of plan",
                         choices = list("Points of Interest" = "Puntos emblematicos",
                                        "Monuments and Buildings" = "Monumentos y Edificios",
                                        "Museums" = "Museos",
                                        "Entertainment" = "Entretenimiento",
                                        "Cathedrals and Churchs" = "Iglesias",
                                        "Parks" = "Parques",
                                        "Shopping" = "Compras"
                                        ),
                         selected = c("Museos", "Puntos emblematicos", "Entretenimiento", "Compras", "Parques","Monumentos y Edificios","Iglesias")),
      
      sliderInput("ratingPlace", 
                  label = "Places Rating:",
                  min = 3.0, max = 5.0,value=c(3.0,5.0), step = 0.1),
      
      textInput("Start", label = "Start point", 
                value = "Hotel NH Collection Madrid Eurobuilding"),
      
      textInput("End", label = "End point", 
                value = "Estacion de Atocha"),
      
      timeInput("start_time", "Start time", value = Sys.time()+3600, seconds = F),

      timeInput("end_time", "End time", value = Sys.time()+36000-3600, seconds = F),
      
      sliderInput("ratingRest", 
                  label = "Restaurant Rating:",
                  min = 1.5, max = 5.0,value=c(1.5,5.0), step = 0.1),
      
      radioButtons("priceRest", label = "Restaurant Price Level",
                   choices = list("€" = 1, "€€" = 2,
                                  "€€€" = 3),selected = 2),
      
      selectInput("foodStyle", 
                         label = "Choose food style:",
                         choices = list("All Styles" = "Todos",
                                        "African" = "Africana", 
                                        "American" = "Americana",
                                        "Arabic" = "Arabe", 
                                        "Asiatic" = "Asiatica",
                                        "Fast Food" = "FastFood",
                                        "Spanish" = "Espanola",
                                        "European" = "Europea",
                                        "Indian" = "India",
                                        "Italian" = "Italiana",
                                        "Mediterranean" = "Mediterranea",
                                        "Mexican" ="Mexicana",
                                        "South American" = "Sudamericana",
                                        "Veggie"
                         ),
                         selected = "Todos"),
      
      submitButton(text = "Enviar")
    ),
    
    mainPanel(
      leafletOutput("mymap", height = "1080px")
      # textOutput("direc"),
      # textOutput("time_output")
    )
  )
)


# Server Logic ------------------------------------------------------------

server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    

    leaflet() %>%
      addTiles()  %>%
      addProviderTiles("NASAGIBS.ViirsEarthAtNight2012", group="NASA") %>%
      addProviderTiles("OpenStreetMap.BlackAndWhite", group = "Blanco y Negro") %>%
      setView(lng= -3.7037921, lat= 40.4166845 , zoom=14) %>%
      addLayersControl(
        baseGroups = c("NASA", "Blanco y Negro"),
        #overlayGroups = c("Tiendas Calle", "Tiendas Centro Comercial"),
        options = layersControlOptions(collapsed = F)
      )
  })



  observe({
    ### Datos a introducir por el usuario:
    
    # Horas de inicio y fin
    horaInicio <- as.numeric(strftime(input$start_time, "%T", format = "%H")) + as.numeric(strftime(input$start_time, "%T", format = "%M"))/60
    horaFinal <- as.numeric(strftime(input$end_time, "%T", format = "%H")) + as.numeric(strftime(input$end_time, "%T", format = "%M"))/60

    horaComida <- 14.5
    tiempoComer <- 0
    
    if (horaFinal < horaInicio) horaFinal <- horaFinal + 24
    if (horaInicio < horaComida & horaFinal > horaComida + 1) tiempoComer <- 1
    
    
    # Puntos de origen y fin
    if (origen != input$Start){
      ptoOrigen <- geoCode(paste(input$Start, "Madrid", sep = ", "))
      origen <- input$Start
    } 
    if (final != input$End){
      ptoFinal <- geoCode(paste(input$End, "Madrid", sep = ", "))
      final <- input$End
    }
    
    tipoTrans <- input$typeTrans
    
    # Filtro de puntuación y tipo de lugar
    puntuacionMinima <- input$ratingPlace
    
    tipoLugar <- input$typePlace
    
    puntuacionRestaurante <- input$ratingRest
    precioRestaurante <- input$priceRest
    estiloComida <- input$foodStyle
    
    # Horas de inicio y fin
    # horaInicio <- 9
    # horaFinal <- 18

    # tipoTrans <- "caminando"
    
    # Filtro de puntuación y tipo de lugar
    # puntuacionMinima <- c(4.0,5)
    # tipoLugar <- c("Museos", "Puntos emblemáticos")
    
    # puntuacionRestaurante <- c(4.5,5)
    # precioRestaurante <- 2
    # estiloComida <- "Española"
    
    cat("Constantes OK \n")
    ####---

    coord <- COORD
    tiempoCaminando <- TIEMPOCAMINANDO
    tiempoCoche <- TIEMPOCOCHE
    restFinal <- RESTAURANTES
    # MODIFICAR TIEMPOS NECESARIOS!
    

    # 1.0 ALGORITMO TSP!
    tiempoDisponible <- horaFinal - horaInicio - tiempoComer

    # Filtro de los lugares y matrices de tiempos
    lugaresFiltrados <- which(coord$rating >= puntuacionMinima[1] & coord$rating <= puntuacionMinima[2] & coord$Agrupacion %in% tipoLugar)

    coord <- coord[lugaresFiltrados, ]
    tiempoCaminando <- tiempoCaminando[lugaresFiltrados,lugaresFiltrados]
    tiempoCoche <- tiempoCoche[lugaresFiltrados,lugaresFiltrados]
    rm(lugaresFiltrados)

    # Cálculo de distancias del origen y final a la BBDD
    coord <- tiempo_origen_final(coord, as.numeric(ptoOrigen[1:2]))
    coord <- tiempo_origen_final(coord, as.numeric(ptoFinal[1:2]), "final")
    
    cat("Tiempos Origen Final OK \n")

    # Criterio de selección de Lugares
    coord$sumaTiempos <- coord$origen + coord$final
    
    coord$multiplicador <- (coord$sumaTiempos - min(coord$sumaTiempos)) / (max(coord$sumaTiempos) - min(coord$sumaTiempos)) + 0.5
    
    coord$ImpTiempo <- coord$rating*coord$opiniones^0.5/coord$multiplicador
    
    tiempoCaminando <- tiempoCaminando[order(coord$ImpTiempo, decreasing = T),order(coord$ImpTiempo, decreasing = T)]
    
    tiempoCoche <- tiempoCoche[order(coord$ImpTiempo, decreasing = T),order(coord$ImpTiempo, decreasing = T)]
    
    coord <- coord[order(coord$ImpTiempo, decreasing = T), ]
    row.names(coord) <- NULL
    
    # Calculamos la ruta optima
    if (tipoTrans == "caminando") aux <- tiempoCaminando
    if (tipoTrans == "coche") aux <- tiempoCoche
    
    # if (nrow(coord) > 0) {
    rutaTSP <- calcularRutaFin(coord, aux, tiempoDisponible, tipoTrans)
    rm(aux)
    cat("Ruta TSP OK \n")
    
    # Ponemos un orden a la ruta
    ordenRuta <- as.numeric(rutaTSP)-1
    if (ordenRuta[1] != 0) ordenRuta <- ordenRuta[c(which(ordenRuta == 0):length(ordenRuta),1:(which(ordenRuta == 0)-1))]

    # Nos quedamos con los destinos
    destinosFinales <- coord[ordenRuta[-c(1,which(length(ordenRuta)-1 == ordenRuta))], ]
    destinosFinales$id <- 1:nrow(destinosFinales)

    if (which(length(ordenRuta)-1 == ordenRuta) != 2) destinosFinales <- destinosFinales[order(destinosFinales$id, decreasing = T), ]

    # Creamos nuestro data frame FINAL

    puntosFin <- rbind(as.numeric(ptoFinal[1:2]), destinosFinales[1:2], as.numeric(ptoOrigen[1:2]))
    puntosFin$nombre <- c(ptoFinal[4], destinosFinales$nombre, ptoOrigen[4])
    puntosFin$tipo <- c("Fin", destinosFinales$tipo, "Inicio")
    puntosFin$rating <- c("--", destinosFinales$rating, "--")
    puntosFin$opiniones <- c("--", destinosFinales$opiniones, "--")
    puntosFin$tiempoNecesario <- c(0, destinosFinales$tiempoNecesario, 0)

    # Agregamos los tiempos a destino
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

    # Agregamos los tiempos acumulados
    puntosFin$tiempoAcuCaminando <- 0
    puntosFin$tiempoAcuCoche <- 0

    for (i in 2:nrow(puntosFin)){
      puntosFin$tiempoAcuCaminando[i] <- puntosFin$tiempoNecesario[i]*60 + puntosFin$tiempoCaminando[i] + puntosFin$tiempoAcuCaminando[i-1]
      puntosFin$tiempoAcuCoche[i] <- puntosFin$tiempoNecesario[i]*60 + puntosFin$tiempoCoche[i] + puntosFin$tiempoAcuCoche[i-1]
    }
    rm(i)

    # Agregamos lógica de restaurantes
    puntosFin$ComidaCami <- puntosFin$ComidaCoche <- F
    puntosFin$ComidaCami[which(puntosFin$tiempoAcuCaminando > (horaComida - horaInicio)*60)] <- T
    puntosFin$ComidaCoche[which(puntosFin$tiempoAcuCoche > (horaComida - horaInicio)*60)] <- T

    cat("Puntos Finales OK \n")
    
    # Filtramos los restaurantes que están de camino
    
    if (estiloComida != "Todos"){
      auxEstilo <- which(names(restFinal) == estiloComida)
      restFinal <- restFinal[restFinal[auxEstilo] == 1, ]
      rm(auxEstilo)
    }
    
    restFinal <- restFinal %>% filter(precio == precioRestaurante,
                                      rating >= puntuacionRestaurante[1],
                                      rating <= puntuacionRestaurante[2])
    
    
    if (tipoTrans == "caminando") {
      aux <- which(puntosFin$ComidaCami)[1]
    } else{
      aux <- which(puntosFin$ComidaCoche)[1]
    }
    
    if (!is.na(aux)) {
      cat("Entra en if length(aux) \n")
      
      if (nrow(restFinal) > 0){
        for (i in 1:nrow(restFinal)){
          if (aux == 1){
            restFinal$distLugCami[i] <- distHaversine(restFinal[i,2:3], puntosFin[aux,1:2]) + distHaversine(restFinal[i,2:3], puntosFin[aux,1:2])
          } else {
            restFinal$distLugCami[i] <- distHaversine(restFinal[i,2:3], puntosFin[aux,1:2]) + distHaversine(restFinal[i,2:3], puntosFin[aux-1,1:2])
          }
          restFinal$distLugCami[i] <- restFinal$distLugCami[i]/1000*795/60 #Minutos
        }
        rm(i)
        
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
        
        # write.csv2(restFinal[c(1:3,6,10,13:32)],"dimRest.csv")
        
        print(restFinal[1,2:3]) # Nos quedamos con el de mayor criterio
        poligono <- rbind(puntosFin[1:(aux-1),1:2], restFinal[1,2:3], puntosFin[aux:nrow(puntosFin),1:2])
        
        # write.csv2(poligono, "poligono.csv")
        # write.csv2(puntosFin, "puntosFin.csv")

        
        cat("Poligono OK \n")
        
        # Pintamos el mapa final
        mytext <- paste("Lugar: ", puntosFin$nombre[-c(1,nrow(puntosFin))], "<br/>", "Rating: ", puntosFin$rating[-c(1,nrow(puntosFin))], "<br/>",
                        "Tipo: ", puntosFin$tipo[-c(1,nrow(puntosFin))], "<br/>", "Opiniones: ", puntosFin$opiniones[-c(1,nrow(puntosFin))], "<br/>",
                        "Tiempo necesario: ", puntosFin$tiempoNecesario[-c(1,nrow(puntosFin))], " horas <br/>", "Tiempo caminando: ", puntosFin$tiempoCaminando[-c(1,nrow(puntosFin))], " min <br/>",
                        "Tiempo en coche:" , puntosFin$tiempoCoche[-c(1,nrow(puntosFin))], " min", sep="") %>%
          lapply(htmltools::HTML)
        mytextIni <- paste("Lugar: ", puntosFin$nombre[1], "<br/>",
                        "Tipo: ", puntosFin$tipo[1],
                        sep="") %>%
          lapply(htmltools::HTML)
        mytextFin <- paste("Lugar: ", puntosFin$nombre[nrow(puntosFin)], "<br/>", 
                           "Tipo: ", puntosFin$tipo[nrow(puntosFin)], "<br/>", 
                           "Tiempo caminando: ", puntosFin$tiempoCaminando[nrow(puntosFin)], " min <br/>",
                           "Tiempo en coche:" , puntosFin$tiempoCoche[nrow(puntosFin)], " min", sep="") %>%
          lapply(htmltools::HTML)
        mytext2 <- paste("Restaurante: ", restFinal$name, "<br/>", "Rating: ", restFinal$rating, "<br/>", sep="") %>%
          lapply(htmltools::HTML)
        mytext3 <- paste("Restaurante: ", restFinal$name[1], "<br/>", "Rating: ", restFinal$rating[1], "<br/>",
                         "Opiniones: ", restFinal$opiniones[1], sep="") %>%
          lapply(htmltools::HTML)
        
        cat("Etiquetas OK \n")
        
        
        leafletProxy("mymap") %>%
          # setView(lng= puntosFin[1,2], lat= puntosFin[1,1] , zoom=13) %>%
          clearMarkers() %>%
          removeShape("Poligono") %>% 
          addMarkers(puntosFin[-c(1,nrow(puntosFin)),2], puntosFin[-c(1,nrow(puntosFin)),1],
                           labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
                           label = mytext, group = "Lugares", 
                           icon = myPoiIcon
          ) %>%
          addMarkers(puntosFin[1,2], puntosFin[1,1],
                     labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
                     label = mytextIni, group = "Lugares", 
                     icon = myStartIcon
          ) %>%
          addMarkers(puntosFin[nrow(puntosFin),2], puntosFin[nrow(puntosFin),1],
                     labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
                     label = mytextFin, group = "Lugares", 
                     icon = myEndIcon
          ) %>%
          addMarkers(restFinal[1,3], restFinal[1,2],
                           labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
                           label = mytext3, group = "Rest",
                           icon = myRestIcon
          ) %>%
          addCircleMarkers(restFinal$lon, restFinal$lat,
                           fillOpacity = 0.7, color="green", radius=3, stroke=FALSE,
                           labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
                           label = mytext2, group = "Restaurantes"
          ) %>%
          addPolylines(poligono[,2], poligono[,1], layerId = "Poligono", color = "red")
      
        } else{
        poligono <- rbind(puntosFin[1:2])
        
        cat("Poligono OK \n")
        
        # Pintamos el mapa final
        mytext <- paste("Lugar: ", puntosFin$nombre[-c(1,nrow(puntosFin))], "<br/>", "Rating: ", puntosFin$rating[-c(1,nrow(puntosFin))], "<br/>",
                        "Tipo: ", puntosFin$tipo[-c(1,nrow(puntosFin))], "<br/>", "Opiniones: ", puntosFin$opiniones[-c(1,nrow(puntosFin))], "<br/>",
                        "Tiempo necesario: ", puntosFin$tiempoNecesario[-c(1,nrow(puntosFin))], " horas <br/>", "Tiempo caminando: ", puntosFin$tiempoCaminando[-c(1,nrow(puntosFin))], " min <br/>",
                        "Tiempo en coche:" , puntosFin$tiempoCoche[-c(1,nrow(puntosFin))], " min", sep="") %>%
          lapply(htmltools::HTML)
        mytextIni <- paste("Lugar: ", puntosFin$nombre[1], "<br/>",
                           "Tipo: ", puntosFin$tipo[1],
                           sep="") %>%
          lapply(htmltools::HTML)
        mytextFin <- paste("Lugar: ", puntosFin$nombre[nrow(puntosFin)], "<br/>", 
                           "Tipo: ", puntosFin$tipo[nrow(puntosFin)], "<br/>", 
                           "Tiempo caminando: ", puntosFin$tiempoCaminando[nrow(puntosFin)], " min <br/>",
                           "Tiempo en coche:" , puntosFin$tiempoCoche[nrow(puntosFin)], " min", sep="") %>%
          lapply(htmltools::HTML)
        
        cat("Etiquetas OK \n")
        
        leafletProxy("mymap") %>%
          # setView(lng= puntosFin[1,2], lat= puntosFin[1,1] , zoom=13) %>%
          clearMarkers() %>%
          removeShape("Poligono") %>% 
          addMarkers(puntosFin[-c(1,nrow(puntosFin)),2], puntosFin[-c(1,nrow(puntosFin)),1],
                     labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
                     label = mytext, group = "Lugares", 
                     icon = myPoiIcon
          ) %>%
          addMarkers(puntosFin[1,2], puntosFin[1,1],
                     labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
                     label = mytextIni, group = "Lugares", 
                     icon = myStartIcon
          ) %>%
          addMarkers(puntosFin[nrow(puntosFin),2], puntosFin[nrow(puntosFin),1],
                     labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
                     label = mytextFin, group = "Lugares", 
                     icon = myEndIcon
          ) %>% 
          addPolylines(poligono[,2], poligono[,1], layerId = "Poligono", color = "red")
      }
    } else {
      poligono <- rbind(puntosFin[1:2])
      
      cat("Poligono OK \n")
      
      # Pintamos el mapa final
      mytext <- paste("Lugar: ", puntosFin$nombre[-c(1,nrow(puntosFin))], "<br/>", "Rating: ", puntosFin$rating[-c(1,nrow(puntosFin))], "<br/>",
                      "Tipo: ", puntosFin$tipo[-c(1,nrow(puntosFin))], "<br/>", "Opiniones: ", puntosFin$opiniones[-c(1,nrow(puntosFin))], "<br/>",
                      "Tiempo necesario: ", puntosFin$tiempoNecesario[-c(1,nrow(puntosFin))], " horas <br/>", "Tiempo caminando: ", puntosFin$tiempoCaminando[-c(1,nrow(puntosFin))], " min <br/>",
                      "Tiempo en coche:" , puntosFin$tiempoCoche[-c(1,nrow(puntosFin))], " min", sep="") %>%
        lapply(htmltools::HTML)
      mytextIni <- paste("Lugar: ", puntosFin$nombre[1], "<br/>",
                         "Tipo: ", puntosFin$tipo[1],
                         sep="") %>%
        lapply(htmltools::HTML)
      mytextFin <- paste("Lugar: ", puntosFin$nombre[nrow(puntosFin)], "<br/>", 
                         "Tipo: ", puntosFin$tipo[nrow(puntosFin)], "<br/>", 
                         "Tiempo caminando: ", puntosFin$tiempoCaminando[nrow(puntosFin)], " min <br/>",
                         "Tiempo en coche:" , puntosFin$tiempoCoche[nrow(puntosFin)], " min", sep="") %>%
        lapply(htmltools::HTML)
      
      cat("Etiquetas OK \n")
      
      leafletProxy("mymap") %>%
        # setView(lng= puntosFin[1,2], lat= puntosFin[1,1] , zoom=13) %>%
        clearMarkers() %>%
        removeShape("Poligono") %>% 
        addMarkers(puntosFin[-c(1,nrow(puntosFin)),2], puntosFin[-c(1,nrow(puntosFin)),1],
                   # fillOpacity = 0.7, color="red", radius=10, stroke=FALSE,
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
                   label = mytext, group = "Lugares", 
                   icon = myPoiIcon
        ) %>%
        addMarkers(puntosFin[-c(1,nrow(puntosFin)),2], puntosFin[-c(1,nrow(puntosFin)),1],
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
                   label = mytext, group = "Lugares", 
                   icon = myPoiIcon
        ) %>%
        addMarkers(puntosFin[1,2], puntosFin[1,1],
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
                   label = mytextIni, group = "Lugares", 
                   icon = myStartIcon
        ) %>%
        addMarkers(puntosFin[nrow(puntosFin),2], puntosFin[nrow(puntosFin),1],
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto"),
                   label = mytextFin, group = "Lugares", 
                   icon = myEndIcon
        ) %>% 
        addPolylines(poligono[,2], poligono[,1], layerId = "Poligono", color = "red")
    }
  })


  # output$direc <- renderText({
  #   if (input$Start != ""){
  #     ubica <- geoCode(input$Start)
  #     paste("Tus coordenadas son Latitud:", ubica[1], "Longitud:", ubica[2])
  #   } else {
  #     "Introduce tu ubicacion de partida..."
  #   }
  # })
  # output$time_output <- renderText({
  #   paste("Hora de inicio:", strftime(input$start_time, "%T", format = "%H:%M"))
  # })
}



# Run app -------------------------------------------------------------------
shinyApp(ui, server)





