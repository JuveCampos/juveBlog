# Librerias ----
library(shiny)
library(sf)
library(leaflet)
library(tidyverse)
library(lwgeom)

# Bases de datos ----
f <- st_read("Mexico_and_US_Border.shp")

mapaLeaflet <-leaflet(options = leafletOptions(zoomControl = TRUE, 
                                               minZoom = 5)) %>% 
  setMaxBounds(lng1 = -86.72, 
               lat1 = 14.52, 
               lng2 = -117.13, 
               lat2 = 32.53) %>% 
  addTiles() %>% 
  addPolylines(data = f, 
               opacity = 1, 
               weight = 3, 
               dashArray = c(8,10)) 

# Funciones globales ---- 
# Le copipegamos, tal cual, lo que hicimos arriba

# Función para calcular las distancias. 
distancia <- function(X, Y){
  pto <- data.frame(x = X, y = Y) %>% 
    st_as_sf(coords = c("x", "y")) 
  st_crs(pto) <- st_crs(f)
  dist <- as.numeric(st_distance(pto, f)) / 1000       
}

dibuja_lineas_minima_distancia <- function(X,Y){
  
  # Creamos el punto a partir de los argumentos X y Y
  pto <<- data.frame(x = X, y = Y) %>% 
    st_as_sf(coords = c("x", "y")) 
  
  # Homologamos el Sistema de Coordenadas de Referencia
  st_crs(pto) <- st_crs(f)
  
  # Extraemos los vértices de la linea
  ptos_linea <- st_coordinates(f) %>%
    as.data.frame() %>%
    st_as_sf(coords = c("X", "Y")) 
  
  # Homologamos el Sistema de Coordenadas de Referencia
  st_crs(ptos_linea) <- st_crs(f)
  
  # Sacamos las distancias del punto a todos los vertices de la frontera
  distancias <- st_distance(ptos_linea, pto)
  
  # Obtencion de la distancia minima
  distancia_minima <- min(distancias) 
  
  # Guardamos el punto de la frontera con la distancia minima
  punto_frontera <<- 
    ptos_linea[distancias == distancia_minima,]
  
  # Construimos la linea de distancia minima
  linea <- st_linestring(matrix(c(pto[,"geometry"] %>% 
                                    st_coordinates(), 
                                  punto_frontera[,"geometry"] %>%       st_coordinates()), ncol = 2, byrow = TRUE))  
  
  # Seleccionamos la linea como objeto a retornar de la funcion 
  return(linea)
  
}

# Hacemos una interfaz de usuario sencilla: 
ui <- shinyUI({
fluidPage(
  titlePanel("Calculo de la distancia a la Frontera Norte de México"), 
  sidebarLayout(
    sidebarPanel(
      h3("Coordenadas del punto"), 
      HTML("<p style = 'color:gray;'>Seleccione dentro del mapa un punto para realizar el calculo de la distancia en <b>linea recta</b> de dicho punto a la frontera norte de México.</p>"),
      HTML("<h6 style = 'color:red;'>Instrucciones</h6>"),
      HTML("<p style = 'color:gray;'>Seleccione un punto dentro del mapa dando <i>click</i> en el punto seleccionado. Para navegar, haga uso del control de zoom ubicado en la esquina superior izquierda del mapa.</p>"),
      br(),
      numericInput(inputId = "txtLat", label = "Latitud: ", value = 19.303741), 
      numericInput(inputId = "txtLon", label = "Longitud: ", value = -99.20775),
      br(),
      wellPanel(
      numericInput(inputId = "numDistancia", "Distancia (km)", value = 746.6956)
      )
    ), 
    mainPanel(
      fluidPage(
        fluidRow(
          column(12,
            leafletOutput("mapa", height = "600px")
          )
        )
      )
    )
  )
)
})

# Programamos el mapa 
server <- function(input, output, session) ({

output$mapa <- renderLeaflet({
  mapaLeaflet
    
    # Generamos elementos
    ln <- dibuja_lineas_minima_distancia(X = input$txtLon, Y = input$txtLat)
    
    # Hacemos el mapa
    mapaLeaflet %>% 
      addPolylines(data = ln, 
                   color = 'green',
                   opacity = 1) %>% 
      addCircleMarkers(data = punto_frontera, 
                       color = 'green', 
                       opacity = 0.8,
                       radius = 6) %>% 
      addCircleMarkers(data = pto, 
                       color = 'green', 
                       opacity = 0.8,
                       radius = 6)
})


# Captamos donde da el click el usuario
pol_of_click <- reactiveValues(click = NULL)

observe({
  if(is.null(input$mapa_click)){
    # print("Esta madre esta nula")
  } else {
    # print("Esta madre no esta taaan nula")
    observeEvent(input$mapa_click,
                 {
                   pol_of_click <- input$mapa_click
                   updateNumericInput(session, "txtLat", value = pol_of_click$lat)
                   updateNumericInput(session, "txtLon", value = pol_of_click$lng)
                   # Función para calcular las distancias. 
                   dist <- reactive({
                     longitud <- distancia(X = pol_of_click$lng, Y = pol_of_click$lat) 
                   })
                   updateNumericInput(session, "numDistancia", value = formatC(dist(), 5))
                 })
  }
})

})

shinyApp(ui, server)
