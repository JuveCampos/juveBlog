# Librerias
library(sf)
library(leaflet)
library(tidyverse)

# Descarga datos
curl::curl_download("https://opendata.arcgis.com/datasets/e735940321bd4383bab528a91bf526f8_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D",
                    destfile = "frontera.zip")

# Deszipeado
unzip(zipfile = "frontera.zip")

# Abrido
f <- st_read("Mexico_and_US_Border.shp")

# Ploteado
plot(f, max.plot = 1)

# # Descomponemos la linea en sus coordenadas
# ptos_linea <- st_coordinates(f) %>% 
#   as.data.frame() %>% 
#   st_as_sf(coords = c("X", "Y"))  
# 
# st_crs(ptos_linea) <- 4326

# Determinamos un punto ()
pto <- data.frame(x = -116.615094, y = 31.848142) %>% 
  st_as_sf(coords = c("x", "y")) 


st_crs(pto) <- 4326

# Calculamos la distancia
st_distance(pto, f)

# Calculamos la distancia entre puntos: 
ptos_linea$distancias <- st_distance(ptos_linea, pto)

# Sacamos la distancia minima
min(ptos_linea$distancias)

# Sacamos el punto 
punto_frontera <- ptos_linea[ptos_linea$distancias == min(ptos_linea$distancias),]

# linea de distancia
linea <- st_linestring(matrix(c(pto[,"geometry"] %>% 
                                 st_coordinates(), 
                               punto_frontera[,"geometry"] %>% 
                                 st_coordinates()), 
                             ncol = 2, byrow = TRUE))

# Mapeamos chido 
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = pto) %>% 
  addCircleMarkers(data = punto_frontera) %>% 
  addPolylines(data = linea) %>% 
  addPolylines(data = f, color = "red")



