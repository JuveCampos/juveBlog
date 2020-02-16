# Originalmente del Post de Randhyll Cho
# https://rpubs.com/RandhyllCho/250112

# Libreria
library(ggplot2)

# Funciones propias
x <-  function(t) 16 * sin(t)^3
y <- function(t) 13*cos(t) - 5*cos(2*t) - 2*cos(3*t) - cos(4*t)

# Generamos una base de datos
dat <- data.frame(t = seq(0, 2*pi, by = 0.01))

# Coordenadas a partir de las funciones propias
dat$y <- y(dat$t)
dat$x <- x(dat$t)

# Ploteo del corazón
heart <- ggplot(dat, aes(x,y)) +
  geom_polygon(fill = "red", 
               col = "firebrick", 
               alpha = 0.9, 
               size = 4) +
  theme_minimal() 

heart
