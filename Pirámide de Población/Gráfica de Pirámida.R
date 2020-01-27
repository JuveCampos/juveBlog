# Configuracion
rm(list = ls())
options(scipen = 999)

# Librerias
library(tidyverse)
library(tidyr)
library(ggthemes)

# Leemos datos
# Por tamaño de localidad
bd <- readxl::read_xls("mor_pop.xls", sheet = 2, skip = 6)


# Procesamos datos
(pop <- bd %>%
  filter(!is.na(Estimador) &
           Estimador == "Valor" &
           `Tamaño de localidad` == "Total" &
           `Grupos quinquenales de edad` != "Total") %>%
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo"))

max(pop$`Población total`)/5



tema <- theme(text = element_text(color = "#3A3F4A"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(linetype = "dotted", size = 0.3, color = "#3A3F4A"),
      axis.title = element_blank(),
      plot.title = element_text(face = "bold", size = 36, margin = margin(b = 10), hjust = 0.030),
      plot.subtitle = element_text(size = 16, margin = margin(b = 20), hjust = 0.030),
      plot.caption = element_text(size = 14, margin = margin(b = 10, t = 50), color = "#5D646F"),
      axis.text.x = element_text(size = 12, color = "#5D646F"),
      axis.text.y = element_blank(),
      strip.text = element_text(color = "#5D646F", size = 18, face = "bold", hjust = 0.030),
      plot.background = element_rect(fill = "#EFF2F4"),
      plot.margin = unit(c(2, 2, 2, 2), "cm"),
      legend.position = "top",
      legend.spacing = unit(0.1, "lines"),
      legend.text  = element_text(size = 14),
      legend.text.align = 0)


### Grafica ----
ggplot(pop, aes(x = `Grupos quinquenales de edad`,
                y = `Poblacion por Sexo`,
                fill = Sexo)) +
  geom_linerange(data = subset(pop, Sexo == "Hombres") %>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
           aes(ymin = -10000, ymax = -10000 +`Poblacion por Sexo`),
           stat = "identity", size = 3.5) +
  geom_linerange(data = subset(pop, Sexo == "Mujeres"),
           aes(ymin = 10000, ymax = 10000+`Poblacion por Sexo`),
           stat = "identity", size = 3.5) +
  coord_flip() +
  scale_y_continuous(#breaks = seq(-180000, 180000, by = 30000)
                     breaks = c(seq(-180000, 0, by = 30000) - 10000, 
                                seq(0, 180000, by = 30000) + 10000
                                ),
                     labels = c(rev(seq(0, 180000, by = 30000)),
                                    seq(0, 180000, by = 30000))) +
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_brewer(palette = "Dark2") + 
  geom_label(aes(x = `Grupos quinquenales de edad`, y = 0, label = `Grupos quinquenales de edad`), 
             inherit.aes = F,
             size = 3.5, label.padding = unit(0.0, "lines"), label.size = 0,
             label.r = unit(0.0, "lines"), fill = "#EFF2F4", alpha = 0.9, color = "#5D646F") + 
  tema



### Grafica ----
ggplot(pop, aes(x = `Grupos quinquenales de edad`,
                y = `Poblacion por Sexo`,
                fill = Sexo)) +
  geom_linerange(data = subset(pop, Sexo == "Hombres") %>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
                 aes(ymin = -10000, ymax = -10000 +`Poblacion por Sexo`),
                 stat = "identity", size = 5, color = "blue") +
  geom_linerange(data = subset(pop, Sexo == "Mujeres"),
                 aes(ymin = 10000, ymax = 10000+`Poblacion por Sexo`),
                 stat = "identity", size = 5, color = "pink") +
  coord_flip()  +
   scale_y_continuous(#breaks = seq(-180000, 180000, by = 30000)
     breaks = c(seq(-180000, 0, by = 30000) - 10000,
                seq(0, 180000, by = 30000) + 10000
     ),
     labels = c(rev(seq(0, 180000, by = 30000)),
               seq(0, 180000, by = 30000))) +
   #theme_tufte()  +  # Tufte theme from ggfortify
   theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank(),
        axis.text.y = element_blank()
        ) +   # Centre plot title
   scale_fill_brewer(palette = "Dark2") +
   geom_label(aes(x = `Grupos quinquenales de edad`, y = 0, label = `Grupos quinquenales de edad`),
             inherit.aes = F, family = "Arial",
             size = 3.5, label.padding = unit(0.0, "lines"), label.size = 0,
             label.r = unit(0.0, "lines"), fill = "#FFFFFF", alpha = 0.9, color = "#5D646F")
#+
#   tema




# Ejemplo práctico para checar!
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# https://klein.uk/teaching/viz/datavis-pyramids/
# https://stackoverflow.com/questions/38268741/geom-bar-ggplot2-stacked-grouped-bar-plot-with-positive-and-negative-values-p

