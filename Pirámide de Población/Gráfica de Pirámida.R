# Configuracion
rm(list = ls())
options(scipen = 999)

# Librerias
library(tidyverse)
library(tidyr)
library(ggthemes)
library(extrafont)

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


######################################################################################################

# Descarga de los datos.
library(curl)
library(readxl)
curl::curl_download(url = "https://www.inegi.org.mx/contenidos/programas/intercensal/2015/tabulados/01_poblacion_chis.xls", destfile = "Poblacion/chis_pop.xls")
curl::curl_download(url = "https://www.inegi.org.mx/contenidos/programas/intercensal/2015/tabulados/01_poblacion_tab.xls", destfile = "Poblacion/tab_pop.xls")
# Leemos datos
# Por tamaño de localidad
chis <- read_xls("Poblacion/chis_pop.xls", sheet = 2, skip = 6)
tab <- read_xls("Poblacion/tab_pop.xls", sheet = 2, skip = 6)

# Librerias
library(tidyverse)
library(tidyr)
# Procesamos datos
pop <- chis %>%
  # Filtramos los renglones vacios, 
  # los renglones donde el Tamaño de localidad` es el Total y 
  # los `Grupos quinquenales de edad` son todos "Total"    
  filter(!is.na(Estimador) &
           Estimador == "Valor" &
           `Tamaño de localidad` == "Total" &
           `Grupos quinquenales de edad` != "Total") %>%
  mutate(totH = sum(Hombres), 
         totM = sum(Mujeres)) %>% 
  mutate(Hombres = (Hombres/totH)*100, 
         Mujeres = (Mujeres/totM)*100
         ) %>% 
  select(-totH, -totM) %>% 
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>% 
  # Nos quedamos con columnas utiles  
  select(`Entidad federativa`, `Grupos quinquenales de edad`, 
         `Población total`, Sexo, `Poblacion por Sexo`) %>% 
  filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))


# Grafica
ggplot(pop, aes(x = `Grupos quinquenales de edad`,
                y = `Poblacion por Sexo`,
                fill = Sexo)) +
  geom_bar(data = subset(pop, Sexo == "Hombres") %>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
           stat = "identity", width = 0.5, fill = "blue") +
  geom_bar(data = subset(pop, Sexo == "Mujeres"),
           stat = "identity", width = 0.5, fill = "pink") + 
  coord_flip() + 
  ggthemes::theme_tufte() + 
  theme(plot.title = element_text(family = "Arial", hjust = 0.5, size = 20), 
        axis.text.x = element_text(family = "Arial"), 
        axis.text.y = element_text(family = "Arial")
        ) + 
  labs(title = "Pirámide Poblacional de Chiapas, 2015", 
       x = "", 
       y = "Hombres                        Mujeres", 
       caption = "Fuente: INEGI. Encuesta intercensal 2015. Tabulados de Población \nSe omiten personas de 75 años y más, por venir aglomeradas en un mismo grupo de edad."
       ) + 
  scale_y_continuous(breaks = seq(-12, 12, by = 2), labels = paste0(seq(-12, 12, by = 2), "%"))
  

ggsave(filename= "Poblacion/ppChiapas.png", dpi = 300)


pop <- tab %>%
  # Filtramos los renglones vacios, 
  # los renglones donde el Tamaño de localidad` es el Total y 
  # los `Grupos quinquenales de edad` son todos "Total"    
  filter(!is.na(Estimador) &
           Estimador == "Valor" &
           `Tamaño de localidad` == "Total" &
           `Grupos quinquenales de edad` != "Total") %>%
  mutate(totH = sum(Hombres), 
         totM = sum(Mujeres)) %>% 
  mutate(Hombres = (Hombres/totH)*100, 
         Mujeres = (Mujeres/totM)*100
  ) %>% 
  select(-totH, -totM) %>% 
  # Hacemos pivot longer rotando las columnas hombres y mujeres    
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo") %>% 
  # Nos quedamos con columnas utiles  
  select(`Entidad federativa`, `Grupos quinquenales de edad`, 
         `Población total`, Sexo, `Poblacion por Sexo`) %>% 
  filter(!(`Grupos quinquenales de edad` %in% c("75 años y más","No especificado")))


# Grafica
ggplot(pop, aes(x = `Grupos quinquenales de edad`,
                y = `Poblacion por Sexo`,
                fill = Sexo)) +
  geom_bar(data = subset(pop, Sexo == "Hombres") %>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
           stat = "identity", width = 0.5, fill = "blue") +
  geom_bar(data = subset(pop, Sexo == "Mujeres"),
           stat = "identity", width = 0.5, fill = "pink") + 
  coord_flip() + 
  ggthemes::theme_tufte() + 
  theme(plot.title = element_text(family = "Arial", hjust = 0.5, size = 20), 
        axis.text.x = element_text(family = "Arial"), 
        axis.text.y = element_text(family = "Arial")
  ) + 
  labs(title = "Pirámide Poblacional de Tabasco, 2015", 
       x = "", 
       y = "Hombres                        Mujeres", 
       caption = "Fuente: INEGI. Encuesta intercensal 2015. Tabulados de Población \nSe omiten personas de 75 años y más, por venir aglomeradas en un mismo grupo de edad."
  ) + 
  scale_y_continuous(breaks = seq(-12, 12, by = 2), labels = paste0(seq(-12, 12, by = 2), "%"))

ggsave(filename= "Poblacion/ppTabasco.png", dpi = 300)

# Ejemplo práctico para checar!
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# https://klein.uk/teaching/viz/datavis-pyramids/
# https://stackoverflow.com/questions/38268741/geom-bar-ggplot2-stacked-grouped-bar-plot-with-positive-and-negative-values-p

