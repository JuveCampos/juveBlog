# Librerias
library(tidyverse)
library(tidyr)

# Leemos datos

# Por tamaño de localidad
bd <- readxl::read_xls("D:/Documents/GitHub/indicadoresMunicipales/01_Datos/Intercensal 2015/Poblacion/chis_pop.xls", sheet = 2, skip = 6)

# # Por municipio
# bd <- readxl::read_xls("D:/Documents/GitHub/indicadoresMunicipales/01_Datos/Intercensal 2015/Poblacion/chis_pop.xls", sheet = 3)

# Procesamos datos
(pop <- bd %>%
  filter(!is.na(Estimador) &
           Estimador == "Valor" &
           `Tamaño de localidad` == "Total" &
           `Grupos quinquenales de edad` != "Total") %>%
  pivot_longer(cols = c("Hombres", "Mujeres"),
               names_to = "Sexo",
               values_to = "Poblacion por Sexo"))

### Grafica ----
ggplot(pop, aes(x = `Grupos quinquenales de edad`,
                y = `Poblacion por Sexo`,
                fill = Sexo)) +
  geom_bar(data = subset(pop, Sexo == "Hombres") %>% mutate(`Poblacion por Sexo` = -`Poblacion por Sexo`),
          stat = "identity", width = 0.6) +
  geom_bar(data = subset(pop, Sexo == "Mujeres"),
          stat = "identity", width = 0.6) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-600000, 600000, by = 100000),
                     labels = c(rev(seq(0, 600000, by = 100000)),
                                    seq(100000, 600000, by = 100000))) +
  theme_tufte() +  # Tufte theme from ggfortify
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank()) +   # Centre plot title
  scale_fill_brewer(palette = "Dark2")

# Ejemplo práctico para checar!
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# https://klein.uk/teaching/viz/datavis-pyramids/
# https://stackoverflow.com/questions/38268741/geom-bar-ggplot2-stacked-grouped-bar-plot-with-positive-and-negative-values-p

