# Instalamos tabulizer 
# install.packages("tabulizer") # Para instalar tabulizer en mi compu

# Librerias ---- 
library(tabulizer) # Para leer tablas en pdf
library(tidyverse) # Para manipular datos

# Obtenemos la url del Plan del INFONAVIT
url <- "https://portalmx.infonavit.org.mx/wps/wcm/connect/67e528e7-f13d-4dbf-a668-b29a594351c3/Plan_Estrategico_y_Financiero_2020-2024.pdf?MOD=AJPERES&CVID=mYkHiU3"
# Extraemos la tabla de la página 116
tab <- extract_tables(url, pages = 116)

# Generamos la tabla: 
matriz <- tab[[1]] %>% # Nos quedamos con la tabla
  as.tibble() %>% # Convertimos a tibble
  filter(V1 != "") %>% # Filtramos los renglones en blanco
  slice(-1) %>% # Quitamos el renglón de los nombres
  separate(V6, 
           into = c("V6", "V7", "V8", "V9"), 
           sep = "\\s+") # La ultima columna, con los datos pegados, la separamos en cuatro

# Le metemos los nombres personalizados (que signifiquen algo para nosotros)
names(matriz) <- c("Entidad", 
                   "Nueva", 
                   "Existente", 
                   "No Hipotecarios", 
                   "Total", 
                   "Derrama Infonavit", 
                   "Derrama Entidades Financieras", 
                   "Derrama No-hipotecarios", 
                   "Derrama Total")

# Convertimos las columnas a numero
matriz[,2:9] <- lapply(matriz[,2:9], function(x){
  x %>% 
    str_remove_all(pattern = ",") %>% # Le quitamos las comas
    as.numeric() # Lo convertimos a numero
})

# Armamos las grafica de porcentaje de créditos por entidad: 

# Generamos la base de datos a graficar
bd_plot <- matriz %>% 
  select(Entidad,Nueva) %>% 
  filter(Entidad != "Nacional") %>% 
  mutate(Pctje = 100*Nueva/sum(Nueva)) %>% 
  arrange(-Pctje) 

# Hacemos la gráfica
bd_plot %>% 
  ggplot(aes(x = reorder(Entidad, Pctje), y = Pctje)) + 
  geom_col(fill = "orange") + 
  coord_flip() + 
  geom_label(aes(label = paste0(round(Pctje,1), "%")), 
             hjust = -0.05) + 
  scale_y_continuous(expand = expansion(c(0,0.3), 0)) + 
  labs(y = "", x = "", 
       title = "Porcentaje del total de Créditos para adquisición de vivienda nueva\npor Entidad Federativa, 2021", 
       caption = "Fuente: Plan Estratégico y Financiero 2021-2025. INFONAVIT. ") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5), 
        panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.text.x = element_blank())
