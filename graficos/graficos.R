library(tidyverse)
library(readxl)
library(scales)

library(ggplot2)
uniones <- readRDS("data/procesada_uniones.rds")

uniones_DN <- uniones[uniones$provincia=="Distrito Nacional",]
colnames(uniones)
ggplot(uniones_DN, aes(x = periodo, y = ratio_divorcio_matrimonio, group= provincia)) +
  geom_line() +
  labs(title = "Total de Divorcios/ Total de Matrimonios - Distrito Nacional R.D", x = "Año", y = "Divorcios/Matrimonios")  +
  scale_y_continuous(labels = percent_format())  +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") + 
  theme_minimal() 


total_pais <- uniones %>% 
  select(periodo, matrimonios, divorcios) %>% 
  group_by(periodo) %>% 
  summarise(total_matrimonios= sum(matrimonios), total_divorcios= sum(divorcios)) %>% 
  mutate(divorcios_matrimonios= total_divorcios/total_matrimonios , provincia= "total pais")


# Convierte las cantidades a formato con comas
total_pais$total_matrimonios <- format(total_pais$total_matrimonios, big.mark = ",", scientific = FALSE)
total_pais$total_divorcios <- format(total_pais$total_divorcios, big.mark = ",", scientific = FALSE)

total_pais$periodo <- as.factor(total_pais$periodo)
# Crea el gráfico utilizando ggplot2
ggplot(total_pais, aes(x = as.factor(periodo))) +
  geom_line(aes(y = as.numeric(total_matrimonios), color = "Matrimonios"), size = 1.5) +
  geom_line(aes(y = as.numeric(total_divorcios), color = "Divorcios"), size = 1.5) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  labs(title = "Évolution des Mariages et des Divorces en République Dominicaine",
       x = "Année",
       y = "Quantité",
       color = "Type d'Événement") +
  theme_minimal()













ggplot(total_pais, aes(x = periodo, y = total_matrimonios, group= provincia)) +
  geom_line() +
  labs(title = "Total de Divorcios/ Total de Matrimonios - Distrito Nacional R.D", x = "Año", y = "Divorcios/Matrimonios")  +
  theme_minimal() 


ggplot(total_pais, aes(x = periodo, y = divorcios_matrimonios, group= provincia)) +
  geom_line() +
  labs(title = "Total de Divorcios/ Total de Matrimonios - República Dominicana", x = "Año", y = "Divorcios/Matrimonios")  +
  scale_y_continuous(labels = percent_format())  +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") + 
  theme_minimal() 





ubicacion_provincia <- which(edad_del_conyugue == "Provincia Baoruco", arr.ind = TRUE)
ubicacion_fila <- ubicacion_provincia[1]
provincia_sliced <- edad_del_conyugue[ubicacion_provincia[1]:nrow(edad_del_conyugue), ]

ubicacion_inicio_sliced <- which(provincia_sliced == "Edad del contrayente", arr.ind = TRUE)
ubicacion_termino_sliced <- which(provincia_sliced == "Total", arr.ind = TRUE)

provincia_sliced <- provincia_sliced[ubicacion_inicio_sliced[[1]]:ubicacion_termino_sliced[[1]],]
provincia_sliced[2, 2] <- "Edad"
colnames(provincia_sliced) <- provincia_sliced[2, ]
provincia_sliced <- provincia_sliced[3:nrow(provincia_sliced), 2:ncol(provincia_sliced)]
provincia_sliced <- provincia_sliced %>% gather("periodo", "cantidad", 2:ncol(provincia_sliced))
provincia_sliced$provincia <- "Provincia Baoruco"