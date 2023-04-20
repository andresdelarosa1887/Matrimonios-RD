library(tidyverse)
library(readxl)
library(scales)

library(ggplot2)

uniones_DN <- uniones[uniones$provincia=="Distrito Nacional",]

ggplot(uniones_DN, aes(x = periodo, y = divorcios_matrimonios, group= provincia)) +
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