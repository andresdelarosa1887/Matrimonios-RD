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

