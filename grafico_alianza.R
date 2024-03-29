library(tidyverse)
library(readxl)
library(scales)

library(ggplot2)
uniones <- readRDS("data/procesada_uniones.rds")



total_pais <- uniones %>% 
  select(periodo, matrimonios, divorcios) %>% 
  group_by(periodo) %>% 
  summarise(total_matrimonios= sum(matrimonios), total_divorcios= sum(divorcios)) %>% 
  mutate(divorcios_matrimonios= total_divorcios/total_matrimonios , provincia= "total pais")



