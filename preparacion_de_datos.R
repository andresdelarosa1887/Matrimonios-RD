library(tidyverse)
library(readxl)
library(scales)

##Datos por provincia y por año
matrimonios_2001_2022 <- suppressMessages(suppressWarnings(read_excel("data/matrimonios/matrimonios_2001_2022.xlsx")))
divorcios_2001_2022 <- suppressMessages(suppressWarnings(read_excel("data/divorcios/divorcios_2001_2022.xlsx")))

##Funcion transformacion de datos
limpiame_ONE <- function(datos_sucios_one, nombre_variable= "x") {
  datos_sucios_one <- datos_sucios_one[11:43, 2:ncol(datos_sucios_one)]
  print(nombre_variable)
  datos_sucios_one[1,1] <- "provincia"
  colnames(datos_sucios_one) <- datos_sucios_one[1,]
  datos_sucios_one <- datos_sucios_one[2:nrow(datos_sucios_one),]
  datos_sucios_one <- datos_sucios_one %>% gather("periodo","variable", 2:ncol(datos_sucios_one)) 
  datos_sucios_one$variable <- as.numeric(datos_sucios_one$variable)
  colnames(datos_sucios_one)[3] <- nombre_variable
  return(datos_sucios_one)
}

matrimonios_2001_2022 <- limpiame_ONE(matrimonios_2001_2022, "matrimonios")
divorcios_2001_2022 <- limpiame_ONE(divorcios_2001_2022, "divorcios")
uniones <- left_join(matrimonios_2001_2022, divorcios_2001_2022, by= c("provincia", "periodo"))
uniones$divorcios_matrimonios= uniones$divorcios/uniones$matrimonios

write_rds(uniones, "data/procesada_uniones.rds")



##Datos por provincia, por edad y por año
edad_del_conyugue <- suppressMessages(suppressWarnings(read_excel("data/matrimonios/edades/matrimonios_edad_masculino_2001_2022.xlsx")))
edad_de_la_conyugue <- suppressMessages(suppressWarnings(read_excel("data/matrimonios/edades/matrimonios_edad_femenino_2001_2022.xlsx")))



limpiame_ONE_edad <- function(datos_sucios_one, nombre_variable= "edad", genero="masculino") { 
  
  vector_provincias <- c("Distrito Nacional",                "Provincia Azua",                  
                         "Provincia Baoruco" ,               "Provincia Barahona",               "Provincia Dajabón",               
                         "Provincia Duarte"   ,              "Provincia El Seibo",               "Provincia Elías Piña" ,           
                         "Provincia Espaillat" ,             "Provincia Hato Mayor",             "Provincia Hermanas Mirabal" ,     
                         "Provincia Independencia",          "Provincia La Altagracia",          "Provincia La Romana"     ,        
                         "Provincia La Vega"       ,         "Provincia María Trinidad Sánchez", "Provincia Monseñor Nouel"  ,      
                         "Provincia Monte Cristi"   ,        "Provincia Monte Plata" ,           "Provincia Pedernales",            
                         "Provincia Peravia",                "Provincia Puerto Plata",           "Provincia Samaná",                
                         "Provincia San Cristóbal",          "Provincia San José De Ocoa",       "Provincia San Juan" ,             
                         "Provincia San Pedro De Macorís",   "Provincia Sánchez Ramírez",        "Provincia Santiago",              
                         "Provincia Santiago Rodríguez",     "Provincia Santo Domingo",          "Provincia Valverde")

    por_provincia_edad <- list()
    for (nombre_provincia in vector_provincias){ 
      ubicacion_provincia <- which(edad_del_conyugue == nombre_provincia, arr.ind = TRUE)
      ubicacion_fila <- ubicacion_provincia[1]
      provincia_sliced <- edad_del_conyugue[ubicacion_provincia[1]:nrow(edad_del_conyugue), ]
  
      slicing <- ifelse(genero=="masculino","Edad del contrayente", "Edad de la contrayente" )
      ubicacion_inicio_sliced <- which(provincia_sliced == slicing, arr.ind = TRUE)
      ubicacion_termino_sliced <- which(provincia_sliced == "Total", arr.ind = TRUE)
      
      provincia_sliced <- provincia_sliced[ubicacion_inicio_sliced[[1]]:ubicacion_termino_sliced[[1]],]
      provincia_sliced[2, 2] <- nombre_variable
      colnames(provincia_sliced) <- provincia_sliced[2, ]
      provincia_sliced <- provincia_sliced[3:nrow(provincia_sliced), 2:ncol(provincia_sliced)]
      provincia_sliced <- provincia_sliced %>% gather("periodo", "cantidad", 2:ncol(provincia_sliced))
      provincia_sliced$provincia <- nombre_provincia
      
      por_provincia_edad[[nombre_provincia]] <- provincia_sliced
    }
    
    todos_por_provincia_edad <- do.call(rbind, por_provincia_edad)
    rownames(todos_por_provincia_edad) <- NULL

return(todos_por_provincia_edad)
}

edad_del_conyugue <- limpiame_ONE_edad(edad_del_conyugue, "edad_conyugue", "masculino")
edad_de_la_conyugue <- limpiame_ONE_edad(edad_de_la_conyugue, "edad_de_la_conyugue", "femenino")


##Funcion transformacion de datos
limpiame_ONE <- function(datos_sucios_one, nombre_variable= "x") {
  datos_sucios_one <- datos_sucios_one[11:43, 2:ncol(datos_sucios_one)]
  print(nombre_variable)
  datos_sucios_one[1,1] <- "provincia"
  colnames(datos_sucios_one) <- datos_sucios_one[1,]
  datos_sucios_one <- datos_sucios_one[2:nrow(datos_sucios_one),]
  datos_sucios_one <- datos_sucios_one %>% gather("periodo","variable", 2:ncol(datos_sucios_one)) 
  datos_sucios_one$variable <- as.numeric(datos_sucios_one$variable)
  colnames(datos_sucios_one)[3] <- nombre_variable
  return(datos_sucios_one)
}





















