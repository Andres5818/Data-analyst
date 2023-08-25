  # Nuevo proyecto de BASE DE DATOS para las cifras
  
library(tidyverse)
library(stringr)
library(readxl)

df <- "Base_marzo.csv"
lf <- "Livo_marzo.csv"
Base1 <- read_csv2(df)

#PIVOTS DE UNIDADES POR VARIABLES

Base1 <- tibble(select(Base1, 1:94, starts_with("saldo2019"),starts_with("saldo2020"),
                      starts_with("saldo2021"),starts_with("saldo2022"), -contains("saldo_mes")))

Base1 <- pivot_longer(Base1, cols = starts_with("saldo"),
                           names_to = "Oferta",
                           values_to = "Valores_Oferta")




Base2 <- read_csv2(df)
Base2 <- select(Base2, 66, starts_with("ventas2019"),starts_with("ventas2020"),
                starts_with("ventas2021"),starts_with("ventas2022"))

  
Base2 <- pivot_longer(Base2, cols = starts_with("ventas"), 
                       names_to = "Ventas",
                       values_to = "Valores_ventas")



  
Base3 <- read_csv2(df)
Base3 <- select(Base3, 66, starts_with("precioenmiles2019"),starts_with("precioenmiles2020"),
                starts_with("precioenmiles2021"),starts_with("precioenmiles2022"))
  
Base3 <- pivot_longer(Base3, cols = starts_with("precioenmiles"), 
                        names_to = "Precioenmiles",
                        values_to = "Precio_(Miles)")
  
Base4 <- read_csv2(df)
Base4 <- select(Base4, 66, starts_with("preciomc2019"),starts_with("preciomc2020"),
                starts_with("preciomc2021"),starts_with("preciomc2022"))

Base4 <- pivot_longer(Base4, cols = starts_with("preciomc"), 
                        names_to = "Preciomc",
                        values_to = "Precio_(Metro_2)")
  
Base5 <- read_csv2(df)
Base5 <- select(Base5, 66, starts_with("estado2019"),starts_with("estado2020"),
                starts_with("estado2021"),starts_with("estado2022"), -contains("estado_mes"))
Base5 <- tibble(Base5)
  
Base5 <- pivot_longer(Base5, cols = starts_with("estado"), 
                        names_to = "Categoria_Estado",
                        values_to = "Estados")
  
Base6 <- read_csv2(df)
Base6 <- select(Base6, 66, starts_with("fase2019"),starts_with("fase2020"),
                starts_with("fase2021"),starts_with("fase2022"),-contains("fase_mes"))

  
Base6 <- pivot_longer(Base6, cols = starts_with("fase"), 
                        names_to = "Categoria_Fase",
                        values_to = "Fases")
  
Base7 <- read_csv2(df)
Base7 <- select(Base7, 66, starts_with("renuncias2019"),starts_with("renuncias2020"),
                starts_with("renuncias2021"),starts_with("renuncias2022"))

  
Base7 <- pivot_longer(Base7, cols = starts_with("renuncias"), 
                        names_to = "Renuncias",
                        values_to = "Valores_Renuncia")
  
Base <- cbind(Base1, Base2, Base3, Base4, Base5, Base6, Base7)
  
rm(Base1, Base2, Base3, Base4, Base5, Base6, Base7)

colnames(Base)[6] <- "Proyecto"

Base <- Base %>% select(-contains("idtipo"))

Base <- Base %>% mutate(Año = substr(Oferta, 6, 9),
                                    Mes = substr(Oferta, 10, 11),
                                    Dia = substr(Oferta, 12, 13),
                                    Calendario = paste0(Año,"/", Mes, "/", Dia))

Base$Calendario <- as.Date(Base$Calendario, format = "%Y/%m/%d")

Base$Oferta <- substr(Base$Oferta, 1, 5)
Base$Ventas <- substr(Base$Ventas, 1, 6)
Base$Precioenmiles <- substr(Base$Precioenmiles, 1, 13)
Base$Preciomc <- substr(Base$Preciomc, 1, 8)
Base$Renuncias <- substr(Base$Renuncias, 1, 9)
Base$Estado <- substr(Base$Categoria_Estado, 1, 6)
Base$Fase <- substr(Base$Categoria_Fase, 1, 4)

Base <- select(Base, -Categoria_Estado, -Categoria_Fase, -Renuncias, -Preciomc, -Precioenmiles,
                        -Oferta, -Ventas)

#------------------------------------------------END PIVOTS-----------------------------------------------

#TRATAMIENTO LIVO

Livo <- read_csv2(lf)

Base1 <- read_csv2(df)
Base1$latitud <- as.numeric(Base1$latitud)
Base1$latitud <- round(Base1$latitud,3)
Base1$longitud <- as.numeric(Base1$longitud) 
Base1$longitud <- round(Base1$longitud,3)

Livo <- Livo %>%
  mutate(Año = substr(fecha, 1, 4),
         Mes = substr(fecha, 5, 6),
         Dia = substr(fecha, 7, 8))%>%
  filter(Año %in% c(2018,2019,2020,2021)) %>% 
  mutate(Proyecto = substr(identificador,
                           str_locate(identificador,"-")+1,100))

Base1 <- Base1 %>% rename(Proyecto = nombre_proyecto)

Base_1 <- Base1 %>% select(Proyecto, longitud, latitud, localidad_comuna)

Livo_n <- merge(x = Livo, y = Base_1, by = "Proyecto")
Livo_n <- Livo_n[!duplicated(Livo_n),]
Livo_n <- Livo_n %>% mutate(Calendario = paste(Dia,"/",Mes,"/",Año))
Livo_n <- Livo_n %>% mutate(Last = ifelse(Livo_n$last_estado == "Cancelado", 
                                          "Cancelado", Livo_n$estado))
Livo_n <- Livo_n %>% mutate(Estado = ifelse(Livo_n$Last == "Construcción" &
                                              Livo_n$fase == "Terminado", "Terminado",
                                            Livo_n$Last))
Livo_n <- Livo_n %>% mutate(Fase = ifelse(Livo_n$Estado == "Proyectado", "Proyectado",
                                          ifelse(Livo_n$Estado == "Rediseñado", "Rediseñado",
                                                 ifelse(Livo_n$Estado == "TVE", "TVE",
                                                        ifelse(Livo_n$Estado == "TE","TE",
                                                               ifelse(Livo_n$Estado == "Paralizado","Paralizado",
                                                                      ifelse(Livo_n$Estado == "Cancelado", "Cancelado",
                                                                             Livo_n$fase)))))))
Livo_n <- Livo_n %>% mutate(rango_atlantico = 
                              ifelse(Livo_n$tipo_vivienda == "VIS", "VIS",
                                     ifelse(Livo_n$tipo_vivienda == "VIP", "VIP",
                                            ifelse(Livo_n$tipo_vivienda == "No VIS" & 
                                                     Livo_n$nuevorango_pre == "VIS 70 - 135 SML",
                                                   "$119 - $207 Mill (135 - 235 SML)",
                                                   ifelse(Livo_n$tipo_vivienda == "No VIS" & 
                                                            Livo_n$nuevorango_pre == "VIP", "VIS",
                                                          Livo_n$nuevorango_pre)))))
Livo_n <- Livo_n %>% mutate(rango_ppm = 
                              ifelse(Livo_n$rango_ppm2 == "501 - 900","$501 - $900",
                                     ifelse(Livo_n$rango_ppm2 == "1.701 - 2.100", "$1.701 - $2.100",
                                            ifelse(Livo_n$rango_ppm2 == "<=500", "<=$500",
                                                   ifelse(Livo_n$rango_ppm2 == "1.301 - 1.700","$1.301 - $1.700",
                                                          ifelse(Livo_n$rango_ppm2 == "2.801 - 3.200","$2.801 - $3.200",
                                                                 ifelse(Livo_n$rango_ppm2 == "3.601 - 4.000","$3.601 - $4.000",
                                                                        ifelse(Livo_n$rango_ppm2 == "901 - 1.300","$901 - $1.300",
                                                                               ifelse(Livo_n$rango_ppm2 == "2.401 - 2.800","$2.401 - $2.800",
                                                                                      ifelse(Livo_n$rango_ppm2 == "2.101 - 2.400","$2.101 - $2.400",
                                                                                             ifelse(Livo_n$rango_ppm2 == "4.801 - 5.200","$4.801 - $5.200",
                                                                                                    ifelse(Livo_n$rango_ppm2 == "3.201 - 3.600","$3.201 - $3.600",
                                                                                                           ifelse(Livo_n$rango_ppm2 == ">6.400",">$6.400",
                                                                                                                  ifelse(Livo_n$rango_ppm2 == "5.601 - 6.000","$5.601 - $6.000",
                                                                                                                         ifelse(Livo_n$rango_ppm2 == "5.201 - 5.600","$5.201 - $5.600",
                                                                                                                                ifelse(Livo_n$rango_ppm2 == "6.001 - 6.400","$6.001 - $6.400",
                                                                                                                                       ifelse(Livo_n$rango_ppm2 == "4.001 - 4.400","$4.001 - $4.400",
                                                                                                                                              ifelse(Livo_n$rango_ppm2 == "4.401 - 4.800","$4.401 - $4.800",
                                                                                                                                                     ifelse(Livo_n$rango_ppm2 == "> 6.400","> $6.400",
                                                                                                                                                            Livo_n$rango_ppm2)))))))))))))))))))
Livo_n <- Livo_n %>% 
  select(fecha, Año, Mes, Dia, Calendario, año_corrido, doce_meses, Proyecto,
         localidad_comuna, departamento, ciudad, zona, barrio, estrato, destino_etapa,
         uso_etapa, tipo_vivienda, Estado, Fase, longitud, latitud, compania_constructora,
         nuevorango_pre, rango_minviv, rango_atlantico, rango_ppm,
         rango_area, segmento_pre, usos, politica_vivienda, unidades, area, valor, 
         precio_mc_promedio,cuenta) %>% 
  rename(Fecha = fecha, Año_corrido = año_corrido, 
         Doce_meses = doce_meses, Localidad = localidad_comuna, Departamento = departamento,
         Ciudad = ciudad, Zona = zona, Barrio = barrio, Estrato = estrato, 
         Destino_etapa = destino_etapa,Tipo = uso_etapa, rango_ppm2 = rango_ppm, 
         Tipo_vivienda = tipo_vivienda, Longitud = longitud, Latitud = latitud)
Livo_n <- Livo_n %>% arrange(Fecha)

rm(Base_1, Livo, Base1)

#----------------------------------------------END LIVO---------------------------------------------------

#TRATAMIENTO BASE

Base$cocina <- as.numeric(Base$cocina)
Base$patio <- as.numeric(Base$patio)
Base$zona_de_ropas <- as.numeric(Base$zona_de_ropas)


# Reemplazo en la fase y estado

Base <- Base %>% 
  mutate(Fase = ifelse(last_estado == "TVE", "TVE",
                       ifelse(last_estado == "Rediseñado","Rediseñado",
                              ifelse(last_estado == "Cancelado", "Cancelado",
                                     ifelse(last_estado == "Proyectado", "Proyectado",
                                            ifelse(last_estado == "TE", "TE",
                                                   ifelse(last_estado == "Paralizado", "Paralizado",
                                                          last_fase)))))),
         Estado = ifelse(last_estado == "Construcción" & Fase == "Terminado",
                         "Terminado", last_estado))

#Promedio en base total 

Livo_tw <- Livo_n["Fecha"]
Livo_tw <- Livo_tw[!duplicated(Livo_tw$Fecha),]
Livo_tw <- tail(Livo_tw, n= 12) #últimas 12 fechas


Livo_m <- Livo_n %>% 
  filter(Fecha %in% Livo_tw,
         cuenta == "Ventas", Tipo == "Apartamento"| 
           Tipo == "Casa") %>% 
  group_by(Proyecto) %>%
  summarise(Sum_doce = round(sum(unidades, na.rm = T)/12,2)) %>% 
  mutate(Promedio_12_meses = paste(Sum_doce, "Unidades/mes"))


Base <- full_join(x = Base, y = Livo_m, by = "Proyecto")

rm(Livo_m, Livo_tw)

#Traer Listado de proyectos del mes

List_proyects <- read_csv2("Listado Proyectos.csv", col_names = T)


#Dado a que la mayoria de los nombres de las columnas se repetia, saltó un error, por ello
#Se uso la función col_names, para hacer distinción
#Dar formato CSV (UTF - 8) antes de subir

#ZCO_NOMBRE : 104


match("ZCO_NOMBRE", names(List_proyects))
match("AEX_AcabadosExtAcabado_4", names(List_proyects))

#ZONA DE PROYECTOS VAN DE LA COLUMNA 54 A LA 74
#SISTEMA CONSTRUCTIVO VA DE LA COLUMNA 75 A LA 89
#VENTANERIA, FACHADA Y CUBIERTAS,  VAN DE LA COLUMNA 90 A LA 99

List_proyects <- List_proyects[, c(match("Proyecto", names(List_proyects)), 
                                   match("ZCO_NOMBRE", names(List_proyects)):
                                     match("AEX_AcabadosExtAcabado_4", names(List_proyects)))]


Zonas_Comunes <- as.data.frame(apply(List_proyects, 2,
                                     function(Zonas_Comunes) 
                                       ifelse(is.na(Zonas_Comunes)," ", Zonas_Comunes)))

Zonas_Comunes <- apply(Zonas_Comunes[,match("ZCO_NOMBRE", names(List_proyects)):
                                       match("ZCO_NOMBRE_20", names(List_proyects))],
                       1, function(Zonas_Comunes) paste(Zonas_Comunes[Zonas_Comunes!= " "], 
                                                        collapse = " - "))

List_proyects <- cbind(List_proyects,Zonas_Comunes)


List_proyects <- List_proyects %>% 
  mutate(Sistema_Constructivo = ifelse(CTG_Categoría == "Sistema Constructivo R", CTG_Característica,
                                       ifelse(CTG_Categoría_1 == "Sistema Constructivo R", CTG_Característica_1,
                                              ifelse(CTG_Categoría_2 == "Sistema Constructivo R", CTG_Característica_2,
                                                     ifelse(CTG_Categoría_3 == "Sistema Constructivo R", CTG_Característica_3,
                                                            ifelse(CTG_Categoría_4 == "Sistema Constructivo R", CTG_Característica_4,""))))),
         Ventanería = ifelse(AEX_AcabadosExtcategoría == "Ventanería R - NR", AEX_AcabadosExtAcabado,
                             ifelse(AEX_AcabadosExtcategoría_1 == "Ventanería R - NR", AEX_AcabadosExtAcabado_1,
                                    ifelse(AEX_AcabadosExtcategoría_2 == "Ventanería R - NR", AEX_AcabadosExtAcabado_2,
                                           ifelse(AEX_AcabadosExtcategoría_3 == "Ventanería R - NR", AEX_AcabadosExtAcabado_3,
                                                  ifelse(AEX_AcabadosExtcategoría_4 == "Ventanería R - NR", AEX_AcabadosExtAcabado_4, ""))))),
         Cubiertas = ifelse(AEX_AcabadosExtcategoría == "Cubiertas R", AEX_AcabadosExtAcabado,
                            ifelse(AEX_AcabadosExtcategoría_1 == "Cubiertas R", AEX_AcabadosExtAcabado_1,
                                   ifelse(AEX_AcabadosExtcategoría_2 == "Cubiertas R", AEX_AcabadosExtAcabado_2,
                                          ifelse(AEX_AcabadosExtcategoría_3 == "Cubiertas R", AEX_AcabadosExtAcabado_3,
                                                 ifelse(AEX_AcabadosExtcategoría_4 == "Cubiertas R", AEX_AcabadosExtAcabado_4, ""))))),
         Fachada = ifelse(AEX_AcabadosExtcategoría == "Fachada R", AEX_AcabadosExtAcabado,
                          ifelse(AEX_AcabadosExtcategoría_1 == "Fachada R", AEX_AcabadosExtAcabado_1,
                                 ifelse(AEX_AcabadosExtcategoría_2 == "Fachada R", AEX_AcabadosExtAcabado_2,
                                        ifelse(AEX_AcabadosExtcategoría_3 == "Fachada R", AEX_AcabadosExtAcabado_3,
                                               ifelse(AEX_AcabadosExtcategoría_4 == "Fachada R", AEX_AcabadosExtAcabado_4, "")))))) %>% 
  select(Proyecto, Zonas_Comunes, Sistema_Constructivo, Ventanería, Cubiertas, Fachada)


Base <- full_join(x = Base, y = List_proyects, by = "Proyecto")
Base <- distinct(Base, .keep_all = T)

Base_2 <- Base[, c(1, length(Base)-2)]
col_names <- names(Base_2)
col_names

Base_2 <- rename(Base_2, Of_t = col_names[2])
Base_2 <- Base_2 %>% group_by(Proyecto) %>% summarise(Suma_saldo = sum(Of_t, na.rm = T))


Base <- merge(Base, Base_2, "Proyecto")

Base <- mutate(Base, Punto_de_equilibrio = round((numero_unidades-Suma_saldo)/numero_unidades,2),
               Punto_de_equilibrio_2 = ifelse(Fase == "TVE", 1,
                                              ifelse(Fase == "TE" , 1,
                                                     ifelse(Fase == "Rediseño", 0, 
                                                            ifelse(Fase == "Proyectado", 0,
                                                                   ifelse(Fase == "Paralizado", 0,
                                                                          ifelse(Fase == "Cancelado", 0,
                                                                                 ifelse(Suma_saldo == 0, 1,
                                                                                        Punto_de_equilibrio))))))))


Base_3 <- Base %>% filter(activo == "Activo", abierta_cerrada == "ABIERTA", 
                          uso_etapa == "Apartamento"|uso_etapa == "Casa",
                          Estado == "Preventa"| Estado == "Construcción"| Estado == "Terminado" ) 

Base_3 <- Base_3[,c(match("Proyecto", names(Base_3)),match("area_por_tipo", names(Base_3)),
                    match("alcobas", names(Base_3)), length(Base)-25,length(Base)-33,
                    length(Base)-41,length(Base)-49,length(Base)-57,length(Base)-65,
                    length(Base)-73,length(Base)-81,length(Base)-89,length(Base)-97,
                    length(Base)-105,length(Base)-113)]


## en cuanto a convertir un factor a un numero, éste primero debe pasar a character y luego a numerico,
#sino no funciona.
# el doble corchete trae una sublista, o el vector asociado a la columna
write.csv2(Base, "Base.csv")






Base_3[,4] <- as.numeric(Base_3[,4])
Base_3[,5] <- as.numeric(Base_3[,5])
Base_3[,6] <- as.numeric(Base_3[,6])
Base_3[,7] <- as.numeric(Base_3[,7])
Base_3[,8] <- as.numeric(Base_3[,8])
Base_3[,9] <- as.numeric(Base_3[,9])
Base_3[,10] <- as.numeric(Base_3[,10])
Base_3[,11] <- as.numeric(Base_3[,11])
Base_3[,12] <- as.numeric(Base_3[,12])
Base_3[,13] <- as.numeric(Base_3[,13])
Base_3[,14] <- as.numeric(Base_3[,14])
Base_3[,15] <- as.numeric(Base_3[,15])

Base_4 <- Base_3[,4:15]

Base_4 <- rowSums(Base_4, na.rm = T)

Base_3 <- cbind(Base_3, Base_4)

Base_3 <- Base_3 %>% group_by(Proyecto, area_por_tipo, alcobas) %>% 
  summarise(besta = sum(Base_4))

Base_3 <- merge(aggregate(besta ~ Proyecto, Base_3, max), Base_3)

## Aggregate() Function in R Splits the data into subsets, computes 
#summary statistics for each subsets and returns the result in a group by form

Base_3 <- distinct(Base_3, Proyecto, .keep_all = T)
Base_3$alcobas <- as.numeric(Base_3$alcobas)
Base_3$alcobas <- ifelse(is.na(Base_3$alcobas) == T, 0, Base_3$al)

Base_3 <- mutate(Base_3, Habitaciones = paste(area_por_tipo, " m2 ", " - ", alcobas, 
                                              ifelse(alcobas == 1, " Alcoba: ",
                                                     "Alcobas: "), besta, 
                                              ifelse(besta == 1,"Venta."," Ventas")))

Base <- full_join(Base, Base_3, "Proyecto")


cols <- c(match("activo", names(Base)), match("longitud", names(Base)),
          match("latitud", names(Base)), match("Proyecto", names(Base)),
          match("ciudad", names(Base)), match("zona", names(Base)),
          match("barrio", names(Base)), match("direccion_proyecto", names(Base)),
          match("telefono_del_proyecto", names(Base)),match("destino", names(Base)),
          match("uso_general", names(Base)), match("estrato", names(Base)),
          match("numero_unidades", names(Base)),match("area_lote", names(Base)),
          match("area_construida", names(Base)),match("pisos_por_bloque_r", names(Base)),
          match("aptos_por_piso_r", names(Base)),match("numero_parqueaderos_visitantes", names(Base)),
          match("numero_parqueaderos_propietarios", names(Base)),
          match("abierta_cerrada", names(Base)),match("codigo_etapa", names(Base)),
          match("nombre_etapa", names(Base)),match("numunidades", names(Base)),
          match("destino_etapa", names(Base)),match("uso_etapa", names(Base)),
          match("Estado", names(Base)), match("Fase", names(Base)),
          match("compania_constructora", names(Base)),
          match("direccion_constructora", names(Base)),
          match("telefono_constructora", names(Base)),
          match("compania_vendedora", names(Base)),
          match("direccion_vendedora", names(Base)),
          match("telefono_vendedora", names(Base)),
          match("entidad_credito_constructor", names(Base)),
          match("entidad_fiduciaria", names(Base)),
          match("condicion_entrega", names(Base)),
          match("fecha_inicio_venta", names(Base)),match("fecha_inicio_construccion", names(Base)),
          match("fecha_terminacion_construccion", names(Base)), match("fecha_entrega", names(Base)),
          match("tipo_vivienda", names(Base)),match("uso", names(Base)),
          match("unidades_por_tipo", names(Base)),
          match("area_por_tipo.x", names(Base)),match("Precio_en_miles", names(Base)),
          match("Precio_metro", names(Base)),match("alcobas.x", names(Base)),
          match("baños", names(Base)),match("balcón_terraza", names(Base)),
          match("estudio", names(Base)),match("sala_comedor", names(Base)),
          match("cocina", names(Base)),match("zona_de_ropas", names(Base)),
          match("patio", names(Base)),
          length(Base)-273, length(Base)-271,length(Base)-289,
          length(Base)-265, length(Base)-263,length(Base)-261,
          length(Base)-257, length(Base)-255,length(Base)-253,
          length(Base)-249, length(Base)-247,length(Base)-245,
          length(Base)-241, length(Base)-239,length(Base)-237,
          length(Base)-233, length(Base)-231,length(Base)-229,
          length(Base)-225, length(Base)-223,length(Base)-221,
          length(Base)-217, length(Base)-215,length(Base)-213,
          length(Base)-209, length(Base)-207,length(Base)-205,
          length(Base)-201, length(Base)-199,length(Base)-197,
          length(Base)-193, length(Base)-191,length(Base)-189,
          length(Base)-185, length(Base)-183,length(Base)-181, #dic 19
          length(Base)-177, length(Base)-175,length(Base)-173,
          length(Base)-169, length(Base)-167,length(Base)-165,
          length(Base)-161, length(Base)-159,length(Base)-157,
          length(Base)-153, length(Base)-151,length(Base)-149,
          length(Base)-145, length(Base)-143,length(Base)-141,
          length(Base)-137, length(Base)-135,length(Base)-133,
          length(Base)-129, length(Base)-127,length(Base)-125,
          length(Base)-121, length(Base)-119,length(Base)-117,
          length(Base)-113, length(Base)-111,length(Base)-109,
          length(Base)-105, length(Base)-103,length(Base)-101,
          length(Base)-97, length(Base)-95,length(Base)-93,
          length(Base)-89, length(Base)-87,length(Base)-85, #dic 2020
          length(Base)-81, length(Base)-79,length(Base)-77,
          length(Base)-73, length(Base)-71,length(Base)-59,
          length(Base)-65, length(Base)-63,length(Base)-61,
          length(Base)-57, length(Base)-55,length(Base)-53,
          length(Base)-49, length(Base)-47,length(Base)-45,
          length(Base)-41, length(Base)-39,length(Base)-37,
          length(Base)-33, length(Base)-31,length(Base)-29,
          length(Base)-30,match("Promedio_12_meses", names(Base)),
          match("Zonas_Comunes", names(Base)),match("Sistema_Constructivo", names(Base)),
          match("Ventanería", names(Base)),match("Cubiertas", names(Base)),
          match("Fachada", names(Base)),match("Punto_de_equilibrio_2", names(Base)),
          match("Habitaciones", names(Base)),match("localidad_comuna", names(Base)),
          match("numero_bloques", names(Base)),match("nit_constructora", names(Base)))

Base <- Base[,cols]

Base <- rename(Base, area_por_tipo = area_por_tipo.x,
               alcobas = alcobas.x, last_estado = Estado, 
               last_fase = Fase) 

colnames(Base)[148] <- "Oferta disponible" #148 es el Número de columna perteneciente al último saldo del mes actualizado.

Base <- arrange(Base, activo)

Base <- as.data.frame(apply(Base, 2, function(x) ifelse(is.na(x), "", x)))
Base$latitud <- as.numeric(Base$latitud)
Base$longitud <- as.numeric(Base$longitud)

#12/12/2021 <- para hacerlo automatico no pueden existir nombres de columnas distintas, por lo tanto se eliminaran
#aquellas que describan un historico en ventas

since <- match("patio", names(Base))+1
until <- match("Oferta disponible", names(Base))-1
Base <- select(Base, -(since:until))


write.csv2(Base, "Base.csv", row.names = F)
