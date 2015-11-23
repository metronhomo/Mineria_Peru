library(dplyr)

base <- readRDS("./data/Base_caracterizacion_grupos.RDS")

base <- base %>%
  select(Folio, Pais, Canal, Tienda_Registro, anio_llegada, edad_llegada.C)

saveRDS(base, "./data/Base_caracterizacion_grupos_2.RDS")