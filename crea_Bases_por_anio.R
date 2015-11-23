library(dplyr)

base <- readRDS("./data/Base_caracterizacion_grupos.RDS")

base_a_2008 <-base %>%
  mutate(anio_llegada=as.integer(as.character(anio_llegada))) %>%
  filter(anio_llegada<2008)

base_2008_2011 <-base %>%
  mutate(anio_llegada=as.integer(as.character(anio_llegada))) %>%
  filter(anio_llegada>=2008 & anio_llegada<=2011)

base_2012_2015 <-base %>%
  mutate(anio_llegada=as.integer(as.character(anio_llegada))) %>%
  filter(anio_llegada>2011)

saveRDS(base_a_2008, "./data/base_a_2008.RDS")
saveRDS(base_2008_2011, "./data/base_2008_2011.RDS")
saveRDS(base_2012_2015, "./data/base_2012_2015.RDS")