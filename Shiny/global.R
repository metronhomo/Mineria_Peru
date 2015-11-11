library(corrplot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(lubridate)

# Base_Peru_con_Grupos_y_Catalogo_sin_LINEA_CREDITO <- readRDS("./data/Base_Peru_con_Grupos_y_Catalogo_sin_LINEA_CREDITO.RDS")
#Base_Red_Corregida_sin_LINEA_CREDITO <- readRDS("./data/Base_Red_Corregida_sin_LINEA_CREDITO.RDS")
caracterizacion <- readRDS("./data/Base_caracterizacion_grupos.RDS")
# caracterizacion <- readRDS("./data/Base_caracterizacion_grupos_2.RDS") #Nada más para que cargue rápido ahorita en el shiny, después ponerle la grande
base_a_2008 <- readRDS("./data/base_a_2008.RDS")
base_2008_2011 <- readRDS("./data/base_2008_2011.RDS")
base_2012_2015 <- readRDS("./data/base_2012_2015.RDS")
cluster_2008_2011 <- readRDS("./data/cluster_2008_2011.RDS")
cluster_2012_2015 <- readRDS("./data/cluster_2012_2015.RDS")
cluster_a_2008 <- readRDS("./data/cluster_a_2008.RDS")
correlaciones_2008_2011 <- readRDS("./data/correlaciones_2008_2011.RDS")
correlaciones_2012_2015 <- readRDS("./data/correlaciones_2012_2015.RDS")
correlaciones_a_2008 <- readRDS("data/correlaciones_a_2008.RDS")
#Resumen_variables_extra_sin_LINEA_CREDITO_con_grupos <- readRDS("./data/Resumen_variables_extra_sin_LINEA_CREDITO_con_grupos.RDS")

idx <- base_2008_2011$Edocivil == "error"
base_2008_2011$Edocivil[idx] <- "Otro"
base_2008_2011$Edocivil <- factor(base_2008_2011$Edocivil)

idx <- base_a_2008$Edocivil == "error"
base_a_2008$Edocivil[idx] <- "Otro"
base_a_2008$Edocivil <- factor(base_a_2008$Edocivil)

idx <- base_2012_2015$Edocivil == "error"
base_2012_2015$Edocivil[idx] <- "Otro"
base_2012_2015$Edocivil <- factor(base_2012_2015$Edocivil)

choices_menu4 <- list(
  #"Mercancias_P",
  #"Motos_P",
  #"Telycomp_P",
  #"Personales_P",
  #"TAZ_P",
  "Sexo",
  "Estado Civil" = "Edocivil",
  "Edad" = "Edad_C",
  'Ingreso' = "Ingreso_C",
  'Número de dependientes' =  "Dependientes_C",
  'Saldados' = "Saldados_C",
  'Activos' = "Activos_C",
  'Difícil cobro' = "Dificil_Cobro_C",
  'Atrasos' = "Atrasos_C",
  'Cancelados' = "Cancelados_C",
  #"meses_ultimacompra",
  'Años desde primera compra' = "anios_primercompra_C",
  'Saldo' = "Saldo_C",
  #"anio_llegada",
  'Edad de llegada' = "edad_llegada.C",
  #"Fec_surt",
  #"Mercancias_SC",
  #"Motos_SC",
  #"Telycomp_SC",
  #"Personales_SC",
  #"TAZ_SC",
  #"Depto",
  #"Subdepto",
  #"Clase",
  #"Subclase",
  'Depto_Desc' = "Depto_Desc"
  #"Subdepto_Desc",
  #"Clase_Desc",
  #"Subclase_Desc",
  #"ITV"
)


nombres_feos <- c(
  "Activos_C",
  "anio_llegada",
  "anios_primercompra_C",
  "Atrasos_C",
  "Cancelados_C",
  "Cap_Actual_C",
  "Dependientes_C",
  "Edad_C",
  "Edocivil",
  "Ingreso_C",
  "LCR",
  "Mercancinas_P_C",
  "meses_ultimacompra_C",
  "Motos_P_C",
  "Normalidad",
  "Personales_P_C",
  "porc_cap_pago_usado_C",
  "Región",
  "Saldo_C",
  "Sexo",
  "TAZ_P_C",
  "Telycomp_P_C"
)

nombres_bonitos <- c(
  "Pedidos Activos",
  "Año llegada",
  "Años desde primera compra",
  "Número de Atrasos",
  "Pedidos Cancelados",
  "Capacidad actual",
  "Número de dependientes",
  "Edad",
  "Estado Civil",
  "Ingreso",
  "Status de Línea de Crédito",
  "Pedidos de Mercancías",
  "Meses desde UC",
  "Pedidos de Motos",
  "Normalidad",
  "Pedidos de Préstamos Personales",
  "Capacidad de Pago usada (%)",
  "Región",
  "Saldo",
  "Género",
  "Pedidos TAZ",
  "Telefonía y Cómputo"
)


source('helpers.R')


