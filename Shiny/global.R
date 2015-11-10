library(corrplot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Base_Peru_con_Grupos_y_Catalogo_sin_LINEA_CREDITO <- readRDS("./data/Base_Peru_con_Grupos_y_Catalogo_sin_LINEA_CREDITO.RDS")
#Base_Red_Corregida_sin_LINEA_CREDITO <- readRDS("./data/Base_Red_Corregida_sin_LINEA_CREDITO.RDS")
base <- readRDS("./data/Base_caracterizacion_grupos.RDS")
cluster_2008_2011 <- readRDS("./data/cluster_2008_2011.RDS")
cluster_2012_2015 <- readRDS("./data/cluster_2012_2015.RDS")
cluster_a_2008 <- readRDS("./data/cluster_a_2008.RDS")
correlaciones_2008_2011 <- readRDS("./data/correlaciones_2008_2011.RDS")
correlaciones_2012_2015 <- readRDS("./data/correlaciones_2012_2015.RDS")
correlaciones_a_2008 <- readRDS("data/correlaciones_a_2008.RDS")
#Resumen_variables_extra_sin_LINEA_CREDITO_con_grupos <- readRDS("./data/Resumen_variables_extra_sin_LINEA_CREDITO_con_grupos.RDS")


source('helpers.R')


