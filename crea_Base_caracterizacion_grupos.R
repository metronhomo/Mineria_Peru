library(dplyr)

base <- readRDS("./data/old/Base_Peru_con_Grupos_y_Catalogo_sin_LINEA_CREDITO.RDS")

grupos <- readRDS("./data/Resumen_variables_extra_sin_LINEA_CREDITO_con_grupos.RDS")

grupos <- grupos %>%
  dplyr::select(2:5,102,112, clase)

base <- base %>%
  dplyr::select(Folio, Pais, Canal, Sucursal, Tienda_Registro, Mercancias_P, Motos_P, Telycomp_P, Personales_P, 
                TAZ_P, Sexo, Edocivil, Edad_C, Ingreso_C, Dependientes_C, Saldados_C, Activos_C, Dificil_Cobro_C,
                Atrasos_C, Cancelados_C, meses_ultimacompra, anios_primercompra_C, Saldo_C, anio_llegada, 
                edad_llegada.C, Fec_surt, Mercancias_SC, Motos_SC, Telycomp_SC, Personales_SC,TAZ_SC, Saldo,
                Depto, Subdepto, Clase, Subclase, Depto_Desc,  Subdepto_Desc, Clase_Desc, Subclase_Desc, ITV,
                Mercancinas_P_C,Personales_P_C,TAZ_P_C,Activos_C,meses_ultimacompra_C, Proddesc)

base <- base %>% 
  inner_join(grupos)


saveRDS(base,"./data/Base_caracterizacion_grupos.RDS")
