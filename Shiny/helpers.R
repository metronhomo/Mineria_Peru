theme_MH <- function() {
  theme(
      axis.text.x=element_text(angle=90,size=22, colour = 'black'),
      axis.text.y=element_text(size=22, colour = 'black'),
      panel.background=element_rect(fill='grey95'),
      strip.background=element_rect(fill="#2c3e50"),
      panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
      strip.text.x = element_text(colour = 'white', size = 22),
      strip.text.y = element_text(colour = 'white', size = 22),
      legend.text=element_text(size=24),
      legend.title=element_blank(),
      panel.grid.major = element_line(colour = 'grey70', linetype = 'dashed'),
      panel.grid.minor = element_line(colour = 'grey70', linetype = 'dashed'))
  }


menu1 <- function(){
  #fluidPage(
    wellPanel(
      helpText(h4('Selecciona los años que quieres ver.')),
      radioButtons(
        'filtroAnio1', 
        label = '',
        choices = list(
          '2008' = '2008',
          '2008 - 2011' = '2008 - 2011',
          '2012 - 2015' = '2012 - 2015'),
        selected = c('2008'))
   # )
  )}

menu2 <- function(){
  #fluidPage(
  wellPanel(
    helpText(h4('Selecciona los años que quieres ver.')),
    radioButtons(
      'filtroAnio2', 
      label = '',
      choices = list(
        '2008' = '2008',
        '2008 - 2011' = '2008 - 2011',
        '2012 - 2015' = '2012 - 2015'),
      selected = c('2008'))
    # )
  )}

menu3 <- function(){
  #fluidPage(
  wellPanel(
    helpText(h4('Selecciona los años que quieres ver.')),
    radioButtons(
      'filtroAnio3', 
      label = '',
      choices = list(
        '2008' = '2008',
        '2008 - 2011' = '2008 - 2011',
        '2012 - 2015' = '2012 - 2015'),
      selected = c('2008'))
    # )
  )}


menu4 <- function(){
  #fluidPage(
  wellPanel(
    helpText(h4('Selecciona los años que quieres ver.')),
    radioButtons(
      'filtroAnio4', 
      label = '',
      choices = list(
        '2008' = '2008',
        '2008 - 2011' = '2008 - 2011',
        '2012 - 2015' = '2012 - 2015'),
      selected = c('2008')),
    br(),
    helpText(h4('Selecciona las variables que quieres cruzar.')),
    selectInput(
      'var_cruce_1', 
      label = '',
      choices = choices_menu4,
      selected = c('Sexo')),
    selectInput(
      'var_cruce_2', 
      label = '',
      choices = choices_menu4,
      selected = c('Edocivil'))
    # )
  )}


menu5 <- function(x = '2008'){
  #fluidPage(
  wellPanel(
    helpText(h4('Selecciona los años que quieres ver.')),
    radioButtons(
      'filtroAnio5', 
      label = '',
      choices = list(
        '2008' = '2008',
        '2008 - 2011' = '2008 - 2011',
        '2012 - 2015' = '2012 - 2015'),
      selected = x)
    # )
  )}

submenu_5_linea <- function(){
  #fluidPage(
  wellPanel(
    helpText(h4('Selecciona la línea que quieres ver.')),
    selectInput(
      'filtroLinea', 
      label = '',
      choices = list(
        "SERVICIOS" = "SERVICIOS",
        "ELECTRÓNICA" = "ELECTRONICA",
        "LÍNEA BLANCA" = "LINEA BLANCA",
        "COLCHONES Y BOXES" = "COLCHONES Y BOXES",
        "MUEBLES" = "MUEBLES",
        "SEGUROS AZTECA" = "SEGUROS AZTECA",
        "TELEFONÍA" = "TELEFONIA",
        "TRANSPORTE" = "TRANSPORTE",
        "CÓMPUTO" = "COMPUTO",
        "NUEVOS NEGOCIOS" = "NUEVOS NEGOCIOS",
        "MOTOCICLETAS" = "MOTOCICLETAS",
        "ENTRETENIMIENTO"
        ),
      selected = "SERVICIOS"),
    radioButtons('total_sublinea', 
    label = '',
    choices = list(
      'Total',
      'Sublíneas'),
    selected = 'Total'
    )
  )}

g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)
}

graf_barras<-function(df,var_x,var_y,titulo="",facet=F,var_facet="",
                      angulo=0,label=percent,x_lab="",y_lab="",n=1){  
  if(!facet){
    
    df<- df %>%
      dplyr::select(one_of(var_x,var_y))
    names(df)<-c("var_x","var_y")
    
    ggplot(df,aes(x=var_x,y=var_y)) +
      geom_bar(stat="identity",fill="#2c3e50",colour="black") +
      geom_text(aes(y=var_y + .03,label=round(var_y*100)),colour='black',size=6) +
      theme_MH() +
      scale_y_continuous(labels=label)  +
      ylab("") +
      xlab("")
  }else{    
    df<- df %>%
      dplyr::select(one_of(var_x,var_y,var_facet))
    names(df)<-c("var_x","var_y","var_facet")
    
    ggplot(df,aes(x=var_x,y=var_y)) +
      geom_bar(stat="identity",fill="#2c3e50",colour="black") +
      geom_text(aes(y=var_y + .03,label=round(var_y*100)),colour='black',size=6) +
      theme_MH() +
      facet_wrap(~var_facet,ncol=n) +
      scale_y_continuous(labels=label)  +
      ylab("") +
      xlab("")
  }
}

graf_densidad<-function(df,var_x,facet=F,var_facet="",x_lim_min,n=1,
                        x_lim_max, salto, angulo=0,label=comma,x_lab="",y_lab="",titulo=""){  
  if(!facet){
    
    df<- df %>%
      dplyr::select(one_of(var_x))
    names(df)<-c("var_x")
    
    ggplot(df,aes(x=var_x)) +
      geom_density(fill="#2c3e50",colour="black") +
      theme_MH() +
      scale_y_continuous(labels=label) +
      scale_x_continuous(breaks=seq(x_lim_min,x_lim_max,by=salto),limits=c(x_lim_min,x_lim_max))  +
      ylab("") +
      xlab("") 
  }else{    
    df<- df %>%
      dplyr::select(one_of(var_x,var_facet))
    names(df)<-c("var_x","var_facet")
    
    ggplot(df,aes(x=var_x)) +
      geom_density(fill="#2c3e50",colour="black") +
      theme_MH() +
      scale_y_continuous(labels=label) +
      scale_x_continuous(breaks=seq(x_lim_min,x_lim_max,by=salto),limits=c(x_lim_min,x_lim_max)) +
      facet_wrap(~var_facet,ncol=n)  +
      ylab("") +
      xlab("") 
  }
}

graf1 <- function(base){
  
  personas <- base %>%
    dplyr::group_by(Folio,Pais,Canal,Tienda_Registro,clase) %>%
    dplyr::summarise(personas=n()) %>%
    dplyr::group_by(clase) %>%
    dplyr::summarise(personas=n()) %>%
    dplyr::mutate(porc_personas=personas/sum(personas))
  
  dinero <- base %>%
    dplyr::group_by(clase) %>% 
    dplyr::summarise(dinero=sum(as.numeric(Saldo))) %>%
    dplyr::mutate(porc_dinero=dinero/sum(dinero))
  
  productos <- base %>%
    dplyr::group_by(clase) %>% 
    dplyr::summarise(productos = sum(Mercancias_P,Motos_P,Telycomp_P,Personales_P,TAZ_P)) %>%
    dplyr::mutate(porc_productos=productos/sum(productos))
  
  people<-graf_barras(df=personas,var_x="clase",var_y="porc_personas",
                      titulo="Distribución de personas vista por grupo",x_lab="Grupo")
  products<-graf_barras(df=productos,var_x="clase",var_y="porc_productos",
                        titulo="Distribución de productos vista por grupo",x_lab="Grupo")
  money<-graf_barras(df=dinero,var_x="clase",var_y="porc_dinero",
                     titulo="Distribución del gasto vista por grupo",x_lab="Grupo")
  
  grafica<-grid.arrange(people,products,money,ncol=1)
  
  return(grafica)
}

graf2 <- function(base){
  x<-as.data.frame(prop.table(table(base$Sexo,base$clase),2))
  y<-as.data.frame(prop.table(table(base$Sexo)))
  y$Var2<-"General"
  x<-rbind(x,y[,c(1,3,2)])
  names(x)<-c("Genero","clase","Porc")
  levels(x$Genero) <- c("Femenino","Masculino")
  
  graf_barras(df=x,var_x="Genero",var_y="Porc",facet=T,var_facet="clase",
              angulo=90,x_lab="Género")
}

graf3 <- function(base){

  x<-as.data.frame(prop.table(table(base$Edocivil,base$clase),2))
  y<-as.data.frame(prop.table(table(base$Edocivil)))
  y$Var2<-"General"
  x<-rbind(x,y[,c(1,3,2)])
  names(x)<-c("Edo_Civil","clase","Porc")
  
  graf_barras(df=x,var_x="Edo_Civil",var_y="Porc",facet=T,var_facet="clase",
              angulo=90,x_lab="Estado Civil") 
}

graf4 <- function(base){
  x<-as.data.frame(prop.table(table(base$Edad_C,base$clase),2))
  y<-as.data.frame(prop.table(table(base$Edad_C)))
  y$Var2<-"General"
  x<-rbind(x,y[,c(1,3,2)])
  names(x)<-c("Edad","clase","Porc")
  
  graf_barras(df=x,var_x="Edad",var_y="Porc",
              titulo="Distribución de la Edad vista por grupo",facet=T,var_facet="clase",
              angulo=90,x_lab="Edad")
}

graf5 <- function(base){

  ingreso <- base %>%
    filter(Ingreso_C!="Cero")
  
  x<-as.data.frame(prop.table(table(ingreso$Ingreso_C,ingreso$clase),2))
  y<-as.data.frame(prop.table(table(ingreso$Ingreso_C)))
  y$Var2<-"General"
  x<-rbind(x,y[,c(1,3,2)])
  names(x)<-c("Ingreso_Mensual","clase","Porc")
  
  graf_barras(df=x,var_x="Ingreso_Mensual",var_y="Porc",
              titulo="Distribución del Ingreso Mensual vista por grupo",facet=T,
              var_facet="clase", angulo=90,x_lab="Ingreso Mensual")
}

graf6 <- function(base){
  edades_anio<-base %>%
    group_by(anio_llegada,edad_llegada.C)%>%
    summarise(conteo=n()) %>%
    mutate(p_conteo=conteo/sum(conteo))%>%
    filter(edad_llegada.C!="1 a 17")
  
  ggplot(edades_anio,aes(x=anio_llegada,y=p_conteo,group=factor(edad_llegada.C),
                         colour=factor(edad_llegada.C))) + 
    geom_smooth(alpha=.4) +
    scale_x_continuous(breaks = seq(2000,2015, by = 1)) +
    scale_y_continuous(labels=percent) +
    theme_MH() +
    guides(col=guide_legend(title.hjust =0.5)) +
    scale_color_discrete(name="Edad") +
    ylab("") +
    xlab("")
}

graf7 <- function(base){
  x<-as.data.frame(prop.table(table(base$Saldados_C,base$clase),2))
  y<-as.data.frame(prop.table(table(base$Saldados_C)))
  y$Var2<-"General"
  x<-rbind(x,y[,c(1,3,2)])
  names(x)<-c("Saldados","clase","Porc")
  
  graf_barras(df=x,var_x="Saldados",var_y="Porc",
              titulo="Distribución del Número de pedidos saldados vista por grupo",facet=T,
              var_facet="clase", angulo=90,x_lab="Pedidos Saldados")
}

graf8 <- function(base){
  x<-as.data.frame(prop.table(table(base$Activos_C,base$clase),2))
  y<-as.data.frame(prop.table(table(base$Activos_C)))
  y$Var2<-"General"
  x<-rbind(x,y[,c(1,3,2)])
  names(x)<-c("Activos","clase","Porc")
  
  graf_barras(df=x,var_x="Activos",var_y="Porc",
              titulo="Distribución del Número de pedidos activos vista por grupo",facet=T,
              var_facet="clase", angulo=90,x_lab="Pedidos Activos")
}

graf9 <- function(base){
  x<-as.data.frame(prop.table(table(base$Atrasos_C,base$clase),2))
  y<-as.data.frame(prop.table(table(base$Atrasos_C)))
  y$Var2<-"General"
  x<-rbind(x,y[,c(1,3,2)])
  names(x)<-c("Atrasos","clase","Porc")
  
  graf_barras(df=x,var_x="Atrasos",var_y="Porc",
              titulo="Variable Atrasos vista por grupo",facet=T,
              var_facet="clase", angulo=90,x_lab="Atrasos")
}

graf10 <- function(base){
  x<-as.data.frame(prop.table(table(base$Cancelados_C,base$clase),2))
  y<-as.data.frame(prop.table(table(base$Cancelados_C)))
  y$Var2<-"General"
  x<-rbind(x,y[,c(1,3,2)])
  names(x)<-c("Cancelados","clase","Porc")
  
  graf_barras(df=x,var_x="Cancelados",var_y="Porc",
              titulo="Distribución de los pedidos cancelados vista por grupo",facet=T,
              var_facet="clase", angulo=90,x_lab="Cancelados")
}

graf11 <- function(base){
  x<-as.data.frame(prop.table(table(base$anios_primercompra_C,base$clase),2))
  y<-as.data.frame(prop.table(table(base$anios_primercompra_C)))
  y$Var2<-"General"
  x<-rbind(x,y[,c(1,3,2)])
  names(x)<-c("A1C","clase","Porc")
  
  graf_barras(df=x,var_x="A1C",var_y="Porc",
              titulo="Distribución de los años desde 1C vista por grupo",facet=T,
              var_facet="clase", angulo=90,x_lab="Meses desde 1C")
}

graf12 <- function(base){
  x<-as.data.frame(prop.table(table(base$meses_ultimacompra,base$clase),2))
  y<-as.data.frame(prop.table(table(base$meses_ultimacompra)))
  y$Var2<-"General"
  x<-rbind(x,y[,c(1,3,2)])
  names(x)<-c("MUC","clase","Porc")
  
  graf_barras(df=x,var_x="MUC",var_y="Porc",
              titulo="Distribución de los meses desde UC vista por grupo",facet=T,
              var_facet="clase", angulo=90,x_lab="Meses desde UC")
}

graf13 <- function(base){
   base<- base %>%
    mutate(anio_compra=year(Fec_surt),
           semana_compra=week(Fec_surt)) %>%
    filter(anio_compra>1980 & anio_compra<2015)
  
  ggplot(base,aes(x=semana_compra)) +
    geom_density(fill="#2c3e50",colour="black") +
    theme_MH() +
    scale_y_continuous(labels=percent) +
    scale_x_continuous(breaks=seq(0,53,by=20),limits=c(0,53)) +
    facet_grid(clase~anio_compra) +
    xlab("Semana") + 
    ylab("")
}

