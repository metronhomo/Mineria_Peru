theme_MH <- function() {
  theme(
      axis.text.x=element_text(angle=90,size=22, colour = 'black'),
      axis.text.y=element_text(size=22, colour = 'black'),
      panel.background=element_rect(fill='grey95'),
      strip.background=element_rect(fill="#2c3e50"),
      panel.border = element_rect(colour = "#2c3e50", fill=NA, size=1),
      strip.text.x = element_text(colour = 'white', size = 22),
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
          'Antes de 2008' = '2008',
          '2008 - 2011' = '2008 - 2011',
          '2012 - 2015' = '2012 - 2015'),
        selected = c('2008'))
   # )
    )}

menu_im <- function(){
  #fluidPage(
  wellPanel(
    helpText(h4('Selecciona los años que quieres ver.')),
    radioButtons(
      'filtroAnio_im', 
      label = '',
      choices = list(
        'Antes de 2008' = '2008',
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
        'Antes de 2008' = '2008',
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
        'Antes de 2008' = '2008',
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
        'Antes de 2008' = '2008',
        '2008 - 2011' = '2008 - 2011',
        '2012 - 2015' = '2012 - 2015'),
      selected = c('2008'))
    # )
  )}

menu5 <- function(){
  #fluidPage(
  wellPanel(
    helpText(h4('Selecciona los años que quieres ver.')),
    radioButtons(
      'filtroAnio5', 
      label = '',
      choices = list(
        'Antes de 2008' = '2008',
        '2008 - 2011' = '2008 - 2011',
        '2012 - 2015' = '2012 - 2015'),
      selected = c('2008'))
    # )
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

linea<-c("Electronica","Linea Blanca", "Muebles")
sublinea<-list(c("Televisiones","Audio","Video"),c("Refrigeradores","Estufas y Microondas",
                                                   "Lavadoras y Secadoras"),
               c("Comedores y Antecomedores","Salas y Recamaras","Literas y Camas","Bicicletas"))

graficas<-function(i){
  articulos2 <- base %>%
    filter(Depto_Desc==linea[i],Subdepto_Desc %in% sublinea[[i]]) %>%
    group_by(Clase_Desc,cluster) %>%
    summarise(n=n(),dinero=sum(as.numeric(ITV),na.rm=T)) %>%
    group_by(cluster) %>%
    mutate(p_n=n/sum(n,na.rm=T),p_dinero=dinero/sum(dinero,na.rm=T)) %>%
    arrange(cluster,desc(dinero))
  
  total <- base %>%
    filter(Depto_Desc==linea[i],Subdepto_Desc %in% sublinea[[i]]) %>%
    group_by(Clase_Desc) %>%
    summarise(n=n(),dinero=sum(as.numeric(ITV),na.rm=T)) %>%
    mutate(p_n=n/sum(n,na.rm=T),p_dinero=dinero/sum(dinero,na.rm=T),cluster="Total") %>%
    arrange(desc(dinero))
  
  total<-total[,c(1,6,2:5)]
  
  articulos2<-rbind(articulos2,total)
  articulos2$Clase_Desc<-factor(articulos2$Clase_Desc,levels=as.character(total$Clase_Desc))
  
  
  ggplot(articulos2,aes(x=Clase_Desc,y=p_dinero)) + 
    geom_bar(stat="identity",fill="turquoise4",colour='black') +
    scale_y_continuous(labels=percent) +
    geom_text(aes(y=p_dinero +.05,
                  label=round(p_dinero*100)),
              colour="black",size=6) +
    facet_wrap(~cluster,ncol=1) +
    theme(panel.background=element_rect(fill='snow2'),
          text=element_text(size=20),
          strip.background=element_rect(fill='skyblue4'),
          axis.text.x  = element_text(angle=90, vjust=0.5),
          strip.text.x=element_text(colour="white",size=15)) +
    xlab("Clase") +
    ylab("") +
    ggtitle(paste("Gasto en clases de la sublínea ",linea[i]," vista por grupo",sep=""))
}

#Estas funciones limpian los valores de los préstamos personales
limpia<-function(d){
  d<- gsub(pattern='q',x=d,replacement='')  
  d <- gsub(pattern="\\s+",x=d,replacement='')
  d <- gsub(pattern="préstamo personal ",x=d,replacement='')
  d <- gsub("[[:space:]]", "", d)
  d <- gsub('[$]', '', d)
  d <- gsub(',', '', d)
  d <- gsub("(\\.)\\1+","",d,perl=T)
}

servicios<-function(df){
  df$Proddesc<-tolower(df$Proddesc)
  desc<-lapply(df$Proddesc,function(d){
    if(grepl("nales",d)){
      q <- str_trim(str_replace(d,"soles",""))
      q <- str_trim(str_replace(q,"s/",""))
      p <- str_locate_all(pattern ='nales',q)
      d <- str_trim(substr(q,p[[1]][2] + 2,nchar(q)))
      limpia(d)
    }else{
      if(grepl("nal",d)){
        q <- str_trim(str_replace(d,"soles",""))
        q <- str_trim(str_replace(q,"s/",""))
        p <- str_locate_all(pattern ='nal',q)
        d <- str_trim(substr(q,p[[1]][2] + 2,nchar(q)))
        limpia(d)
      }else
        d <- gsub("(\\.)\\1+","",d,perl=T)        
    }
  })    
  df$descripcion_limpia<-as.numeric(Reduce('rbind',desc))
  
  df2<- df %>%
    group_by(descripcion_limpia,cluster) %>%
    summarise(n=sum(n),dinero=sum(dinero)) %>%
    group_by(cluster) %>%
    mutate(p_n=n/sum(n,na.rm=T),p_dinero=dinero/sum(dinero,na.rm=T)) %>%
    mutate(descripcion=cut(descripcion_limpia,
                           breaks=c(-Inf,0,3000,4000,5000,6000,
                                    7000,8000,9000,10000,14000,Inf),
                           labels=c("Cero","1-3000","3001-4000",
                                    "4001-5000","5001-6000","6001-7000",
                                    "7001-8000","8001-9000","9001-10000",
                                    "10001-14000","Más de 14000")))%>%
    arrange(cluster,desc(dinero))
  return(df2)
}

graficas2<-function(i){
  articulos2 <- base %>%
    filter(Depto_Desc==linea[i],Subdepto_Desc %in% sublinea[[i]]) %>%
    group_by(Proddesc,cluster) %>%
    summarise(n=n(),dinero=sum(as.numeric(ITV),na.rm=T)) %>%
    group_by(cluster) %>%
    mutate(p_n=n/sum(n,na.rm=T),p_dinero=dinero/sum(dinero,na.rm=T)) %>%
    arrange(cluster,desc(dinero))
  
  total <- base %>%
    filter(Depto_Desc==linea[i],Subdepto_Desc %in% sublinea[[i]]) %>%
    group_by(Proddesc) %>%
    summarise(n=n(),dinero=sum(as.numeric(ITV),na.rm=T)) %>%
    mutate(p_n=n/sum(n,na.rm=T),p_dinero=dinero/sum(dinero,na.rm=T),cluster="Total") %>%
    arrange(desc(dinero))
  
  if(linea[i]=="SERVICIOS"){
    cadenas <- articulos2 %>%
      filter(Proddesc %in% c("LINEA DE CREDITO                ",
                             "VENTAS LCR OTRAS TIENDAS        "))
    articulos3 <- articulos2 %>%
      filter(!Proddesc %in% c("LINEA DE CREDITO                ",
                              "VENTAS LCR OTRAS TIENDAS        "))
    articulos3<-servicios(articulos3)
    names(cadenas)[1]<-"descripcion_limpia"
    cadenas$descripcion<-str_trim(cadenas$descripcion_limpia)
    
    articulos3<-rbind(articulos3,cadenas)
    
    cadenas_t <- total %>%
      filter(Proddesc %in% c("LINEA DE CREDITO                ",
                             "VENTAS LCR OTRAS TIENDAS        "))
    articulos3_t <- total %>%
      filter(!Proddesc %in% c("LINEA DE CREDITO                ",
                              "VENTAS LCR OTRAS TIENDAS        "))
    articulos3_t<-servicios(articulos3_t)
    names(cadenas_t)[1]<-"descripcion_limpia"
    cadenas_t$descripcion<-str_trim(cadenas_t$descripcion_limpia)
    
    total<-rbind(articulos3_t,cadenas_t)
    
  }else{
    total<-total[,c(1,6,2:5)]
  }
  
  articulos2<-rbind(articulos3,total) 
  articulos2$cluster<-as.factor(articulos2$cluster)
  articulos2$n<-as.numeric(articulos2$n)
  articulos2<-as.data.frame(articulos2)
  
  if(linea[i]=="SERVICIOS"){
    articulos2<-as.data.frame(articulos2) %>%
      group_by(cluster,descripcion) %>%
      summarise(n=sum(n),dinero=sum(dinero)) %>%
      mutate(p_n=n/sum(n),p_dinero=dinero/sum(dinero))
    
  }else{
    articulos2 <- articulos2[articulos2$p_dinero>.01,]
    articulos2$Proddesc<-factor(articulos2$Proddesc,levels=as.character(total$Proddesc))
  }
  
  ggplot(articulos2,aes(x=descripcion,y=p_dinero)) + 
    geom_bar(stat="identity",fill="turquoise4",colour='black') +
    scale_y_continuous(labels=percent) +
    geom_text(aes(y=p_dinero +.05,
                  label=round(p_dinero*100)),
              colour="black",size=6) +
    facet_wrap(~cluster,ncol=1) +
    theme(panel.background=element_rect(fill='snow2'),
          text=element_text(size=20),
          strip.background=element_rect(fill='skyblue4'),
          axis.text.x  = element_text(angle=90, vjust=0.5),
          strip.text.x=element_text(colour="white",size=15)) +
    xlab("Clase") +
    ylab("") +
    ggtitle(paste("Gasto en clases de la sublínea ",linea[i]," vista por grupo",sep=""))
  
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
    scale_color_discrete(name="Edad")
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

