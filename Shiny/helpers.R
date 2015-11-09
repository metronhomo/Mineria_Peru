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
    helpText(h4('Selecciona los años que quieres ver.')),
    selectInput(
      'var_cruce_1', 
      label = '',
      choices = list(
        'Estado Civil'),
      selected = c('Estado Civil')),
    helpText(h4('Selecciona los años que quieres ver.')),
    selectInput(
      'var_cruce_2', 
      label = '',
      choices = list(
        'Estado Civil'),
      selected = c('Estado Civil'))
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
        '2008' = '2008',
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
      geom_bar(stat="identity",fill="turquoise4",colour="black") +
      geom_text(aes(y=var_y + .03,label=round(var_y*100)),colour='black',size=6) +
      theme(axis.text.x  = element_text(angle=angulo),
            text=element_text(size=20),
            panel.background=element_rect(fill='snow2'),
            strip.background=element_rect(fill='skyblue4'),
            strip.text.x=element_text(colour="white",size=15)) +
      scale_y_continuous(labels=label) +
      xlab(x_lab) +
      ylab(y_lab) +
      ggtitle(titulo)
  }else{    
    df<- df %>%
      dplyr::select(one_of(var_x,var_y,var_facet))
    names(df)<-c("var_x","var_y","var_facet")
    
    ggplot(df,aes(x=var_x,y=var_y)) +
      geom_bar(stat="identity",fill="turquoise4",colour="black") +
      geom_text(aes(y=var_y + .03,label=round(var_y*100)),colour='black',size=6) +
      theme(axis.text.x  = element_text(angle=angulo),
            text=element_text(size=20),
            panel.background=element_rect(fill='snow2'),
            strip.background=element_rect(fill='skyblue4'),
            strip.text.x=element_text(colour="white",size=15)) +
      facet_wrap(~var_facet,ncol=n) +
      scale_y_continuous(labels=label) +
      xlab(x_lab) +
      ylab(y_lab) + 
      ggtitle(titulo)
  }
}

graf_densidad<-function(df,var_x,facet=F,var_facet="",x_lim_min,n=1,
                        x_lim_max, salto, angulo=0,label=comma,x_lab="",y_lab="",titulo=""){  
  if(!facet){
    
    df<- df %>%
      dplyr::select(one_of(var_x))
    names(df)<-c("var_x")
    
    ggplot(df,aes(x=var_x)) +
      geom_density(fill="turquoise4",colour="black") +
      theme(panel.background=element_rect(fill='snow2'),
            text=element_text(size=20),
            strip.background=element_rect(fill='skyblue4'),
            strip.text.x=element_text(colour="white",size=15)) +
      scale_y_continuous(labels=label) +
      scale_x_continuous(breaks=seq(x_lim_min,x_lim_max,by=salto),limits=c(x_lim_min,x_lim_max)) +
      xlab(x_lab) + 
      ylab(y_lab) +
      ggtitle(titulo)
  }else{    
    df<- df %>%
      dplyr::select(one_of(var_x,var_facet))
    names(df)<-c("var_x","var_facet")
    
    ggplot(df,aes(x=var_x)) +
      geom_density(fill="turquoise4",colour="black") +
      theme(panel.background=element_rect(fill='snow2'),
            text=element_text(size=20),
            strip.background=element_rect(fill='skyblue4'),
            strip.text.x=element_text(colour="white",size=15)) +
      scale_y_continuous(labels=label) +
      scale_x_continuous(breaks=seq(x_lim_min,x_lim_max,by=salto),limits=c(x_lim_min,x_lim_max)) +
      facet_wrap(~var_facet,ncol=n) +
      xlab(x_lab) + 
      ylab(y_lab) +
      ggtitle(titulo)
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

filtro <- function(base,filtro){
  if (filtro == '2008'){
    df <-base %>%
      mutate(anio_llegada=as.integer(as.character(anio_llegada))) %>%
      filter(anio_llegada<2008)
  }else{
    if (filtro == '2008 - 2011'){
      df <-base %>%
        mutate(anio_llegada=as.integer(as.character(anio_llegada))) %>%
        filter(anio_llegada>=2008 & anio_llegada<=2011)
    }else{
      df <-base %>%
        mutate(anio_llegada=as.integer(as.character(anio_llegada))) %>%
        filter(anio_llegada>2011)
      }
  }
  return(df)
}

graf1<-function(base,filtro){
  base <- filtro(base,filtro)
  
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
  
  grafca<-grid.arrange(people,products,money,ncol=1)
  
  return(grafica)
}

