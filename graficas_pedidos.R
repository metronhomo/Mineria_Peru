library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(lubridate)

base_a_2008 <- readRDS("./Shiny/data/base_a_2008.RDS")
base_2008_2011 <- readRDS("./Shiny/data/base_2008_2011.RDS")
base_2012_2015 <- readRDS("./Shiny/data/base_2012_2015.RDS")

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


graficas<-function(i, base){
  articulos2 <- base %>%
    filter(Depto_Desc==linea[i],Subdepto_Desc %in% sublinea[[i]]) %>%
    group_by(Clase_Desc,clase) %>%
    summarise(n=n(),dinero=sum(as.numeric(ITV),na.rm=T)) %>%
    group_by(clase) %>%
    mutate(p_n=n/sum(n,na.rm=T),p_dinero=dinero/sum(dinero,na.rm=T)) %>%
    arrange(clase,desc(dinero))
  
  total <- base %>%
    filter(Depto_Desc==linea[i],Subdepto_Desc %in% sublinea[[i]]) %>%
    group_by(Clase_Desc) %>%
    summarise(n=n(),dinero=sum(as.numeric(ITV),na.rm=T)) %>%
    mutate(p_n=n/sum(n,na.rm=T),p_dinero=dinero/sum(dinero,na.rm=T),clase="General") %>%
    arrange(desc(dinero))
  
  total<-total[,c(1,6,2:5)]
  
  articulos2<-rbind(articulos2,total)
  articulos2$Clase_Desc<-factor(articulos2$Clase_Desc,levels=as.character(total$Clase_Desc))
  
  ggplot(articulos2,aes(x=Clase_Desc,y=p_dinero)) + 
    geom_bar(stat="identity",fill="#2c3e50",colour='black') +
    scale_y_continuous(labels=percent) +
    geom_text(aes(y=p_dinero +.05,
                  label=round(p_dinero*100)),
              colour="black",size=6) +
    facet_wrap(~clase,ncol=1) +
    theme_MH() +
    xlab("") +
    ylab("") 
}


graficas2<-function(i, base){
  articulos2 <- base %>%
    filter(Depto_Desc==linea[i],Subdepto_Desc %in% sublinea[[i]]) %>%
    group_by(Proddesc,clase) %>%
    summarise(n=n(),dinero=sum(as.numeric(ITV),na.rm=T)) %>%
    group_by(clase) %>%
    mutate(p_n=n/sum(n,na.rm=T),p_dinero=dinero/sum(dinero,na.rm=T)) %>%
    arrange(clase,desc(dinero))
  
  total <- base %>%
    filter(Depto_Desc==linea[i],Subdepto_Desc %in% sublinea[[i]]) %>%
    group_by(Proddesc) %>%
    summarise(n=n(),dinero=sum(as.numeric(ITV),na.rm=T)) %>%
    mutate(p_n=n/sum(n,na.rm=T),p_dinero=dinero/sum(dinero,na.rm=T),clase="General") %>%
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
  articulos2$clase<-as.factor(articulos2$clase)
  articulos2$n<-as.numeric(articulos2$n)
  articulos2<-as.data.frame(articulos2)
  
  if(linea[i]=="SERVICIOS"){
    articulos2<-as.data.frame(articulos2) %>%
      group_by(clase,descripcion) %>%
      summarise(n=sum(n),dinero=sum(dinero)) %>%
      mutate(p_n=n/sum(n),p_dinero=dinero/sum(dinero))
    
  }else{
    articulos2 <- articulos2[articulos2$p_dinero>.01,]
    articulos2$Proddesc<-factor(articulos2$Proddesc,levels=as.character(total$Proddesc))
  }
  
  ggplot(articulos2,aes(x=descripcion,y=p_dinero)) + 
    geom_bar(stat="identity",fill="#2c3e50",colour='black') +
    scale_y_continuous(labels=percent) +
    geom_text(aes(y=p_dinero +.05,
                  label=round(p_dinero*100)),
              colour="black",size=6) +
    facet_wrap(~clase,ncol=1) +
    theme_MH() +
    xlab("") +
    ylab("") 
  
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
    group_by(descripcion_limpia,clase) %>%
    summarise(n=sum(n),dinero=sum(dinero)) %>%
    group_by(clase) %>%
    mutate(p_n=n/sum(n,na.rm=T),p_dinero=dinero/sum(dinero,na.rm=T)) %>%
    mutate(descripcion=cut(descripcion_limpia,
                           breaks=c(-Inf,0,3000,4000,5000,6000,
                                    7000,8000,9000,10000,14000,Inf),
                           labels=c("Cero","1-3000","3001-4000",
                                    "4001-5000","5001-6000","6001-7000",
                                    "7001-8000","8001-9000","9001-10000",
                                    "10001-14000","Más de 14000")))%>%
    arrange(clase,desc(dinero))
  return(df2)
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

# 
# crea_lista_graficas <- function(base) {
# 
#   linea<-c("ELECTRONICA","LINEA BLANCA", "MUEBLES","SEGUROS AZTECA")
#   
#   sublinea<-list(c("TELEVISIONES","AUDIO","VIDEO"),c("ELECTRODOMESTICOS","ESTUFAS Y MICROONDAS",
#                                                      "REFRIGERADORES","LAVADORAS Y SECADORAS"),
#                  c("BLANCOS","ESTANCIA","SALAS RECAMARAS Y TAPIZADOS","COMEDORES Y ANTECOMEDORES"),
#                  "SEGUROS DE VIDA")
#   
#   grafs_sublinea <- lapply(1:4,function(i) graficas(i, base))
#   
#   
#   linea<-c("SERVICIOS")
#   sublinea<-c("PRESTAMOS PERSONALES")
#   grafs_sublinea <- append(grafs_sublinea, lapply(1,function(i) graficas2(i, base)))
#   return(grafs_sublinea)
# }

# #La función no jala. Tuve que hacerlo uno por uno
# grafs_pedidos_sublinea_a_2008 <- crea_lista_graficas(base_a_2008)
# grafs_pedidos_sublinea_2008_2011 <- crea_lista_graficas(base_2008_2011)
# grafs_pedidos_sublinea_2012_2015 <- crea_lista_graficas(base_2012_2015)

base <- base_a_2008
linea<-c("ELECTRONICA","LINEA BLANCA", "MUEBLES","SEGUROS AZTECA")
sublinea<-list(c("TELEVISIONES","AUDIO","VIDEO"),c("ELECTRODOMESTICOS","ESTUFAS Y MICROONDAS",
                                                   "REFRIGERADORES","LAVADORAS Y SECADORAS"),
               c("BLANCOS","ESTANCIA","SALAS RECAMARAS Y TAPIZADOS","COMEDORES Y ANTECOMEDORES"),
               "SEGUROS DE VIDA")
grafs_sublinea <- lapply(1:4,function(i) graficas(i, base))
linea<-c("SERVICIOS")
sublinea<-c("PRESTAMOS PERSONALES")
grafs_sublinea <- append(grafs_sublinea, lapply(1,function(i) graficas2(i, base)))
names(grafs_sublinea) <- c(
  "ELECTRONICA",
  "LINEA BLANCA",
  "MUEBLES",
  "SEGUROS AZTECA",
  "SERVICIOS"
)
grafs_pedidos_sublinea_a_2008 <- grafs_sublinea



base <- base_2008_2011
linea<-c("ELECTRONICA","LINEA BLANCA", "MUEBLES","SEGUROS AZTECA")
sublinea<-list(c("TELEVISIONES","AUDIO","VIDEO"),c("ELECTRODOMESTICOS","ESTUFAS Y MICROONDAS",
                                                   "REFRIGERADORES","LAVADORAS Y SECADORAS"),
               c("BLANCOS","ESTANCIA","SALAS RECAMARAS Y TAPIZADOS","COMEDORES Y ANTECOMEDORES"),
               "SEGUROS DE VIDA")
grafs_sublinea <- lapply(1:4,function(i) graficas(i, base))
linea<-c("SERVICIOS")
sublinea<-c("PRESTAMOS PERSONALES")
grafs_sublinea <- append(grafs_sublinea, lapply(1,function(i) graficas2(i, base)))
names(grafs_sublinea) <- c(
  "ELECTRONICA",
  "LINEA BLANCA",
  "MUEBLES",
  "SEGUROS AZTECA",
  "SERVICIOS"
)
grafs_pedidos_sublinea_2008_2011 <- grafs_sublinea





base <- base_2012_2015
linea<-c("ELECTRONICA","LINEA BLANCA", "MUEBLES","SEGUROS AZTECA")
sublinea<-list(c("TELEVISIONES","AUDIO","VIDEO"),c("ELECTRODOMESTICOS","ESTUFAS Y MICROONDAS",
                                                   "REFRIGERADORES","LAVADORAS Y SECADORAS"),
               c("BLANCOS","ESTANCIA","SALAS RECAMARAS Y TAPIZADOS","COMEDORES Y ANTECOMEDORES"),
               "SEGUROS DE VIDA")
grafs_sublinea <- lapply(1:4,function(i) graficas(i, base))
linea<-c("SERVICIOS")
sublinea<-c("PRESTAMOS PERSONALES")
grafs_sublinea <- append(grafs_sublinea, lapply(1,function(i) graficas2(i, base)))
names(grafs_sublinea) <- c(
  "ELECTRONICA",
  "LINEA BLANCA",
  "MUEBLES",
  "SEGUROS AZTECA",
  "SERVICIOS"
)
grafs_pedidos_sublinea_2012_2015 <- grafs_sublinea




saveRDS(grafs_pedidos_sublinea_a_2008, "./Shiny/data/grafs_pedidos_sublinea_a_2008.RDS")
saveRDS(grafs_pedidos_sublinea_2008_2011, "./Shiny/data/grafs_pedidos_sublinea_2008_2011.RDS")
saveRDS(grafs_pedidos_sublinea_2012_2015, "./Shiny/data/grafs_pedidos_sublinea_2012_2015.RDS")
