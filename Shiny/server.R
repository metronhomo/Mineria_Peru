# server.R
library(shiny)


shinyServer(function(input, output,session){
  
  data_correlaciones <- reactive({
    #a <- correlaciones_2012_2015$correlations
    if(input$filtroAnio1 == "2008") {
      a <- correlaciones_a_2008$correlations
    }
    else{
      if(input$filtroAnio1 == "2008 - 2011") {
        a <- correlaciones_2008_2011$correlations
      }
      else
        a <- correlaciones_2012_2015$correlations
    }
    return(a)
  })
  
  output$graf_correlaciones <- renderPlot({
    data <- data_correlaciones()

    nombres <- data %>% row.names()
    idx <- match(nombres, nombres_feos)
    row.names(data) <- nombres_bonitos[idx]
    colnames(data) <- nombres_bonitos[idx]
    
    corrplot(data, method = "circle",mar=c(0,0,0,0),tl.cex = 1.6,cl.cex=1.6,tl.col="black")},
    height = 1000,
    width = 1300
  )
  
  data_grafs_barras2 <- reactive({
    #a <- correlaciones_2012_2015$correlations
    if(input$filtroAnio2 == "2008") {
      a <- base_a_2008
    }
    else{
      if(input$filtroAnio2 == "2008 - 2011") {
        a <- base_2008_2011
      }
      else
        a <- base_2012_2015
    }
    return(a)
  })
  
  output$edad_anio <- renderPlot({
    graf6(caracterizacion)},
    height = 1000,
    width = 1300
  )
  
  output$personas_productos_gasto <- renderPlot({
    graf1(data_grafs_barras2())},
    height = 1000,
    width = 1300
  )
  
  output$genero <- renderPlot({
    graf2(data_grafs_barras2())},
    height = 1000,
    width = 1300
  )
  
  output$edo_civil <- renderPlot({
    graf3(data_grafs_barras2())},
    height = 1000,
    width = 1300
  )
  
  output$edad <- renderPlot({
    graf4(data_grafs_barras2())},
    height = 1000,
    width = 1300
  )
  
  output$ing_mensual <- renderPlot({
    graf5(data_grafs_barras2())},
    height = 1000,
    width = 1300
  )
  
  data_grafs_barras3 <- reactive({
    #a <- correlaciones_2012_2015$correlations
    if(input$filtroAnio3 == "2008") {
      a <- base_a_2008
    }
    else{
      if(input$filtroAnio3 == "2008 - 2011") {
        a <- base_2008_2011
      }
      else
        a <- base_2012_2015
    }
    return(a)
  })

  output$ped_saldados <- renderPlot({
    graf7(data_grafs_barras3())},
    height = 1000,
    width = 1300
  )
  
  output$ped_activos <- renderPlot({
    graf8(data_grafs_barras3())},
    height = 1000,
    width = 1300
  )
  
  output$atrasos<- renderPlot({
    graf9(data_grafs_barras3())},
    height = 1000,
    width = 1300
  )
  
  output$ped_cancelados <- renderPlot({
    graf10(data_grafs_barras3())},
    height = 1000,
    width = 1300
  )

  output$anios_1C <- renderPlot({
    graf11(data_grafs_barras3())},
    height = 1000,
    width = 1300
  )
  
  output$anios_UC <- renderPlot({
    graf12(data_grafs_barras3())},
    height = 1000,
    width = 1300
  )
  
  output$compras_anio_gpo <- renderPlot({
    graf13(data_grafs_barras3())},
    height = 1000,
    width = 1300
  )
  
  output$im1 <- renderImage({
    list(src="images/i1.png",
         filetype='image/png',
         width=1300,
         height=1000,
         alt='wiiiii')
  }, 
  deleteFile = F)
  
  output$im2 <- renderImage({
    list(src="images/i2.png",
         filetype='image/png',
         width=1300,
         height=1000,
         alt='wiiiii')
  }, 
  deleteFile = F)
  
  output$im3 <- renderImage({
    list(src="images/i3.png",
         filetype='image/png',
         width=1300,
         height=1000,
         alt='wiiiii')
  }, 
  deleteFile = F)
  
  data_cruces <- reactive({
    if(input$filtroAnio4 == "2008") {
      a <- base_a_2008
    }
    else{
      if(input$filtroAnio4 == "2008 - 2011") {
        a <- base_2008_2011
      }
      else
        a <- base_2012_2015
    }
    return(a)
  })
  
  
  output$graf_cruces <- renderPlot({
    data <- data_cruces()
    if(input$var_cruce_1 != input$var_cruce_2)  {
      data1 <- data %>%
        group_by_("Folio", "Pais", "Canal", "Tienda_Registro", "clase", input$var_cruce_1, input$var_cruce_2) %>%
        tally() %>%
        group_by_("clase", input$var_cruce_1, input$var_cruce_2) %>%
        tally() %>%
        dplyr::mutate(porc = n/sum(n)) %>% 
        select(-n)
      
      data2 <- data %>%
        group_by_("Folio", "Pais", "Canal", "Tienda_Registro", input$var_cruce_1, input$var_cruce_2) %>%
        tally() %>%
        group_by_(input$var_cruce_1, input$var_cruce_2) %>%
        tally() %>%
        dplyr::mutate(porc = n/sum(n)) %>% 
        select(-n)
      data2$clase <- "General"
      data2 <- data2[,c(4,1,2,3)]
      
      names(data1) <- c("clase", "Var1", "Var2", "porc")
      names(data2) <- c("clase", "Var1", "Var2", "porc")
      data3 <- rbind(data1, data2)
      data3 %>%
        ggplot(aes(x=Var1)) + 
        geom_bar(aes(x = Var1, y = porc), stat = 'identity', fill="#2c3e50", colour="black") + 
        geom_text(aes(y = porc + .03, label = round(porc*100)), colour='black', size=6) +
        facet_grid(Var2~clase) +
        theme_MH() +
        scale_y_continuous(labels=percent)  +
        ylab("") +
        xlab("")
    }
    else { 
      var <- input$var_cruce_1
      x<-as.data.frame(prop.table(table(data[[var]], data$clase),2))
      y<-as.data.frame(prop.table(table(data[[var]])))
      y$Var2<-"General"
      x<-rbind(x,y[,c(1,3,2)])
      names(x)<-c("Var","clase","porc")
      
#       graf_barras(df=x,var_x="Sexo",var_y="Porc",facet=T,var_facet="clase",
#                   angulo=90,x_lab="Sexo")    
      #data %>%
      x %>%
        ggplot(aes(x=Var)) + 
        geom_bar(aes(x = Var, y = porc), stat = 'identity', fill="#2c3e50", colour="black") + 
        geom_text(aes(y = porc + .03, label = round(porc*100)), colour='black', size=6) +
        facet_grid(~clase) +
        theme_MH() +
        scale_y_continuous(labels=percent)  +
        ylab("") +
        xlab("")
    }
    
  },    
  height = 1000,
  width = 1300)
  
  
#   data_pedidos <- reactive({
#     if(input$filtroAnio5 == "2008") {
#       a <- base_a_2008
#     }
#     else{
#       if(input$filtroAnio5 == "2008 - 2011") {
#         a <- base_2008_2011
#       }
#       else
#         a <- base_2012_2015
#     }
#     return(a)
#   })
  
  data_pedidos <- reactive({
    input$filtroAnio5
  })
  
  output$grafs_gasto_acumulado <- renderPlot({
    if(data_pedidos() == "2008") {
      base <- base_a_2008
    }
    else{
      if(data_pedidos() == "2008 - 2011") {
        base <- base_2008_2011
      }
      else
        base <- base_2012_2015
    }
    
    # base <- data_pedidos()
    
    gasto <- base %>%
      group_by(clase,Depto_Desc) %>%
      summarise(total=sum(as.numeric(ITV),na.rm=T)) %>%
      mutate(porcentaje=total/sum(total,na.rm=T))
    
    gasto_t<-base%>%
      group_by(Depto_Desc)%>%
      summarise(total=sum(as.numeric(ITV),na.rm=T))%>%
      mutate(porcentaje=total/sum(total,na.rm=T))
    
    gasto_t$clase<-"General"
    
    #unimos todos los dataframes
    gasto_lineas<-rbind(gasto,gasto_t)
    gasto_lineas$Depto_Desc<-as.character(gasto_lineas$Depto_Desc)
    aux<-gasto_t%>%
      arrange(desc(total))
    orden<-unique(aux$Depto_Desc)
    
    gasto_lineas$Depto_Desc<-factor(gasto_lineas$Depto_Desc,levels=orden)
    
    graf_barras(df=gasto_lineas,var_x="Depto_Desc",var_y="porcentaje",titulo="Gasto acumulado por grupo",
                facet=T, var_facet="clase", angulo=90,x_lab="Pedidos",n=1)
    
    },
    height = 1000,
    width = 1300
  )
  
  input_linea <- reactive({
    input$filtroLinea
  })
  
  tipo_graf <- reactive({
    input$total_sublinea
  })
  
  output$grafs_gasto_lineas <- renderPlot({
    tipo <- tipo_graf()
    if(tipo == "Total"){
      if(data_pedidos() == "2008") {
        base <- base_a_2008
      }
      else{
        if(data_pedidos() == "2008 - 2011") {
          base <- base_2008_2011
        }
        else
          base <- base_2012_2015
      }
      
      # base <- data_pedidos()
      depto <- input_linea()
      
      slineas_compu_t<-base%>%
        filter(Depto_Desc == depto)%>%
        group_by(Subdepto_Desc)%>%
        summarise(total=sum(as.numeric(ITV),na.rm=T))%>%
        mutate(porcentaje=total/sum(total,na.rm=T))
      
      slineas_compu_t$clase<-"General"
      
      slineas_compu<-base%>%
        filter(Depto_Desc == depto)%>%
        group_by(clase, Subdepto_Desc)%>%
        summarise(total = sum(as.numeric(ITV),na.rm=T))%>%
        mutate(porcentaje = total/sum(total,na.rm=T))
      
      slineas_compu<-rbind(slineas_compu, slineas_compu_t)
      slineas_compu$Subdepto_Desc<-as.character(slineas_compu$Subdepto_Desc)
      
      aux<-slineas_compu_t%>%
        arrange(desc(total))
      orden<-unique(aux$Subdepto_Desc)
      
      slineas_compu$Subdepto_Desc<-factor(slineas_compu$Subdepto_Desc,levels=orden)
      
      graf_barras(df=slineas_compu,var_x="Subdepto_Desc",var_y="porcentaje",
                  titulo = paste("Gasto por sublíneas de la línea: ", depto),
                  facet=T, var_facet="clase", angulo=90, x_lab = paste("Sublíneas de la línea ", depto),n=1)
    }
    else {
      
    }
    
  },
  height = 1000,
  width = 1300
  )
  
})