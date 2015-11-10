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
    corrplot(data_correlaciones(), method = "circle",mar=c(0,0,0,0),tl.cex = 1.6,cl.cex=1.6,tl.col="black")},
    height = 1000,
    width = 1300
  )
  
  data_grafs_barras <- reactive({
    #a <- correlaciones_2012_2015$correlations
    if(input$filtroAnio1 == "2008") {
      a <- base_a_2008
    }
    else{
      if(input$filtroAnio1 == "2008 - 2011") {
        a <- base_2008_2011
      }
      else
        a <- base_2012_2015
    }
    return(a)
  })
  
  output$personas_productos_gasto <- renderPlot({
    graf1(data_grafs_barras())},
    height = 1000,
    width = 1300
  )
  
  output$genero <- renderPlot({
    graf2(data_grafs_barras())},
    height = 1000,
    width = 1300
  )
  
  output$edo_civil <- renderPlot({
    graf3(data_grafs_barras())},
    height = 1000,
    width = 1300
  )
  
  output$edad <- renderPlot({
    graf4(data_grafs_barras())},
    height = 1000,
    width = 1300
  )
  
  output$ing_mensual <- renderPlot({
    graf5(data_grafs_barras())},
    height = 1000,
    width = 1300
  )
  
  output$edad_anio_llegada <- renderPlot({
    graf6(data_grafs_barras())},
    height = 1000,
    width = 1300
  )

  output$ped_saldados <- renderPlot({
    graf7(data_grafs_barras())},
    height = 1000,
    width = 1300
  )
  
  output$ped_activos <- renderPlot({
    graf8(data_grafs_barras())},
    height = 1000,
    width = 1300
  )
  
  output$atrasos<- renderPlot({
    graf9(data_grafs_barras())},
    height = 1000,
    width = 1300
  )
  
  output$ped_cancelados <- renderPlot({
    graf10(data_grafs_barras())},
    height = 1000,
    width = 1300
  )

  output$anios_1C <- renderPlot({
    graf11(data_grafs_barras())},
    height = 1000,
    width = 1300
  )
  
  output$anios_UC <- renderPlot({
    graf12(data_grafs_barras())},
    height = 1000,
    width = 1300
  )
  
  output$compras_anio_gpo <- renderPlot({
    graf13(data_grafs_barras())},
    height = 1000,
    width = 1300
  )
  
})