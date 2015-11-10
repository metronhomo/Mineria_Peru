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
  
  output$personas_productos_gasto <- renderPlot({
    graf1(base,input$filtroAnio2)},
    height = 1000,
    width = 1300
  )
  
  output$genero <- renderPlot({
    graf2(base,input$filtroAnio2)},
    height = 1000,
    width = 1300
  )
  
  output$edo_civil <- renderPlot({
    graf3(base,input$filtroAnio2)},
    height = 1000,
    width = 1300
  )
  
  output$edad <- renderPlot({
    graf4(base,input$filtroAnio2)},
    height = 1000,
    width = 1300
  )
  
  output$ing_mensual <- renderPlot({
    graf5(base,input$filtroAnio2)},
    height = 1000,
    width = 1300
  )
  
  output$edad_anio_llegada <- renderPlot({
    graf6(base,input$filtroAnio2)},
    height = 1000,
    width = 1300
  )

  output$ped_saldados <- renderPlot({
    graf7(base,input$filtroAnio3)},
    height = 1000,
    width = 1300
  )
  
  output$ped_activos <- renderPlot({
    graf8(base,input$filtroAnio3)},
    height = 1000,
    width = 1300
  )
  
  output$atrasos<- renderPlot({
    graf9(base,input$filtroAnio3)},
    height = 1000,
    width = 1300
  )
  
  output$ped_cancelados <- renderPlot({
    graf10(base,input$filtroAnio3)},
    height = 1000,
    width = 1300
  )

  output$anios_1C <- renderPlot({
    graf11(base,input$filtroAnio3)},
    height = 1000,
    width = 1300
  )
  
  output$anios_UC <- renderPlot({
    graf12(base,input$filtroAnio3)},
    height = 1000,
    width = 1300
  )
  
  output$compras_anio_gpo <- renderPlot({
    graf13(base,input$filtroAnio3)},
    height = 1000,
    width = 1300
  )
  
})