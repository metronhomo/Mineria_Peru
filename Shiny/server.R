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
  
  output$personas <- renderPlot({
    graf1(base,input$filtroAnio1)},
    height = 1000,
    width = 1300
  )
  
  data_cruces <- reactive({
    
    Base_caracterizacion_grupos %>%
      filtro2(., input$filtroAnio4) %>%
      group_by_("Folio", "Pais", "Canal", "Tienda_Registro", "clase", input$var_cruce_1, input$var_cruce_2) %>%
      tally() %>%
      group_by_("clase", input$var_cruce_1, input$var_cruce_2) %>%
      tally() %>%
      dplyr::mutate(porc = n/sum(n)) %>% 
      select(-n)
    
  })
  
  
  output$graf_cruces <- renderPlot({
    data <- data_cruces()
    if(ncol(data) == 4)  {
      names(data) <- c("clase", "Var1", "Var2", "porc")
      data %>%
        ggplot() + 
        geom_bar(aes(x = Var1, y = porc), stat = 'identity') + 
        facet_grid(Var2~clase) +
        theme_MH()
    }
    else { names(data) <- c("clase", "Var", "porc")
    data %>%
      ggplot() + 
      geom_bar(aes(x = Var, y = porc), stat = 'identity') + 
      facet_grid(~clase) +
      theme_MH()
    }
    
  },    
  height = 1000,
  width = 1300)
  
})