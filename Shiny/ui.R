library(shiny)
library(shinythemes)

shinyUI(navbarPage("Minería de datos Perú",theme = shinytheme("flatly"),
                   tabPanel("Composición de grupos",
                            sidebarLayout(
                              sidebarPanel(
                                menu(),
                                width = 2
                              ),    
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Porcentaje de personas en cada grupo",
                                           column(5,plotOutput("plot",height=100,width=1300)),align="center"),
                                  tabPanel("Distribución de personas, productos y gasto",
                                           column(5,plotOutput("plot",height=100,width=1300)),align="center"),
                                  tabPanel("Composición de género",
                                           column(5,plotOutput("plot",height=100,width=1300)),align="center"),
                                  tabPanel("Composición de estado civil",
                                           column(5,plotOutput("plot",height=100,width=1300)),align="center")
                                )
                              )
                              
                            )
                   )
                   
))