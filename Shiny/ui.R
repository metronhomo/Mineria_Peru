library(shiny)
library(shinythemes)

shinyUI(navbarPage("Minería de datos Perú",theme = shinytheme("flatly"),
                   tabPanel("Análisis Previo",
                            sidebarLayout(
                              sidebarPanel(
                                conditionalPanel(
                                  condition = "input.panel1  != 'Ingreso de Personas por Edad y Año'",
                                  menu1()
                                ),
                                width = 2),
                              mainPanel(
                                tabsetPanel(id="panel1",
                                  tabPanel("Ingreso de Personas por Edad y Año",
                                           column(5,plotOutput("edad_anio",height=100,width=1300)),align="center"),
                                  tabPanel("Interacción entre variables",
                                           column(5,plotOutput("graf_correlaciones",height=100,width=1300)),align="center"),
                                  tabPanel("Análisis de Estructura",
                                    column(5, align="center",
                                           conditionalPanel(
                                             condition = "input.filtroAnio1  == '2008'",
                                             imageOutput("im1")
                                           ),
                                           conditionalPanel(
                                             condition = "input.filtroAnio1  == '2008 - 2011'",
                                             imageOutput("im2")
                                           ),
                                           conditionalPanel(
                                             condition = "input.filtroAnio1  == '2012 - 2015'",
                                             imageOutput("im3")
                                           ))
                                  )
                                )
                              )
                            )
                   ),
                   tabPanel("Composición demográfica de grupos",
                            sidebarLayout(
                              sidebarPanel(
                                  menu2(),
                                width = 2),    
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Distribución de personas, productos y gasto",
                                           column(5,plotOutput("personas_productos_gasto",height=100,width=1300)),align="center"),
                                  tabPanel("Género",
                                           column(5,plotOutput("genero",height=100,width=1300)),align="center"),
                                  tabPanel("Estado civil",
                                           column(5,plotOutput("edo_civil",height=100,width=1300)),align="center"),
                                  tabPanel("Edad",
                                           column(5,plotOutput("edad",height=100,width=1300)),align="center"),
                                  tabPanel("Ingreso mensual",
                                           column(5,plotOutput("ing_mensual",height=100,width=1300)),align="center")
                                )
                              )
                              
                            )
                   ),
                   tabPanel("Comportamiento de compra por grupo",
                            sidebarLayout(
                              sidebarPanel(
                                menu3(),
                                width = 2),    
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Pedidos saldados",
                                           column(5,plotOutput("ped_saldados",height=100,width=1300)),align="center"),
                                  tabPanel("Pedidos activos",
                                           column(5,plotOutput("ped_activos",height=100,width=1300)),align="center"),
                                  tabPanel("Atrasos",
                                           column(5,plotOutput("atrasos",height=100,width=1300)),align="center"),
                                  tabPanel("Pedidos cancelados",
                                           column(5,plotOutput("ped_cancelados",height=100,width=1300)),align="center"),
                                  tabPanel("Años desde la primera compra",
                                           column(5,plotOutput("anios_1C",height=100,width=1300)),align="center"),
                                  tabPanel("Meses desde la última compra",
                                           column(5,plotOutput("anios_UC",height=100,width=1300)),align="center"),
                                  tabPanel("Compras por semana",
                                           column(5,plotOutput("compras_anio_gpo",height=100,width=1300)),align="center")
                                )
                                
                              )
                                )
                   )
#                    tabPanel("Cruce de variables",
#                             sidebarLayout(
#                               sidebarPanel(
#                                 menu4(),
#                                 width = 2
#                               ),    
#                               mainPanel(
#                                 tabsetPanel(
#                                   tabPanel("Algo",
#                                            column(5,plotOutput("plot",height=100,width=1300)),align="center")
#                                 )
#                                 
#                               )
#                             )
#                    ),
#                    tabPanel("Pedidos",
#                             sidebarLayout(
#                               sidebarPanel(
#                                 menu5(),
#                                 width = 2
#                               ),    
#                               mainPanel(
#                                 tabsetPanel(
#                                   tabPanel("Gasto acumulado por grupo",
#                                            column(5,plotOutput("plot",height=100,width=1300)),align="center"),
#                                   tabPanel("Gasto por sublíneas por grupo",
#                                            column(5,plotOutput("plot",height=100,width=1300)),align="center")
#                                 )
#                                 
#                               )
#                             )
#                    )
                   
))