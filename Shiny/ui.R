library(shiny)
library(shinythemes)

shinyUI(navbarPage("Minería de datos Perú",theme = shinytheme("flatly"),
                   tabPanel("Gráfica de correlaciones",
                            sidebarLayout(
                              sidebarPanel(
                                menu1(),
                                width = 2),
                              mainPanel(
                                plotOutput("graf_correlaciones",height=100,width=1300)
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
                                  tabPanel("Distribución de género",
                                           column(5,plotOutput("genero",height=100,width=1300)),align="center"),
                                  tabPanel("Distribución de estado civil",
                                           column(5,plotOutput("edo_civil",height=100,width=1300)),align="center"),
                                  tabPanel("Distribución de edad",
                                           column(5,plotOutput("edad",height=100,width=1300)),align="center"),
                                  tabPanel("Distribución de ingreso mensual",
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
                                  tabPanel("Distribución de pedidos saldados",
                                           column(5,plotOutput("ped_saldados",height=100,width=1300)),align="center"),
                                  tabPanel("Distribución de número de pedidos activos",
                                           column(5,plotOutput("ped_activos",height=100,width=1300)),align="center"),
                                  tabPanel("Distribución de pedidos y atrasos",
                                           column(5,plotOutput("atrasos",height=100,width=1300)),align="center"),
                                  tabPanel("Distribución de pedidos cancelados",
                                           column(5,plotOutput("ped_cancelados",height=100,width=1300)),align="center"),
                                  tabPanel("Distribución de años desde la primera compra",
                                           column(5,plotOutput("anios_1C",height=100,width=1300)),align="center"),
                                  tabPanel("Distribución de meses desde la última compra",
                                           column(5,plotOutput("anios_UC",height=100,width=1300)),align="center"),
                                  tabPanel("Distribución de compras por semana",
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