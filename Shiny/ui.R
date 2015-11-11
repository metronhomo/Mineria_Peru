library(shiny)
library(shinythemes)

shinyUI(navbarPage("Minería de datos Perú",theme = shinytheme("flatly"),
                   tabPanel("Análisis Previo",
                            sidebarLayout(
                              sidebarPanel(
                                conditionalPanel(
                                  condition = "input.panel1  == 'Ingreso de Personas por Edad y Año'",
                                  fluidRow(wellPanel(
                                          style = "background-color: #2c3e50;",
                                          h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                          br(),
                                          h4("En esta gráfica podemos ver cómo ha ido cambiando la constitución de edad entre los clientes
                                            del grupo con el paso de los años.",style = "color:#C2D1E0"),
                                          br(),
                                          h4(" En particular, vemos que en los primeros años había un mayor número de personas entre los 
                                            26 y 45 años; sin embargo, con el paso del tiempo se fue atrayendo un mayor número de jóvenes.",
                                            style = "color:#C2D1E0"),
                                          br(),
                                          h4("A partir del 2012, la mayor parte de las personas que realizan transacciones se encuentran entre
                                            los 18 y los 35 años, por lo que hay que tener especial cuidado en no permitir sobre endeudamiento,
                                            pues probablemente .",
                                            style = "color:#C2D1E0")
                                          )
                                      )
                                ),
                                conditionalPanel(
                                  condition = "input.panel1  == 'Interacción entre variables'",
                                    fluidRow(
                                      wellPanel(
                                              menu1()
                                            )),
                                    fluidRow(
                                      wellPanel(
                                              style = "background-color: #2c3e50;",
                                              h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                              br(),
                                              h4("Aquí podemos ver cómo interactuan las variables entres sí.",style = "color:#C2D1E0"),
                                              br(),
                                              h4("Entre más grandes y obscuros sean los círculos mayor es la interacción entre las 
                                                  variables que estamos observando. Círculos azules indican interacción
                                                  positiva, mientras que círculos rojos indican interacciones negativas.",
                                                  style = "color:#C2D1E0")
                                                 )
                                             )
                                  ),
                                conditionalPanel(
                                  condition = "input.panel1  == 'Análisis de Estructura'",
                                  fluidRow(
                                    wellPanel(
                                      menu_im()
                                    )),
                                  fluidRow(
                                    wellPanel(
                                      style = "background-color: #2c3e50;",
                                      h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                      br(),
                                      h4("Aquí podemos ahondar en la relación que existe entre las variables.",style = "color:#C2D1E0"),
                                      br(),
                                      h4("En estas gráficas podemos ver las dependecias condicionales que existen. Es decir, si elegimos una
                                          variable de la gráfica somos capaces de hacer inferencias sobre cuáles son las variables que provocan
                                          cambios en dicha variable.",style = "color:#C2D1E0"),
                                      br(),
                                      h4("Por ejemplo, si elegimos la variable edad vemos que las únicas variables que 
                                          ENTRAN son Género y Estado Civil, por lo que si llegara una persona nueva solo necesitamos conocer su 
                                         género y estado civil para dar una inferencia sobre su edad",
                                         style = "color:#C2D1E0")
                                    )
                                    )
                                ),
                                width = 2),
                              mainPanel(
                                tabsetPanel(id="panel1",
                                  tabPanel("Ingreso de Personas por Edad y Año",
                                           column(5,plotOutput("edad_anio",height=100,width=1300)),align="center"),
                                  tabPanel("Interacción entre variables",
                                           column(5,plotOutput("graf_correlaciones",height=100,width=1300)),align="center"),
                                  tabPanel("Análisis de Estructura",
                                           column(5,imageOutput("imagen")),align="center")
                                  )
                                )
                              )
                   ),
                   tabPanel("Composición demográfica de grupos",
                            sidebarLayout(
                              sidebarPanel(
                                menu2(),
                                conditionalPanel(
                                  condition = "input.panel2  == 'Distribución de personas, productos y gasto'",
                                  fluidRow(
                                    wellPanel(
                                      style = "background-color: #2c3e50;",
                                      h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                      br(),
                                      h4("En la primer gráfica miramos la cantidad de personas que hay en cada grupo.",style = "color:#C2D1E0"),
                                      br(),
                                      h4("En la segunda gráfica miramos el porcentaje de productos adquiridos por grupo.", style = "color:#C2D1E0"),
                                      br(),
                                      h4("En la tercer gráfica miramos el porcentaje de gasto que cada grupo realizó.", style = "color:#C2D1E0")
                                    )
                                    )
                                  ),
                                conditionalPanel(
                                  condition = "input.panel2  == 'Género'",
                                  fluidRow(
                                    wellPanel(
                                      style = "background-color: #2c3e50;",
                                      h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                      br(),
                                      h4("En esta gráfica miramos la distribución del género en cada uno de los grupos y al final tenemos la 
                                         distribución a total.",style = "color:#C2D1E0")
                                    )
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.panel2  == 'Estado civil'",
                                  fluidRow(
                                    wellPanel(
                                      style = "background-color: #2c3e50;",
                                      h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                      br(),
                                      h4("En esta gráfica miramos la distribución del estado civil en cada uno de los grupos y al final tenemos la 
                                         distribución a total.",style = "color:#C2D1E0")
                                      )
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.panel2  == 'Edad'",
                                  fluidRow(
                                    wellPanel(
                                      style = "background-color: #2c3e50;",
                                      h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                      br(),
                                      h4("En esta gráfica miramos la distribución de la edad en cada uno de los grupos y al final tenemos la 
                                         distribución a total.",style = "color:#C2D1E0")
                                      )
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.panel2  == 'Ingreso mensual'",
                                  fluidRow(
                                    wellPanel(
                                      style = "background-color: #2c3e50;",
                                      h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                      br(),
                                      h4("En esta gráfica miramos la distribución del ingreso mensual en cada uno de los grupos y al final tenemos la 
                                         distribución a total.",style = "color:#C2D1E0")
                                      )
                                  )
                                ),
                                width = 2),    
                              mainPanel(
                                tabsetPanel(id="panel2",
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
                                conditionalPanel(
                                  condition = "input.panel3  == 'Pedidos saldados'",
                                  fluidRow(
                                    wellPanel(
                                      style = "background-color: #2c3e50;",
                                      h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                      br(),
                                      h4("En esta gráfica miramos la distribución del número de pedidos saldados en cada uno de los grupos y al final tenemos la 
                                         distribución a total.",style = "color:#C2D1E0")
                                      )
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.panel3  == 'Pedidos activos'",
                                  fluidRow(
                                    wellPanel(
                                      style = "background-color: #2c3e50;",
                                      h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                      br(),
                                      h4("En esta gráfica miramos la distribución del número de pedidos activos en cada uno de los grupos y al final tenemos la 
                                         distribución a total.",style = "color:#C2D1E0")
                                      )
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.panel3  == 'Atrasos'",
                                  fluidRow(
                                    wellPanel(
                                      style = "background-color: #2c3e50;",
                                      h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                      br(),
                                      h4("En esta gráfica miramos la distribución del número de atrasos en cada uno de los grupos y al final tenemos la 
                                         distribución a total.",style = "color:#C2D1E0")
                                      )
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.panel3  == 'Pedidos cancelados'",
                                  fluidRow(
                                    wellPanel(
                                      style = "background-color: #2c3e50;",
                                      h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                      br(),
                                      h4("En esta gráfica miramos la distribución del número de pedidos cancelados en cada uno de los grupos y al final tenemos la 
                                         distribución a total.",style = "color:#C2D1E0")
                                      )
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.panel3  == 'Años desde la primer compra'",
                                  fluidRow(
                                    wellPanel(
                                      style = "background-color: #2c3e50;",
                                      h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                      br(),
                                      h4("En esta gráfica miramos la distribución del número de años desde que los clientes realizaron
                                          su primer compra en cada uno de los grupos y al final tenemos la 
                                         distribución a total.",style = "color:#C2D1E0")
                                      )
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.panel3  == 'Meses desde la última compra'",
                                  fluidRow(
                                    wellPanel(
                                      style = "background-color: #2c3e50;",
                                      h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                      br(),
                                      h4("En esta gráfica miramos la distribución del número de meses desde la última compra en cada uno de los grupos y
                                        al final tenemos la distribución a total.",style = "color:#C2D1E0")
                                    )
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.panel3  == 'Compras por semana'",
                                  fluidRow(
                                    wellPanel(
                                      style = "background-color: #2c3e50;",
                                      h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                      br(),
                                      h4("En esta gráfica miramos la distribución de las compras por semana a lo largo de los años 
                                          para cada uno de los grupos en los bloques de año pertinentes.",style = "color:#C2D1E0")
                                      )
                                    )
                                ),
                                width = 2),    
                              mainPanel(
                                tabsetPanel(id="panel3",
                                  tabPanel("Pedidos saldados",
                                           column(5,plotOutput("ped_saldados",height=100,width=1300)),align="center"),
                                  tabPanel("Pedidos activos",
                                           column(5,plotOutput("ped_activos",height=100,width=1300)),align="center"),
                                  tabPanel("Atrasos",
                                           column(5,plotOutput("atrasos",height=100,width=1300)),align="center"),
                                  tabPanel("Pedidos cancelados",
                                           column(5,plotOutput("ped_cancelados",height=100,width=1300)),align="center"),
                                  tabPanel("Años desde la primer compra",
                                           column(5,plotOutput("anios_1C",height=100,width=1300)),align="center"),
                                  tabPanel("Meses desde la última compra",
                                           column(5,plotOutput("anios_UC",height=100,width=1300)),align="center"),
                                  tabPanel("Compras por semana",
                                           column(5,plotOutput("compras_anio_gpo",height=100,width=1300)),align="center")
                                )
                                
                              )
                                )
                   )
))