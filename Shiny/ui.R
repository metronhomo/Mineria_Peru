library(shiny)
library(shinythemes)

shinyUI(navbarPage("",theme = shinytheme("flatly"),
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
                   tabPanel("Grupos",
                            sidebarLayout(
                              sidebarPanel(
                                fluidRow(
                                  wellPanel(
                                    menu_g()
                                  )),
                                  fluidRow(wellPanel(
                                    style = "background-color: #2c3e50;",
                                    h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                    br(),
                                    h4("En esta gráfica podemos ver la composición de cada uno de los grupos en cada una de las variables
                                       que utilizamos para crear a los mismos.",style = "color:#C2D1E0"),
                                    br(),
                                    h4(" En la parte de arriba de cada gráfica podemos ver la proporción de personas que se encuentra en ese grupo;
                                        cada una de las variables ocupadas son categorías; la altura de la barra indica la proporción de personas
                                        que se encuentra en esa categoría.",
                                       style = "color:#C2D1E0"),
                                    br(),
                                    h4("En el primer botón de descarga que se encuentra abajo puedes obtener el layout de las variables ocupadas así como 
                                       las categorías que conforman a dichas variables. En el segundo, puedes descargar las personas que conforman el intervalo de tiempo que estás viendo; la base contiene los identificadores de cliente, 
                                       el grupo al que pertenecen y el primer año en el que realizaron su compra",
                                       style = "color:#C2D1E0"),
                                    h3("Descarga el layout",style = "color:#C2D1E0"),
                                    br(),
                                    downloadButton('layout','Layout'),
                                    h3("Descarga la base",style = "color:#C2D1E0"),
                                    br(),
                                    downloadButton('archivodescarga','Base')
                                    )),
                                width = 2),
                              mainPanel(column(5,imageOutput("imagen_grupos")))
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
                   ),
                   tabPanel("Cruce de variables",
                           sidebarLayout(
                             sidebarPanel(
                               menu4(),
                               fluidRow(
                                 wellPanel(
                                   style = "background-color: #2c3e50;",
                                   h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                   br(),
                                   h4("Selecciona el periodo de tiempo sobre el cuál quieres mirar el cruce de
                                      variables.",style = "color:#C2D1E0"),
                                   br(),
                                   h4("Después selecciona dos variables de los selectores de arriba para ver cómo 
                                      interactúan entre sí en cada uno de los distintos grupos que hay y 
                                      a total.",style = "color:#C2D1E0")
                                   )
                                   ),
                               width = 2
                             ),    
                             column(5,plotOutput("graf_cruces",height=100,width=1300))
                             
                           )
                   ),
                   tabPanel("Pedidos",
                            sidebarLayout(
                              sidebarPanel(
                                menu5(),
                                conditionalPanel(
                                  condition = "input.panel5  == 'Gasto acumulado por grupo'",
                                  #menu5(),
                                  fluidRow(
                                    wellPanel(
                                      style = "background-color: #2c3e50;",
                                      h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                      br(),
                                      h4("En esta gráfica puedes mirar cómo fue la distribución por sublíneas en cada 
                                         uno de los grupos y en cada uno de los periodos de tiempo elegidos.",style = "color:#C2D1E0")
                                      )
                                  )
                                ),
                                conditionalPanel(
                                  condition = "input.panel5  == 'Gasto por líneas'",
                                  #menu5(),
                                  submenu_5_linea(),
                                  fluidRow(
                                    wellPanel(
                                      style = "background-color: #2c3e50;",
                                      h3('¿Qué significa?',align="center",style = "color:#C2D1E0"),
                                      br(),
                                      h4("Selecciona el periodo de tiempo sobre el cuál quieres mirar la gráfica.",
                                         style = "color:#C2D1E0"),
                                      br(),
                                      h4("Después selecciona la línea con la que quieres que se haga la gráfica.
                                         ",style = "color:#C2D1E0"),
                                      br(),
                                      h4("Utilizando el botón de sublínea puedes elegir que la gráfica 
                                         muestre todas las sublíneas de la línea que elegiste; si por el contrario eliges el 
                                         botón de clase en la gráfica se muestran las clases de las sublíneas más populares de 
                                         la línea que elegiste.",style = "color:#C2D1E0")
                                      )
                                    )
                                  ),
                                conditionalPanel(
                                  condition = "input.panel5  == 'Gasto por sublíneas'",
                                  #menu5(),
                                  helpText("bla bla sublíneas")
                                ),
                                width = 2),
                              mainPanel(
                                tabsetPanel(
                                  id="panel5",
                                  tabPanel("Gasto acumulado por grupo",
                                           column(5,plotOutput("grafs_gasto_acumulado",height=100,width=1300)),align="center"),
                                  tabPanel("Gasto por líneas",
                                           column(5,plotOutput("grafs_gasto_lineas",height=100,width=1300)),align="center")
                                )
                              )
                              )
                   ),
                   tabPanel("Conclusiones",
                            fluidPage(
                              column(12,wellPanel(
                                style = "background-color: #2c3e50;",
                                h2(strong('Conclusiones Generales',style = "color:#C2D1E0")),
                                p('Con el paso de los años hemos observado un cambio en la composición de edad de los clientes nuevos. En el
                                  pasado los clientes nuevos se encontraban entre los 26 y los 45 años. Desde 2012 la edad de la mayoría de los 
                                  clientes nuevos va 18 a 35 años. Este cambio puede ser favorable si logramos crear un sentimiento de lealtad entre ellos
                                  y si tomamos las debidas precauciones.',style = "color:#C2D1E0;font-size:16pt"),
                                p('Tomando en cuenta este cambio en la constitución de edad y que el banco entró a Perú en 2008 decidimos analizar la información 
                                  en tres periodos de tiempo (antes de 2008, 2008 a 2011, 2012 a 2015). Debido a esto filtramos a los clientes que llegaron en cada 
                                  uno de los intervalos de tiempo antes mencionados y obtuvimos grupos utilizando sus patrones históricos de comportamiento de compras',
                                  style = "color:#C2D1E0;font-size:16pt"),
                                p('En cada uno de los grupos creados podemos ver que hay personas que satisfacen sus necesidades y después abandonan al grupo. Estas personas
                                  en general compran mercancías o préstamos personales en cantidades moderadas, terminan de pagar y después no vuelven a comprar. Estos clientes
                                  son clientes buenos que no estamos logrando retener y que sería fructífero hacerlo.',style = "color:#C2D1E0;font-size:16pt"),
                                p('Por otro lado, vemos que hay personas que tienen muchos pedidos de mercancías los cuáles van acompañados de una gran cantidad de pedidos de
                                  préstamos personales y pedidos con TAZ. Estas personas, en general, tienen su status de línea de Crédito inactiva por cliente malo o cancelada.
                                  Ahora que tenemos identificado este patrón sería bueno hacer un análisis más profundo de sus patrones de compra para identificar de forma preevntiva
                                  personas que se están comportando de esa manera, con la finalidad de evitar que se conviertan en cartera vecida.',style = "color:#C2D1E0;font-size:16pt"),
                                p('Las características de las personas que entraron antes del 2008 se pueden resumir en:',
                                  tags$ul(
                                    tags$li(strong("Cartera Vencida: "),"clientes con status de línea de crédito inactiva por malos, que tenían ocupado entre el 
                                            80 y el 100 % de su línea de crédito y que hicieron muchos pedidos de mercancías."),
                                    tags$li(strong("Moderados, no malos, muertos: "),"clientes con status de línea de crédito inactiva por tiempo que se atrasaron
                                            poco, que deben poco y que dejaron hace mucho tiempo."),
                                    tags$li(strong("Alto riesgo, muertos: "),"clientes que ya abandonaron la tienda y que en su historia acumularon muchos pedidos de mercancías, TAZ y préstamos
                                            personales, que se atrasaron considerablemente y tienen mucho saldo."),
                                    tags$li(strong("Activos y muertos de alto riesgo: "),"como los clientes del grupo anterior, pero con una considerable cantidad de 
                                            clientes con línea de crédito activa. Hay que tener cuidado en que estos clientes no se conviertan en cartera vencida."),
                                    style = "color:#C2D1E0;font-size:16pt"
                                    ),style = "color:#C2D1E0;font-size:16pt"),
                                p('Las características de las personas que entraron entre el 2008 y el 2011 se pueden resumir en:',
                                  tags$ul(
                                    tags$li(strong("Aceptables: "),"clientes con muchos pedidos que se atrasan poco y que ya no tienen pedidos activos."),
                                    tags$li(strong("Recién muertos, bajo riesgo: "),"personas con pocos pedidos que ya no tienen activos, deben poco y dejaron
                                            de comprar recientemente."),
                                    tags$li(strong("Cartera de riesgo medio: "),"clientes con una distribución elongada de pedidos en todos los rubros, con 
                                            atrasos moderados; una gran parte de ellos tienen su status de línea de crédito activa por lo que hay que tener cuidado
                                            con ellos."),
                                    tags$li(strong("Cartera de Riesgo Alto: "),"clientes con muchos pedidos en todos los rubros que están sobre endeudados. La 
                                            mayor parte de ellos son clientes con status de línea de crédito inactiva por mal cliente"),
                                    style = "color:#C2D1E0;font-size:16pt"
                                    ),style = "color:#C2D1E0;font-size:16pt"),
                                p('Las características de las personas que entraron entre el 2012 y el 2015 se pueden resumir en:',
                                  tags$ul(
                                    tags$li(strong("Aceptables y con riesgo bajo: "),"Personas que tienen status activo en su línea de crédito con pocos pedidos de mercancías
                                            que en la actualidad no tienen pedidos activos."),
                                    tags$li("Malos y de alto riesgo fase 3: ","clientes con muchos pedidos en todos los rubros; la mayor parte de ellos están sobre endeudados,
                                            tienen muchos pedidos activos, muchos atrasos, pero no deben tanto dinero."),
                                    tags$li(strong("Malos y de alto riesgo fase 2: "), "clientes con muchos pedidos en mercancías y en préstamos personales, con hasta el 80% de su línea de crédito usada, con dos o tres pedidos 
                                            activos, con dos o tres atrasos y poco saldo"),
                                    tags$li(strong("Malos y de alto riesgo fase 1: "), "personas con muchos pedidos de mercancías y préstamos personales con un pedido activo
                                            y poco saldo."),
                                    style = "color:#C2D1E0;font-size:16pt"
                                    ),style = "color:#C2D1E0;font-size:16pt")
                                    )
                                  )
                                  )
                            
                              )

))