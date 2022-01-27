library(pacman)
pacman::p_load(readxl,tidyverse,dplyr,ggplot2,expss,DT,summarytools,rgdal,leaflet,sp,sf,pryr,reshape2,plyr,plotly,
               shinythemes,shinydashboard,shinyWidgets,jsonlite,rsconnect)
estados2<-readOGR("México_Estados.shp")
densidad<-read_excel("Población_07.xlsx",
                     range = "a5:b36",
                     col_names = c("Estado","Densidad"))
poblacion<-read_excel("Poblacion_01.xlsx",
                      range = "a6:e764",
                      col_names = c("Estado","Edad","Total","Hombre","Mujer"))

i<-c(2,3,18,14,1,11,22,13,16,15,9,6,17,31,4,21,23,29,12,20,27,7,26,8,5,25,10,32,24,19,28,30)
pob<-poblacion[!(poblacion$Estado == "Estados Unidos Mexicanos"),]
pob<-pob[pob$Edad==0,]
pob1<-poblacion[poblacion$Edad==0,]
estados2$poblacion<-unlist(pob[i,3],use.names = FALSE)
estados2$pobfem<-unlist(pob[i,5],use.names = FALSE)
estados2$pobmas<-unlist(pob[i,4],use.names = FALSE)
estados2$densidad<-unlist(densidad[i,2],use.names = FALSE)
estados2$ESTADO<-unlist(pob[i,1],use.names = FALSE)
lbls<-c("Total",paste(seq(0,95,5),seq(4,99,5),sep = "-"),"100 y más", "No especificado")
poblacion$Edad<-factor(poblacion$Edad,labels = lbls)
bins<-seq(0,18,2)
pal <- colorBin("YlOrRd", domain = estados2$poblacion/1000000, bins = bins)
mapa<-leaflet(estados2) %>% addProviderTiles("Esri.WorldTerrain") %>% 
  addPolygons(
    smoothFactor = 0.8,
    fillColor = ~pal(poblacion/1000000),
    weight = 2,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = sprintf(
      "<strong>%s</strong><br/>Población Total: %s<br/>Población femenina: %s<br/>población masculina: %s<br/>Presonas / km<sup>2</sup> : %s",estados2$ESTADO,estados2$poblacion,estados2$pobfem,estados2$pobmas,estados2$densidad) %>% lapply(htmltools::HTML),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~poblacion/1000000, opacity = 0.7, title ="Personas por millon",
            position = "bottomleft")

shinyApp(
  ui<- bootstrapPage(
    tags$head(includeHTML("globtag.html")),
    navbarPage(theme = shinytheme("darkly"), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">COVID-19 tracker</a>'), id="nav",
               windowTitle = "Distribución de población",
               
               tabPanel("Introducción",
                        tags$div(
                          "Lo que se pretende con este trabajo es ajustar la distribución de la población mexicana mediante coeficientes de 
                          B-spline, cabe destacar que es de gran ayuda al realizar análisis cuyos conjuntos de datos son bastante grandes, lo cual hace 
                          que algunas herramientas usadas de manera convencional sean poco efectivas. Esto se puede ver cuando se da un aumento de manera
                          exponencial en el volumen de los espacios lo que causa dispersión en los datos." ,
                         tags$br(),tags$br(),
                          "Una de las ventajas de trabajar coeficientes de B-spline es que se reducen las dimensiones,entonces,el objetivo es poder 
                          hacer que los datos funcionales se caractericen mediante dichos coeficientes y poder utilizarlos para hacer comparaciones 
                          y monitoreos.",
                         tags$br(),tags$br(),
                          "Es bien sabido que las funciones modelan muchos e importantes procesos, por lo que hacer una extracción de información de 
                          manera optima es de gran utilidad,de manera que es intuitivo el avance que se esta teniendo en el uso de las funciones de 
                          variables reales en los distintos ámbitos de investigación.Los métodos matemáticos que hacen esto posible se encuentran en 
                          la rama de Análisis de datos funcionales.",
                         tags$h3("¿ Por qué?"),
                         
                         "Si bien a lo largo de los años se han hecho proyecciones de las poblaciones mediante los datos que se recolectan en los censos 
                         de población que se hacen cada seis años, el hecho de hacer manejo de tan gran cantidad de datos hace que los procesos sean más
                         largos y complicados según los métodos aplicados. Por lo que nuestra intención es hacer estas proyecciones de una manera más optima.",
                         tags$br(), tags$br(),
                         
                         "Debido a que el uso de el análisis de datos funcionales es una herramienta estadística poco conocida ,
                         surgió la curiosidad e interés para realizar su estudio y aplicación, de una forma que se nos de una nueva perspectiva del 
                         comportamiento de la distribución del estado. En un caso más amplio, si es posible, ayudar al desarrolllo y aplicación de los métodos
                         multivariados.",
                         tags$br(), tags$br(),
                         "Con los resultados obtenidos poder establecer tal vez algunas sugerencias a nivel gubernamental y brindar un nuevo tema de 
                         interés a la comunidad estudiantil."
                          )
                 
               ),
             

    tabPanel("Piramides de población",
             
             sidebarLayout(
               sidebarPanel(
                 
                 span(tags$i(h6("Una piramide poblacional ")), style="color:#045a8d"),
                 span(tags$i(h6("Se representan los datos estadísticos de la población 
                                clasificados por sexo y edad, que permite las comparaciones entre territorios en el tiempo y una fácil y rápida 
                                percepción de los fenómenos demográficos que afectan o no a esa población."))),
                 
                 
                 pickerInput("region_select", "Región:",   
                             choices = as.character(pob1[order(pob1$Edad),]$Estado), 
                             options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                             selected = as.character(pob1[order(-pob1$Edad),]$Estado)[1],
                             multiple = FALSE), 
                 
                ),
               
               mainPanel("Piramide",
                         "Se puede observar que las piramides en su maryoría son más amplias en las partes bajas que es donde las edades son más bajas,
                         lo cual expresa que la población en México es bastante joven, en contraparte se observa que es muy poca la frecuencia de tener adultos
                         con más de 90 años.",
                         tags$br(),
                         "Sin embargo tenemos datosregistrado en no especificados, lo cual muestra que se podria decir que hay cierta cantidad dde personas que desconocen la manera de representar su edad
                          o algún otro problema para facilitar a los encuestadores los registros precisos",
                         plotlyOutput("piramide"),
               )
             )
    ),
    tabPanel("Población en México",
             div(class="outer",
                 tags$head(includeCSS("style.css")),
                 leafletOutput("mymap", width="100%", height="100%"),
             )
    ),
    tabPanel("Datos",
             "Los datos aquí usados fueron obtenidos de la página oficial del",
             tags$a(href="https://www.inegi.org.mx/temas/estructura/#Tabulados", "INEGI"),
             tags$br(), tags$br(),
             " La base de datos cuenta con 5 variables y ",
             pickerInput("region", "Región:",   
                         choices = as.character(pob1[order(pob1$Edad),]$Estado), 
                         options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                         selected = as.character(pob1[order(-pob1$Edad),]$Estado)[1],
                         multiple = FALSE), 
             tableOutput("rawtable"),
             downloadButton("downloadCsv", "Download as CSV"),tags$br(),tags$br(),
    ),
    tabPanel("Inconcluso ",
      "La información en general esta inconclusa debido a que por el momento no se ha realizado los ajustes de las variables para la modelación,
      se espera terminar la página con la información completa al mismo ritmo que la tesis "
    ),
   )
  ),
 
  server = function(input, output,session) {
    output$mymap <- renderLeaflet({ 
      mapa
    })
    output$piramide<-renderPlotly({
      p<-poblacion[!(poblacion$Edad== "Total"),]
      sel<-input$region_select
      p<-p[p$Estado== sel,]
      p2<-p %>% mutate(H=Hombre/sum(Hombre)*100,M=Mujer/sum(Mujer)*100) %>% select(Edad,H,M)
      pop<-p2 %>% gather(sexo,p_edad,-1)
      g<-ggplot(pop,aes(x=Edad,fill=sexo,
                        y=ifelse(sexo=="H",-p_edad,p_edad)))+ 
        geom_bar(stat = "identity")+ 
        scale_y_continuous(limits = max(pop$p_edad)*c(-1,1),labels = abs) + 
        labs(y= "Porcentajes por edades quinquenales")+
        coord_flip()
      ggplotly(g)
    })
    output$downloadCsv <- downloadHandler(
      filename = function() {
        paste("pobalción", ".csv", sep="")
      },
      content = function(file) {
        cases_sub = poblacion 
        write.csv(cv_cases_sub, file)
      }
    )
 
    output$rawtable <- renderTable({
      datos<-poblacion %>% filter(Estado==input$region)
      datos 
    })
    
  }
)