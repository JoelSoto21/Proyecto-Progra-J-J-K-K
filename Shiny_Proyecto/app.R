library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
options(scipen = 999)

#    https://shiny.posit.co/
# nunca cambiar el nombre o se jode
#
#si debe de añadir o modificar el dataset original, cree una variable que use sus gráficos, si no, pueden haber muchos conlfictos en los gráficos, de ser posible, señale cuales son sus variables/datos/objetos  para evitar errores entre gráficos
Dataset_cafe <- read_csv("DatasetForCoffeeSales2.csv") #solo cambie la ruta del archivo, no toque variables, si requiere añadir un dato, conversión, lo que sea de trabajo como objeto extra, hagalo afuera de todo el bloque de shiny

#Modificaciones para la pregunta 1
jorgito<-Dataset_cafe #Cambio para no tocar el dataset original
jorgito <- jorgito %>%
  rename(Final.Sales = `Final Sales`) #solo
jorgito$Date <- as.Date(jorgito$Date, format = "%m/%d/%Y") #reordeno archivo a formato de fecha
df = data.frame( jorgito$Date, jorgito$Final.Sales ) #dataframe de jorgito
promediainador <- df %>% #lo convierto en promediainador para crear un objeto nuevo más simplificado y listo
  mutate(
    Año = format(jorgito.Date, "%Y"),
    Mes = format(jorgito.Date, "%m")
  ) %>% #agarre cada fecha por mes y año y separelas
  group_by(Año, Mes) %>%
  summarise(
    Promedio = mean(jorgito.Final.Sales, na.rm = TRUE), #promediainador será dos columnas, una por de meses y otra por años y tendra un promeido de las ventas finales en cada mes
    .groups = "drop"
  )
Ramgo=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24) #Se tenia que hacer una forma de que R entendiera que 1 a 24 son variables distintas, y al crear promediainador se me fue el detalle de que le puse enero=1, tanto 2023 como 2024, entonces solo agregue otra columna con 24 id´s diferentes, para que usara esa colunmna como x
promediainador$Rango<-Ramgo
#Finalización de modificaciones para el gráfico 1




#No tocar nada de esta parte
ui <- fluidPage(
  
  titlePanel("Análisis de ventas de café"),
  
  sidebarLayout(
    
    sidebarPanel(
      h3("Preguntas"),
      
      actionButton("pregunta1", "Pregunta 1", width = "100%"),
      br(), br(),
      
      actionButton("pregunta2", "Pregunta 2", width = "100%"),
      br(), br(),
      
      actionButton("pregunta3", "Pregunta 3", width = "100%"),
      br(), br(),
      
      actionButton("pregunta4", "Pregunta 4", width = "100%"),
      br(), br(),
      
      actionButton("pregunta5", "Pregunta 5", width = "100%"),
      br(), br(),
      
      h4("Filtros del gráfico"),
      
      actionButton("ver_todos", "Ingresos totales", width = "100%"),
      br(), br(),
      
      actionButton("mayores_ventas", "Mayores ventas", width = "100%"),
      br(), br(),
      
      actionButton("menores_ventas", "Menores ventas", width = "100%")
    ),
    
    mainPanel(
      uiOutput("contenido_pregunta")
    )
  )
)

server <- function(input, output) {
  
  resumen_cafe <- Dataset_cafe %>%
    group_by(Product) %>%
    summarise(
      Total_Cantidad = sum(Quantity, na.rm = TRUE),
      Total_Ingresos = sum(`Final Sales`, na.rm = TRUE),
      Promedio_Ingresos = mean(`Final Sales`, na.rm = TRUE),
      Numero_Ventas = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(Total_Ingresos))
  
  vista <- reactiveVal("todos")
  
  observeEvent(input$ver_todos, {
    vista("todos")
  })
  
  observeEvent(input$mayores_ventas, {
    vista("mayores")
  })
  
  observeEvent(input$menores_ventas, {
    vista("menores")
  })
  
  datos_grafico <- reactive({
    
    if (vista() == "todos") {
      resumen_cafe
    } else if (vista() == "mayores") {
      resumen_cafe %>%
        filter(Total_Cantidad == max(Total_Cantidad))
    } else if (vista() == "menores") {
      resumen_cafe %>%
        filter(Total_Cantidad == min(Total_Cantidad))
    }
  })
  pantalla <- reactiveVal("ninguna")
  #Tuvimos que añadir un donde o un algo para que entienda que es cada pantalla, 
  observeEvent(input$pregunta1, {
    pantalla("p1")
  })
  
  observeEvent(input$pregunta2, {
    pantalla("p2")
  })
  observeEvent(input$pregunta3, {
    pantalla("p3")
  })
  observeEvent(input$pregunta4, {
    pantalla("p4")
  })
  observeEvent(input$pregunta5, {
    pantalla("p5")
  })
  
  observeEvent(input$ver_todos, {
    vista("todos")
  })
  #Desde aquí en adelante puede manipular, pero hagalo con cuidado y siempre con respaldo
  output$contenido_pregunta <- renderUI({
    
    if (pantalla() == "p1") {
      
      tagList(
        h2("Pregunta 1"),
        h4("¿Cómo varían las ventas de café a lo largo del tiempo?"),
        sliderInput("Rango", "Meses", 1, 24, c(1,3)),
        plotOutput("distPlot")
      )
      
    } else if (pantalla() == "p2") {
      
      tagList(
        h2("Pregunta 2"),
        h4("¿Qué tipos de café generan mayores ingresos y cantidades de venta?"),
        br(),
        plotOutput("grafico_ingresos"),
        br(),
        h3("Resumen mostrado"),
        tableOutput("tabla_resumen")
      )
    }
    else if(pantalla()=="p3"){ #aquí añaden la tag list, guíense de la pregunta 1 y 2
      
    }
    else if(pantalla()=="p4"){#aquí añaden la tag list, guíense de la pregunta 1 y 2
      
    }
    else if(pantalla()=="p5"){#aquí añaden la tag list, guíense de la pregunta 1 y 2
      
    }
  })
  
  #Inicio pregunta 2
  
  output$grafico_ingresos <- renderPlot({
    
    req(input$pregunta2)
    
    ggplot(
      datos_grafico(),
      aes(
        x = reorder(Product, Total_Ingresos),
        y = Total_Ingresos,
        fill = Product
      )
    ) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(
        values = c(
          "Brazilian" = "forestgreen",
          "Colombian" = "gold",
          "Costa Rica" = "blue",
          "Ethiopian" = "purple",
          "Guatemala" = "skyblue"
        )
      ) +
      labs(
        title = "Ingresos totales por tipo de café",
        x = "Tipo de café",
        y = "Ingresos totales"
      ) +
      theme_minimal()
  })
  
  output$tabla_resumen <- renderTable({
    req(input$pregunta2)
    datos_grafico()
  })
  #Final pregunta 2
  
  
  #Inicio pregunta 1
  output$distPlot <- renderPlot({
    
    Canijote <- promediainador[
      promediainador$Rango >= input$Rango[1] &
        promediainador$Rango <= input$Rango[2],
    ]
    
    ggplot(Canijote, aes(x = Rango, y = Promedio)) +
      geom_line(color = "blue3", linewidth = 2) +
      geom_point() +
      labs(
        title = "Variación del promedio de venta de café a lo largo del tiempo",
        x = "Meses",
        y = "Promedio"
      ) +
      scale_x_continuous(
        breaks = seq(1, 24, by = 1),
        labels = c("Ene/23","Feb/23","Mar/23","Abr/23","May/23","Jun/23",
                   "Jul/23","Ago/23","Sep/23","Oct/23","Nov/23","Dic/23",
                   "Ene/24","Feb/24","Mar/24","Abr/24","May/24","Jun/24",
                   "Jul/24","Ago/24","Sep/24","Oct/24","Nov/24","Dic/24")
      )
  })
  #Finalización pregunta 1
  #Añada a partir de aquí
} #Finaliza el código, no escriba nada fuera de este parentesis y siemore llevelo con espacios

shinyApp(ui = ui, server = server)



