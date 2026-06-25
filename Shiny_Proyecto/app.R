library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(scales)
options(scipen = 999)

#    https://shiny.posit.co/
# nunca cambiar el nombre o se jode
#
#si debe de añadir o modificar el dataset original, cree una variable que use sus gráficos, si no, pueden haber muchos conlfictos en los gráficos, de ser posible, señale cuales son sus variables/datos/objetos  para evitar errores entre gráficos
Dataset_cafe <- DatasetForCoffeeSales2 #solo cambie la ruta del archivo, no toque variables, si requiere añadir un dato, conversión, lo que sea de trabajo como objeto extra, hagalo afuera de todo el bloque de shiny

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


#Modificaciones para la pregunta 3
datos_pregunta3 <- Dataset_cafe
#Finalización de modificaciones para la pregunta 3


#Modificaciones para la pregunta 4 (Añadido por vos)
datos_pregunta4 <- Dataset_cafe
# Aseguramos formato lógico para evitar conflictos con el checkbox interactivo
datos_pregunta4$Used_Discount <- as.logical(datos_pregunta4$Used_Discount)
#Finalización de modificaciones para la pregunta 4


#Modificaciones para la pregunta 5 (Añadido e integrado)
datos_pregunta5 <- Dataset_cafe
#Finalización de modificaciones para la pregunta 5


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
      
      br(),
      hr(),
      
      h4("Instrucciones"),
      
      p("Seleccione una pregunta para visualizar su respectivo análisis, gráfico y resumen de resultados."),
      
      p("Cada sección incluye filtros interactivos que permiten explorar la información de ventas de café de Árabia Saudita.")
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
  
  resumen_ciudades_p3 <- reactive({
    
    req(input$producto_p3)
    
    if (input$producto_p3 == "Todos") {
      
      datos_pregunta3 %>%
        group_by(City) %>%
        summarise(
          Ingreso_total = sum(`Final Sales`, na.rm = TRUE),
          Promedio_ventas = mean(`Final Sales`, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(Ingreso_total))
      
    } else {
      
      datos_pregunta3 %>%
        filter(Product == input$producto_p3) %>%
        group_by(City) %>%
        summarise(
          Ingreso_total = sum(`Final Sales`, na.rm = TRUE),
          Promedio_ventas = mean(`Final Sales`, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(Ingreso_total))
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
  
  # Filtro reactivo basado en el slider para la Pregunta 5
  datos_filtrados_p5 <- reactive({
    req(input$rango_precio)
    datos_pregunta5 %>%
      filter(`Unit Price` >= input$rango_precio[1] & `Unit Price` <= input$rango_precio[2])
  })
  
  # Expresión reactiva para filtrar la base aislada según el checkbox de la P4
  datos_filtrados_p4 <- reactive({
    req(input$filtro_descuento_p4) 
    datos_pregunta4 %>% 
      filter(Used_Discount %in% input$filtro_descuento_p4)
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
        
        h4("Filtros del gráfico"),
        
        fluidRow(
          column(
            4,
            actionButton("ver_todos", "Ingresos totales", width = "100%")
          ),
          
          column(
            4,
            actionButton("mayores_ventas", "Mayores ventas", width = "100%")
          ),
          
          column(
            4,
            actionButton("menores_ventas", "Menores ventas", width = "100%")
          )
        ),
        
        br(), br(),
        
        plotOutput("grafico_ingresos"),
        br(),
        h3("Resumen mostrado"),
        tableOutput("tabla_resumen")
      )
      
    } else if(pantalla()=="p3"){ #aquí añaden la tag list, guíense de la pregunta 1 y 2
      
      tagList(
        h2("Pregunta 3"),
        h4("¿Cuáles ciudades Árabes generan los mayores ingresos por ventas de café durante el tiempo registrado?"),
        
        radioButtons(
          inputId = "producto_p3",
          label = "Seleccione el tipo de café:",
          choices = c(
            "Todos los productos" = "Todos",
            sort(unique(datos_pregunta3$Product))
          ),
          selected = "Todos"
        ),
        
        h3("Ingresos acumulados por ciudad"),
        plotOutput("grafico_ciudades", height = "450px"),
        
        hr(),
        
        fluidRow(
          column(
            4,
            wellPanel(
              h4("Promedio de ventas"),
              textOutput("promedio_ventas")
            )
          ),
          
          column(
            4,
            wellPanel(
              h4("Mediana general"),
              textOutput("mediana_ventas")
            )
          ),
          
          column(
            4,
            wellPanel(
              h4("Ciudad con mayor ingreso"),
              textOutput("mejor_ciudad")
            )
          )
        ),
        
        hr(),
        
        h3("Resumen de ingresos por ciudad"),
        tableOutput("tabla_ciudades"),
        
        hr(),
        
        h3("Interpretación"),
        textOutput("texto_explicativo")
      )
      
    }
    else if(pantalla()=="p4"){
      tagList(
        h2("Pregunta 4"),
        h4("¿Cómo se comportan las cantidades finales dependiendo de si se usa descuento o no?"),
        br(),
        
        
        checkboxGroupInput(
          inputId = "filtro_descuento_p4",
          label = "Seleccione el estado del descuento:",
          choices = c("Con Descuento" = TRUE, "Sin Descuento" = FALSE),
          selected = c(TRUE, FALSE)
        ),
        
        br(),
        
        
        plotOutput("grafico_dispersion_p4"),
        br(),
        hr(),
        
        
        h5("Grafico de dispersión"),
        p("Este gráfico de dispersión permite evaluar la relación entre la cantidad de unidades vendidas y ",
          "las ventas finales obtenidas. Al activar y desactivar los filtros de la consola, se puede apreciar ",
          "visualmente si la aplicación de descuentos desplaza la tendencia de los puntos o altera los patrones de compra."),
        
        h5("Variables"),
        tags$ul(
          tags$li("Muestra analizada a partir del archivo 'DatasetForCoffeeSales2.csv'."),
          tags$li("Variable 'Used_Discount' clasificada de manera binaria (TRUE para transacciones con rebaja, FALSE para precio regular)."),
          tags$li("Las cantidades finales corresponden al cruce directo entre 'Quantity' y 'Final Sales'.")
        )
      )
    }
    else if(pantalla()=="p5"){ #aquí añaden la tag list, guíense de la pregunta 1 y 2
      tagList(
        h2("Pregunta 5"),
        h4("Análisis de Precio vs Cantidad"),
        sliderInput("rango_precio", 
                    "Seleccionar rango de Precio Unitario:", 
                    min = min(datos_pregunta5$`Unit Price`, na.rm = TRUE), 
                    max = max(datos_pregunta5$`Unit Price`, na.rm = TRUE), 
                    value = c(min(datos_pregunta5$`Unit Price`, na.rm = TRUE), max(datos_pregunta5$`Unit Price`, na.rm = TRUE))),
        br(),
        plotOutput("grafico_p5"),
        br(),
        h3("Resumen de Precio vs Cantidad Vendida"),
        tableOutput("tabla_resumen_p5")
      )
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
  
  output$grafico_ciudades <- renderPlot({
    
    resumen <- resumen_ciudades_p3()
    
    ggplot(
      resumen,
      aes(
        x = reorder(City, -Ingreso_total),
        y = Ingreso_total,
        fill = City
      )
    ) +
      geom_col(show.legend = FALSE) +
      scale_y_continuous(labels = dollar_format(prefix = "$")) +
      labs(
        title = "Ingresos acumulados por ciudad",
        subtitle = "Según el producto de café seleccionado",
        x = "Ciudad",
        y = "Ingresos totales por ventas finales"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  output$promedio_ventas <- renderText({
    
    promedio <- mean(
      if (input$producto_p3 == "Todos") {
        datos_pregunta3$`Final Sales`
      } else {
        datos_pregunta3 %>%
          filter(Product == input$producto_p3) %>%
          pull(`Final Sales`)
      },
      na.rm = TRUE
    )
    
    dollar(promedio)
  })
  
  output$mediana_ventas <- renderText({
    
    mediana <- median(
      if (input$producto_p3 == "Todos") {
        datos_pregunta3$`Final Sales`
      } else {
        datos_pregunta3 %>%
          filter(Product == input$producto_p3) %>%
          pull(`Final Sales`)
      },
      na.rm = TRUE
    )
    
    dollar(mediana)
  })
  
  output$mejor_ciudad <- renderText({
    
    resumen <- resumen_ciudades_p3()
    
    req(nrow(resumen) > 0)
    
    paste0(
      resumen$City[1],
      " (",
      dollar(resumen$Ingreso_total[1]),
      ")"
    )
  })
  
  output$tabla_ciudades <- renderTable({
    
    resumen_ciudades_p3() %>%
      mutate(
        Ingreso_total = dollar(Ingreso_total),
        Promedio_ventas = dollar(Promedio_ventas)
      ) %>%
      rename(
        Ciudad = City,
        `Ingreso total` = Ingreso_total,
        `Promedio por venta` = Promedio_ventas
      )
  })
  
  output$texto_explicativo <- renderText({
    
    resumen <- resumen_ciudades_p3()
    
    req(nrow(resumen) > 0)
    
    ciudad1 <- resumen$City[1]
    ingreso1 <- resumen$Ingreso_total[1]
    
    if (nrow(resumen) >= 2) {
      
      ciudad2 <- resumen$City[2]
      ingreso2 <- resumen$Ingreso_total[2]
      
      paste0(
        "Para el producto seleccionado, la ciudad con mayor ingreso acumulado es ",
        ciudad1, ", con un total de ", dollar(ingreso1),
        ". Le sigue ", ciudad2, ", con ", dollar(ingreso2),
        ". El gráfico permite comparar fácilmente el ingreso total generado en cada ciudad."
      )
      
    } else {
      
      paste0(
        "Para el producto seleccionado, la ciudad con mayor ingreso acumulado es ",
        ciudad1, ", con un total de ", dollar(ingreso1), "."
      )
    }
  })
  
  # pregunta 4
  # 4.3 Renderizar Gráfico de Dispersión Interactivo P4
  output$grafico_dispersion_p4 <- renderPlot({
    ggplot(datos_filtrados_p4(), aes(x = Quantity, y = `Final Sales`, color = Used_Discount)) +
      geom_point(size = 3.5, alpha = 0.7, position = position_jitter(width = 0.15, height = 0)) +
      scale_color_manual(
        values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db"),
        labels = c("TRUE" = "Con Descuento", "FALSE" = "Sin Descuento")
      ) +
      labs(
        title = "¿Cómo se comportan las cantidades finales dependiendo de si se usa descuento o no?",
        subtitle = "Análisis de Dispersión: Unidades vs Ventas Finales",
        x = "Cantidad de Unidades (Quantity)",
        y = "Monto de Ventas Finales (Final Sales)",
        color = "Filtro de Descuento"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", size = 14),
        legend.position = "bottom"
      )
  })
  #fin pregunta 4
  # 
  
  # Outputs pregunta 5
  output$grafico_p5 <- renderPlot({
    df_p5 <- datos_filtrados_p5() %>%
      group_by(`Unit Price`) %>%
      summarise(Total_Cantidad = sum(Quantity, na.rm = TRUE), .groups = "drop")
    
    ggplot(df_p5, aes(x = factor(`Unit Price`), y = Total_Cantidad, fill = factor(`Unit Price`))) +
      geom_col(color = "black") +
      scale_fill_brewer(palette = "YlOrBr") +
      labs(title = "Relación Precio Unitario vs Cantidad Total Vendida",
           x = "Precio Unitario ($)", 
           y = "Cantidad Total Vendida",
           fill = "Precio ($)") +
      theme_minimal(base_size = 14)
  })
  
  output$tabla_resumen_p5 <- renderTable({
    datos_filtrados_p5() %>%
      group_by(`Unit Price`) %>%
      summarise(
        `Cantidad Comprada` = sum(Quantity, na.rm = TRUE),
        `Número de Transacciones` = n(),
        `Venta Final Promedio` = mean(`Final Sales`, na.rm = TRUE),
        .groups = "drop"
      )
  })
  
} #Finaliza el código, no escriba nada fuera de este parentesis y siemore llevelo con espacios

shinyApp(ui = ui, server = server)