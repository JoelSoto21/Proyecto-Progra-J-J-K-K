library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
options(scipen = 999)

Dataset_cafe <- read_csv("DatasetForCoffeeSales2.csv")

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
  
  output$contenido_pregunta <- renderUI({
    
    req(input$pregunta2)
    
    tagList(
      h2("Pregunta 2"),
      h4("¿Qué tipos de café generan mayores ingresos y cantidades de venta?"),
      br(),
      plotOutput("grafico_ingresos"),
      br(),
      h3("Resumen mostrado"),
      tableOutput("tabla_resumen")
    )
  })
  
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
}

shinyApp(ui = ui, server = server)