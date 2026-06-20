library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)


# Convertir fecha
datos$Date <- mdy(datos$Date)

# Interfaz
ui <- fluidPage(
  
  titlePanel("Pregunta 3: Ciudades con mayores ingresos por ventas de café"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Filtro interactivo"),
      
      radioButtons(
        inputId = "producto",
        label = "Seleccione el tipo de café:",
        choices = c(
          "Todos los productos" = "Todos",
          sort(unique(datos$Product))
        ),
        selected = "Todos"
      )
    ),
    
    mainPanel(
      
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
  )
)

# Servidor
server <- function(input, output) {
  
  # Datos según producto seleccionado
  datos_filtrados <- reactive({
    
    if (input$producto == "Todos") {
      
      datos
      
    } else {
      
      datos %>%
        filter(Product == input$producto)
    }
  })
  
  # Resumen por ciudad
  resumen_ciudades <- reactive({
    
    datos_filtrados() %>%
      group_by(City) %>%
      summarise(
        Ingreso_total = sum(`Final Sales`, na.rm = TRUE),
        Promedio_ventas = mean(`Final Sales`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Ingreso_total))
  })
  
  # Gráfico con ciudades en eje X
  output$grafico_ciudades <- renderPlot({
    
    resumen <- resumen_ciudades()
    
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
  
  # Promedio general
  output$promedio_ventas <- renderText({
    
    promedio <- mean(datos_filtrados()$`Final Sales`, na.rm = TRUE)
    
    dollar(promedio)
  })
  
  # Mediana general
  output$mediana_ventas <- renderText({
    
    mediana <- median(datos_filtrados()$`Final Sales`, na.rm = TRUE)
    
    dollar(mediana)
  })
  
  # Ciudad con mayor ingreso
  output$mejor_ciudad <- renderText({
    
    resumen <- resumen_ciudades()
    
    req(nrow(resumen) > 0)
    
    paste0(
      resumen$City[1],
      " (",
      dollar(resumen$Ingreso_total[1]),
      ")"
    )
  })
  
  # Tabla
  output$tabla_ciudades <- renderTable({
    
    resumen_ciudades() %>%
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
  
  # Texto automático
  output$texto_explicativo <- renderText({
    
    resumen <- resumen_ciudades()
    
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
}

shinyApp(ui = ui, server = server)