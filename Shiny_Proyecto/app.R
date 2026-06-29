#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
# nunca cambiar el nombre o se jode
#
#
#
#
#
#
#
#
#



library(shiny)     
library(readr)     
library(dplyr)     
library(ggplot2)   
library(scales)   
options(scipen = 999) # Desactiva la notación científica (ej. muestra 100000 en vez de 1e+05)

#    https://shiny.posit.co/
# nunca cambiar el nombre o se jode
#
#si debe de añadir o modificar el dataset original, cree una variable que use sus gráficos, si no, pueden haber muchos conlfictos en los gráficos, de ser posible, señale cuales son sus variables/datos/objetos  para evitar errores entre gráficos

# Lectura del archivo de datos original
Dataset_cafe <- read_csv("DatasetForCoffeeSales2.csv") #solo cambie la ruta del archivo, no toque variables, si requiere añadir un dato, conversión, lo que sea de trabajo como objeto extra, hagalo afuera de todo el bloque de shiny
#Aquí añadimos algunas funciones que se usan mucho, entonces para no estar explicando cada una
#h2 coloca título
#h4 un subtitulo
#h5 muestra otro subtitulo
#p lo que se quiere escribir en cierto lugar
#dive(style==) permite añadir un poco de decoración, pero se uso meramente en colocar fonditos blancos a algunas interpretaciones y tablas para una fácil lectura sin que se pierda en el fondo
#taglist() esto permite decir que queremos que contenga cada bloque, en este caso, cada pregunta cuando se muestre



# Modificaciones para la pregunta 1
jorgito<-Dataset_cafe #Cambio para no tocar el dataset original
jorgito <- jorgito %>%
  rename(Final.Sales = `Final Sales`) #solo cambio el nombre pues usaba read.csv y los compañeros usan read_csventonces así evito molestar a los compañeros
jorgito$Date <- as.Date(jorgito$Date, format = "%m/%d/%Y") #reordeno archivo a formato de fecha
df = data.frame( jorgito$Date, jorgito$Final.Sales ) #dataframe de jorgito

promediainador <- df %>% #lo convierto en promediainador para crear un objeto nuevo más simplificado y listo
  mutate(
    Año = format(jorgito.Date, "%Y"), # Extrae el año de la fecha
    Mes = format(jorgito.Date, "%m")  # Extrae el mes numérico de la fecha
  ) %>% #agarre cada fecha por mes y año y separelas
  group_by(Año, Mes) %>% # Agrupa los datos por año y luego por mes
  summarise(
    Promedio = mean(jorgito.Final.Sales, na.rm = TRUE), #promediainador será dos columnas, una por de meses y otra por años y tendra un promeido de las ventas finales en cada mes
    .groups = "drop" # Desagrupa para evitar problemas en cálculos futuros
  )
# Vector de 1 a 24 para identificar de forma lineal y consecutiva los 24 meses del periodo 2023-2024
Ramgo=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24) #Se tenia que hacer una forma de que R entendiera que 1 a 24 son variables distintas, 
#y al crear promediainador se me fue el detalle de que le puse enero=1,
#tanto 2023 como 2024, entonces solo agregue otra columna con 24 id´s 
#diferentes, para que usara esa colunmna como x
promediainador$Rango<-Ramgo
#Finalización de modificaciones para el gráfico 1


# Modificaciones para la pregunta 3
datos_pregunta3 <- Dataset_cafe
#Finalización de modificaciones para la pregunta 3


# Modificaciones para la pregunta 4 
datos_pregunta4 <- Dataset_cafe
# Aseguramos formato lógico para evitar conflictos con el checkbox interactivo
datos_pregunta4$Used_Discount <- as.logical(datos_pregunta4$Used_Discount) # Convierte a TRUE/FALSE explícito
#Finalización de modificaciones para la pregunta 4


# Modificaciones para la pregunta 5 
datos_pregunta5 <- Dataset_cafe
#Finalización de modificaciones para la pregunta 5


# No tocar nada de esta parte

ui <- fluidPage(
  tags$head( #Esto añade a donde se le indique decoración, se puede hacer prácticamente todo con esto
    tags$style(HTML("  
                    body {
                      background-image: url('cafe2.jpeg');
                      background-size: cover;
                      background-attachment: fixed;
                      background-position: center;
                    }
                     #tabla_resumen table {
      background-color: white;
      color: black;
    }

    #tabla_resumen th {
      background-color: #D2B48C;
      color: black;
    }

  
    #pregunta1 {
      background-color: #3498db; 
      color: white;
      border-radius: 10px;
    }

    #pregunta2 {
      background-color: #27ae60;
      color: white;
      border-radius: 10px;
    }

    #pregunta3 {
      background-color: #9b59b6;
      color: white;
      border-radius: 10px;
    }

    #pregunta4 {
      background-color: #e67e22;
      color: white;
      border-radius: 10px;
    }

    #pregunta5 {
      background-color: #e74c3c;
      color: white;
      border-radius: 10px;
    }
  ")) #El tag$style(HTML()), hace que shiny entienda lo que esta escrito dentro, como algo válido para decorar, y yap
  ),#Se le dijo por pregunta X, añada color al cuadradito y que el texto por pregunta sea blanco para que no se vea feo y el border-radius le iba diciendo que tanto se redondean las esquinas 0 es cuadrado, 10 alguito y 50 o más(si es que se puede más) ya es como un huevo y solo se usa ";"  porque es como el CSS lee que termina cada orden
  
  titlePanel("Análisis de ventas de café"), #Título del panel # Título principal visible de la app
  
  sidebarLayout( # Distribuye la pantalla en un panel lateral (izq) y uno principal (der)
    
    sidebarPanel( # Panel lateral para los controles, botones e instrucciones
      h3("Preguntas"),
      
      # Botones de acción interactivos para alternar entre las preguntas
      actionButton("pregunta1", "Pregunta 1", width = "100%"),
      br(), br(), # Saltos de línea para distanciar los botones
      
      actionButton("pregunta2", "Pregunta 2", width = "100%"),
      br(), br(),
      
      actionButton("pregunta3", "Pregunta 3", width = "100%"),
      br(), br(),
      
      actionButton("pregunta4", "Pregunta 4", width = "100%"),
      br(), br(),
      
      actionButton("pregunta5", "Pregunta 5", width = "100%"),
      
      br(),
      hr(), # Línea horizontal divisoria
      
      h4("Instrucciones"),
      
      p("Seleccione una pregunta para visualizar su respectivo análisis, gráfico y resumen de resultados."),
      
      p("Cada sección incluye filtros interactivos que permiten explorar la información de ventas de café de Árabia Saudita.")
    ),
    
    mainPanel( # Panel principal donde se renderiza el contenido seleccionado dinámicamente
      uiOutput("contenido_pregunta") # Output dinámico generado en el Server basado en la pregunta activa
    )
  )
)


server <- function(input, output) {
  
  # Resumen base del dataset agrupado por producto (utilizado en la pregunta 2)
  resumen_cafe <- Dataset_cafe %>%
    group_by(Product) %>%
    summarise(
      Total_Cantidad = sum(Quantity, na.rm = TRUE),
      Total_Ingresos = sum(`Final Sales`, na.rm = TRUE),
      Promedio_Ingresos = mean(`Final Sales`, na.rm = TRUE),
      Numero_Ventas = n(), # Cuenta la cantidad de registros por producto
      .groups = "drop"
    ) %>%
    arrange(desc(Total_Ingresos)) # Ordena de mayor a menor ingreso total
  
  # Variable reactiva para controlar los sub-filtros de la Pregunta 2
  vista <- reactiveVal("todos")
  
  # Cambios de estado para la variable reactiva 'vista' según el botón presionado
  observeEvent(input$ver_todos, {
    vista("todos")
  })
  
  observeEvent(input$mayores_ventas, {
    vista("mayores")
  })
  
  observeEvent(input$menores_ventas, {
    vista("menores")
  })
  
  #  reactiva  filtra la tabla resumen_cafe según el estado del botón interno en P2
  datos_grafico <- reactive({
    
    if (vista() == "todos") {
      resumen_cafe
    } else if (vista() == "mayores") {
      resumen_cafe %>%
        filter(Total_Cantidad == max(Total_Cantidad)) # Filtra solo el producto con más unidades vendidas
    } else if (vista() == "menores") {
      resumen_cafe %>%
        filter(Total_Cantidad == min(Total_Cantidad)) # Filtra solo el producto con menos unidades vendidas
    }
  })
  
  #  reactiva para calcular el volumen e ingresos por ciudad en la Pregunta 3
  resumen_ciudades_p3 <- reactive({
    
    req(input$producto_p3) # Asegura que el input exista antes de ejecutar el código
    
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
        filter(Product == input$producto_p3) %>% # Filtra por el producto específico seleccionado en la UI
        group_by(City) %>%
        summarise(
          Ingreso_total = sum(`Final Sales`, na.rm = TRUE),
          Promedio_ventas = mean(`Final Sales`, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(Ingreso_total))
    }
  })
  
  # Variable reactiva principal que determina cuál pregunta ("p1", "p2", etc.) se muestra en el MainPanel
  pantalla <- reactiveVal("naira")#Tuvimos que añadir un donde o un algo para que entienda que es cada cosa, naira es solo para que no este vacío desde el inicio si no se bugea
  
  # Observadores de eventos que detectan los clicks en el menú lateral y cambian de pantalla
  observeEvent(input$pregunta1, { #definimos un p# para cada pregunta y llamarla cuando se va usar, así podemos mostrarlas por separado
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
  }) #lo que hace el observeEvent, es para específicamente "cuando presione X ocurra Y" aquí se uso para solo mostrar las preguntas cuadno se seleccionen sin que choquen
  
  # Filtro reactivo basado en el slider para la Pregunta 5 (Rango de Precio Unitario)
  datos_filtrados_p5 <- reactive({
    req(input$rango_precio)
    datos_pregunta5 %>%
      filter(`Unit Price` >= input$rango_precio[1] & `Unit Price` <= input$rango_precio[2])
  })
  
  #  reactiva para filtrar la base aislada según el checkbox de la P4 (Si usó o no descuento)
  datos_filtrados_p4 <- reactive({
    req(input$filtro_descuento_p4) 
    datos_pregunta4 %>% 
      filter(Used_Discount %in% input$filtro_descuento_p4) # Verifica concordancia con los valores lógicos seleccionados
  })
  
  #Desde aquí en adelante puede manipular, pero hagalo con cuidado y siempre con respaldo
  

  output$contenido_pregunta <- renderUI({
    
    # INTERFAZ: PREGUNTA 1
    if (pantalla() == "p1") {
      
      tagList(
        h2("Pregunta 1"),
        
        h4("¿Cómo varían las ventas de café a lo largo del tiempo?"),
        
        # Deslizador interactivo para recortar o expandir la escala temporal de meses (1 a 24)
        sliderInput("Rango", "Meses", 1, 24, c(3,19)),
        plotOutput("distPlot"),
        div( #Como agregamos un fondo, se tuvo que modificar para que se leyera correctame, tuvimos que añadir un fondo blanco a cada parte
          style = "
      background-color: white;
      padding: 10px;
      border-radius: 0px;
      margin-bottom: 20px;
      
    " ,
          h5("Gráfico de líneas"),
          p("El gráfico de líneas explica de forma visual la manera en la que se mueve a lo largo de los meses
        el promedio de las ventas del café en general, se opto por utilizar el promedio mensual de ventas
        en vez del total de ventas mensual, ya que de esta manera se evita un pequeño sesgo donde los meses
        con mayor cantidad de días tendrían una ligera ventaja al momento de compararlos, pues poseen un mayor
        perído de tiempo"
          )  )
      )
      
      # INTERFAZ: PREGUNTA 2
    } else if (pantalla() == "p2") {
      
      tagList(
        h2("Pregunta 2"),
        h4("¿Qué tipos de café generan mayores ingresos y cantidades de venta?"),
        
        h4("Filtros del gráfico"),
        
        # Diseño de cuadrícula en una sola fila con 3 columnas equitativas (ancho 4 cada una)
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
        tableOutput("tabla_resumen") # Tabla estructurada de los datos del gráfico superior
      )
      
      # INTERFAZ: PREGUNTA 3
    } else if(pantalla()=="p3"){ #aquí añaden la tag list, guíense de la pregunta 1 y 2
      
      tagList(
        h2("Pregunta 3"),
        h4("¿Cuáles ciudades Árabes generan los mayores ingresos por ventas de café durante el tiempo registrado?"),
        
        # Botones de opción única (Radio Buttons) para elegir un café específico o revisar generalidad
        radioButtons(
          inputId = "producto_p3",
          label = "Seleccione el tipo de café:",
          choices = c(
            "Todos los productos" = "Todos",
            sort(unique(datos_pregunta3$Product)) # Clasifica y ordena alfabéticamente las categorías únicas de café
          ),
          selected = "Todos"
        ),
        
        h3("Ingresos acumulados por ciudad"),
        plotOutput("grafico_ciudades", height = "450px"),
        
        hr(),
        
        # Tres recuadros destacados (wellPanel) con estadísticas clave agregadas
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
        
        div( #Como agregamos un fondo, se tuvo que modificar para que se leyera correctame, tuvimos que añadir un fondo blanco a cada parte
          style = "
      background-color: white;
      padding: 15px;
      border-radius: 10px;
      margin-bottom: 20px;
    ",
          tableOutput("tabla_ciudades")
        ), #Div sirve para el fondo blanco, casi la misma lógica que el CSS
        
        h3("Interpretación"),
        
        div(
          style = "
      background-color: white;
      padding: 15px;
      border-radius: 10px;
    ",
          textOutput("texto_explicativo")
        ) #Esto hace que se coloque el fondito blanco div es muy útil para modificar el fondo
      )
      
    }
    # INTERFAZ: PREGUNTA 4
    else if(pantalla()=="p4"){
      tagList(
        h2("Pregunta 4"),
        h4("¿Cómo se comportan las cantidades finales dependiendo de si se usa descuento o no?"),
        br(),
        
        # Filtro de selección múltiple (Checkbox Group) para comparar categorías de descuento
        checkboxGroupInput(
          inputId = "filtro_descuento_p4",
          label = "Seleccione el estado del descuento:",
          choices = c("Con Descuento" = TRUE, "Sin Descuento" = FALSE),
          selected = c(TRUE, FALSE) # Ambas opciones vienen marcadas por defecto
        ),
        
        br(),
        
        plotOutput("grafico_dispersion_p4"),
        br(),
        hr(),
        
        div(
          style= "
          background-color: white;
      padding: 15px;
      border-radius: 10px;
      margin-bottom: 20px;
          ",
          h5("Grafico de dispersión"),
          p("Este gráfico de dispersión permite evaluar la relación entre la cantidad de unidades vendidas y ",
            "las ventas finales obtenidas. Al activar y desactivar los filtros de la consola, se puede apreciar ",
            "visualmente si la aplicación de descuentos desplaza la tendencia de los puntos o altera los patrones de compra."),
          
          h5("Variables"),
          tags$ul( # Genera una lista viñetada desordenada en HTML
            tags$li("Muestra analizada a partir del archivo 'DatasetForCoffeeSales2.csv'."),
            tags$li("Variable 'Used_Discount' clasificada de manera binaria (TRUE para transacciones con rebaja, FALSE para precio regular)."),
            tags$li("Las cantidades finales corresponden al cruce directo entre 'Quantity' y 'Final Sales'.")
          ))
      )
    }
    # INTERFAZ: PREGUNTA 5
    else if(pantalla()=="p5"){ #aquí añaden la tag list, guíense de la pregunta 1 y 2
      tagList(
        h2("Pregunta 5"),
        h4("Análisis de Precio vs Cantidad"),
        # Buscador de rangos dinámicos (Slider con doble control deslizante) basado en los valores numéricos del precio unitario
        sliderInput("rango_precio", 
                    "Seleccionar rango de Precio Unitario:", 
                    min = min(datos_pregunta5$`Unit Price`, na.rm = TRUE), 
                    max = max(datos_pregunta5$`Unit Price`, na.rm = TRUE), 
                    value = c(min(datos_pregunta5$`Unit Price`, na.rm = TRUE), max(datos_pregunta5$`Unit Price`, na.rm = TRUE))),
        br(),
        plotOutput("grafico_p5"),
        br(),
        h3("Resumen de Precio vs Cantidad Vendida"),
        div( #Como agregamos un fondo, se tuvo que modificar para que se leyera correctame, tuvimos que añadir un fondo blanco a cada parte
          style = "
      background-color: white;
      padding: 15px;
      border-radius: 10px;
      margin-bottom: 20px;
    " ,
          tableOutput("tabla_resumen_p5")
        ))
    }
  })

  # Inicio pregunta 2
  
  # Genera el gráfico de barras horizontales de ingresos por tipo de café
  output$grafico_ingresos <- renderPlot({
    
    req(input$pregunta2) # Asegura la ejecución únicamente cuando se selecciona la pregunta 2
    
    ggplot(
      datos_grafico(), # Llama al objeto reactivo filtrado
      aes(
        x = reorder(Product, Total_Ingresos), # Ordena los productos según su nivel de ingresos de manera ascendente
        y = Total_Ingresos,
        fill = Product
      )
    ) +
      geom_col() + # Crea barras
      coord_flip() + # Rota los ejes para transformar el gráfico en barras horizontales
      scale_fill_manual( # Asigna colores fijos específicos a cada categoría de producto de café
        values = c(
          "Brazilian" = "forestgreen",
          "Colombian" = "gold",
          "Costa Rica" = "blue",
          "Ethiopian" = "purple",
          "Guatemala" = "skyblue"
        )
      ) +
      labs(
        title = "Gráfico #2 \n Ingresos totales por tipo de café",
        x = "Tipo de café",
        y = "Ingresos totales"
      ) +
      theme_minimal() # Tema estético limpio y con cuadrícula suave de fondo
  })
  
  # Imprime la tabla resumen asociada a la Pregunta 2
  output$tabla_resumen <- renderTable({
    req(input$pregunta2)
    datos_grafico()
  })
  #Final pregunta 2
  
  
  #Inicio pregunta 1
  # Genera el gráfico de líneas temporal de promedios de venta mensual
  output$distPlot <- renderPlot({
    
    # Subfiltra el dataset promediainador basándose en los extremos numéricos [1] y [2] del sliderInput
    Canijote <- promediainador[
      promediainador$Rango >= input$Rango[1] & #para modificar un x de rango
        promediainador$Rango <= input$Rango[2], #para modificar un x2 de rango
    ]
    
    ggplot(Canijote, aes(x = Rango, y = Promedio)) + #le digo que debe usar y los valores de x y "y"
      geom_line(color = "blue3", linewidth = 2) + # Dibuja la línea azul de tendencia temporal
      geom_point() + # Dibuja los puntos exactos sobre la línea por cada mes
      labs(
        title = "Gráfico #1\n Variación del promedio de venta de café a lo largo del tiempo",
        x = "Meses",
        y = "Promedio"
      ) + #nombres para los lados del gráfico
      scale_x_continuous( # Configura de forma continua el eje X y fuerza la visualización de etiquetas de texto legibles por fecha
        breaks = seq(1, 24, by = 1),
        labels = c("Ene/23","Feb/23","Mar/23","Abr/23","May/23","Jun/23",
                   "Jul/23","Ago/23","Sep/23","Oct/23","Nov/23","Dic/23",
                   "Ene/24","Feb/24","Mar/24","Abr/24","May/24","Jun/24",
                   "Jul/24","Ago/24","Sep/24","Oct/24","Nov/24","Dic/24") #nombres para cada punto de x, así es facil identificar en que fecha se encuentra ubicado
      )
  })
  #Finalización pregunta 1
  #Añada a partir de aquí
  
  # Genera el gráfico de columnas verticales para la Pregunta 3 (Ciudades)
  output$grafico_ciudades <- renderPlot({
    
    resumen <- resumen_ciudades_p3() # Captura la expresión reactiva procesada
    
    ggplot(
      resumen,
      aes(
        x = reorder(City, -Ingreso_total), # Ordena las ciudades de mayor a menor ingreso acumulado
        y = Ingreso_total,
        fill = City
      )
    ) +
      geom_col(show.legend = FALSE) + # Crea columnas y oculta la leyenda redundante lateral
      scale_y_continuous(labels = dollar_format(prefix = "$")) + # Aplica formato monetario con signo de dólares al eje Y
      labs(
        title = "Gráfico #3\n Ingresos acumulados por ciudad",
        subtitle = "Según el producto de café seleccionado",
        x = "Ciudad",
        y = "Ingresos totales por ventas finales"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1) # Rota los nombres de las ciudades 45 grados para evitar superposiciones
      )
  })
  
  # Calcula e imprime el promedio simple de ventas finales filtrado por producto en P3
  output$promedio_ventas <- renderText({
    
    promedio <- mean(
      if (input$producto_p3 == "Todos") {
        datos_pregunta3$`Final Sales`
      } else {
        datos_pregunta3 %>%
          filter(Product == input$producto_p3) %>%
          pull(`Final Sales`) # Extrae el vector columna directamente para el cálculo aritmético
      },
      na.rm = TRUE
    )
    
    dollar(promedio) # Devuelve el valor con formato de moneda legible
  })
  
  # Calcula la mediana de ventas finales de la Pregunta 3
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
  
  # Identifica y devuelve en texto el nombre y monto consolidado de la ciudad con mejores resultados
  output$mejor_ciudad <- renderText({
    
    resumen <- resumen_ciudades_p3()
    
    req(nrow(resumen) > 0) # Control preventivo en caso de tablas sin observaciones
    
    paste0(
      resumen$City[1], # Toma el valor de la fila 1 (gracias al arrange descendente previo)
      " (",
      dollar(resumen$Ingreso_total[1]),
      ")"
    )
  })
  
  # Renderiza la tabla descriptiva por ciudad formateando los números crudos a dólares
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
  
  # Genera el texto explicativo narrativo automático según los resultados del filtro P3
  output$texto_explicativo <- renderText({
    
    resumen <- resumen_ciudades_p3()
    
    req(nrow(resumen) > 0)
    
    ciudad1 <- resumen$City[1]
    ingreso1 <- resumen$Ingreso_total[1]
    
    if (nrow(resumen) >= 2) {
      
      ciudad2 <- resumen$City[2]
      ingreso2 <- resumen$Ingreso_total[2]
      
      paste0(
        "Para el producto seleccionado, la ciudad con mayor ingreso acumulado is ",
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
  # Renderizar Gráfico de Dispersión Interactivo P4
  # Construye el gráfico de dispersión para observar relaciones entre cantidad vendida y monto final
  output$grafico_dispersion_p4 <- renderPlot({
    ggplot(datos_filtrados_p4(), aes(x = Quantity, y = `Final Sales`, color = Used_Discount)) +
      geom_point(size = 3.5, alpha = 0.7, position = position_jitter(width = 0.15, height = 0)) + # Añade jitter leve en el eje X para dispersar puntos sobrepuestos
      scale_color_manual(
        values = c("TRUE" = "#e74c3c", "FALSE" = "#3498db"),
        labels = c("TRUE" = "Con Descuento", "FALSE" = "Sin Descuento")
      ) +
      labs(
        title = "¿Cómo se comportan las cantidades finales dependiendo de si se usa descuento o no?",
        subtitle = "Gráfico #4\n Análisis de Dispersión: Unidades vs Ventas Finales",
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
  # Construye el gráfico de columnas agrupado por el valor nominal del precio unitario
  output$grafico_p5 <- renderPlot({
    df_p5 <- datos_filtrados_p5() %>%
      group_by(`Unit Price`) %>%
      summarise(Total_Cantidad = sum(Quantity, na.rm = TRUE), .groups = "drop")
    
    ggplot(df_p5, aes(x = factor(`Unit Price`), y = Total_Cantidad, fill = factor(`Unit Price`))) + # Convierte a factor para tratarlo de forma discreta/categórica en el eje X
      geom_col(color = "black") +
      scale_fill_brewer(palette = "YlOrBr") + # Aplica paleta secuencial de colores marrón-amarillo-naranja
      labs(title = " Gráfico #5 \n  Relación Precio Unitario vs Cantidad Total Vendida",
           x = "Precio Unitario ($)", 
           y = "Cantidad Total Vendida",
           fill = "Precio ($)") +
      theme_minimal(base_size = 14)
  })
  
  # Genera la tabla resumen de métricas clave transaccionales cruzando precios en P5
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


shinyApp(ui = ui, server = server) # Llama y asocia la interfaz (UI) y el servidor (Server) para compilar y desplegar la aplicación interactiva 
