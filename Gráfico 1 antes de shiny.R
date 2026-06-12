1. ¿Cómo varían las ventas de café a lo largo del tiempo?
  Variables:
  ●	Date
●	Final Sales

Estructura de Shiny:
  Gráfico: Gráfico de líneas (line chart) con puntos marcados 
Indicadores: totales
Filtros Interactivos:  rango de fecha(año y mes)
Elementos complementarios: texto explicativo

Justificación
Ayudará a Identificar y analizar las tendencias de ventas mensuales permitiendo de esta manera una visualización clara a cerca de en qué momentos hubieron mayores y menor niveles de venta del café


jorgito <- read.csv("~/Estadistica/proyecto progra/DatasetForCoffeeSales2 (1).csv")
View(jorgito)
#Planteamiento mental:
#Requiereo entender que hace cada variable de las que voy a utilizar
#Date: fecha de compra
##Final sales: venta final

#2, Requiero poder cambiar el formato de fecha para que R lo use como la variable x
library("ggplot2")
library("dplyr")
jorgito$Date <- as.Date(jorgito$Date, format = "%m/%d/%Y")

class(jorgito$Date)

#Gráfico 1 prueba 1
df = data.frame( jorgito$Date, jorgito$Final.Sales )

ggplot( df, aes( x = jorgito.Date, y = jorgito.Final.Sales) ) + #Data + Mapping
  geom_line( color = "green4" ) + #Layers
  geom_point() + #Layers
  labs( title = "Variacion del promedio de venta de café a lo largo del tiempo ", x = "Tiempo", y = "Promedio"  ) 
#Funciona, pero se ve horrible, que tal si los separo por promedios mensuales?


#3 que sea por pormedios mensuales de cada año

promediainador <- df %>% 
  mutate(
    Año = format(jorgito.Date, "%Y"),
    Mes = format(jorgito.Date, "%m")
  ) %>% #agarre cada fecha por mes y año y separelas
  group_by(Año, Mes) %>%
  summarise(
    Promedio = mean(jorgito.Final.Sales, na.rm = TRUE), #promediainador será dos columnas, una por de meses y otra por años y tendra un promeido de las ventas finales en cada mes
    .groups = "drop"
  )

#probar como sé ve, Gráfico 1 prueba 2
ggplot( promediainador, aes( x = Mes, y = Promedio) ) + 
  geom_line( color = "green4" ) +
  geom_point() + 
  labs( title ="Variacion del promedio de venta de café a lo largo del tiempo ", x = "Meses", y = "Promedio"  ) 
#Necesito que Año o mes no colapsen y funciónen como una variable númerica donde sirva para separar por mes la variable x del gráfico


Ramgo=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24) #simplemente agruegue una columna, con números del 1 al 24 que era la cantidad de variables o filas que habían, y problema arreglado



promediainador$Rango<-Ramgo
#probando el gráfico, Gráfico 1 prueba 3


ggplot( promediainador, aes( x = Rango, y = Promedio) ) + 
  geom_line( color = "green4",linewidth=2  ) +
  geom_point() + 
  labs( title ="Gráfico 1",subtitle = "Variacion del promedio de venta de café a lo largo del tiempo ", x = "Meses", y = "Promedio" )+
  scale_x_continuous( breaks = seq(1, 24, by = 1 ),labels = c("Ene/23", "Feb/23", "Mar/23", "Abr/23", "May/23", "Jun/23",
                                                             "Jul/23", "Ago/23", "Sep/23", "Oct/23", "Nov/23", "Dic/23",
                                                             "Ene/24", "Feb/24", "Mar/24", "Abr/24", "May/24", "Jun/24",
                                                             "Jul/24", "Ago/24", "Sep/24", "Oct/24", "Nov/24", "Dic/24")
  ) 
  
 


































