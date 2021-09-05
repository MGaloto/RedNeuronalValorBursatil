
library(corrplot) 
library(PerformanceAnalytics) 
library(quantmod)
library(highcharter)
library(tidyr)
library(miscset)
library(ggplot2)
library(corrplot) 
library(GGally)
library(readr)  
library(dplyr)  
library(crayon) 
library(modeest)
library(plotly)
library(ggthemes)
library(reshape)
library(caret)
library(lubridate)
library(neuralnet)
library(NeuralNetTools)




# Importar acciones

hoy = today()

AAPL=getSymbols("AAPL.BA" , src = 'yahoo', auto.assign = F, from = "2013-07-15", to = hoy, periodicity = "daily")



# Se crean los Data Frames

AAPL = as.data.frame(AAPL)




# Se agregan las Fechas como variables


AAPL$Fecha <- row.names(AAPL)


# Se omite
AAPL = na.omit(AAPL)



# Se crean las columnas

colnames(AAPL) = c("Open", "High", "Low", "Close", "Volumen", "Ajustado", "Fecha")




# Se numeran las filas
rownames(AAPL) = 1:nrow(AAPL)



# Se crean las variaciones de los últimos 5 dias

AAPLdis = round( ( AAPL$Close[length(AAPL$Close)] - AAPL$Close[length(AAPL$Close) - 5] ) / AAPL$Close[length(AAPL$Close) - 5] ,4)*100 

AAPL$Fecha = as.Date(AAPL$Fecha)

# Grafico serie de tiempo de prueba


grafico_aapl <- plot_ly() %>% 
  add_lines(y = log(AAPL$Close) , name = "AAPL", x=AAPL$Fecha ,line = list(shape = "spline")) %>% 
  layout(title = "AAPL en Log", xaxis = list(title = 'Periodo'), yaxis = list (title = 'AAPL'))

grafico_aapl



# pronostico

# Convierto en numerica la variable

AAPL$Ajustado = as.numeric(AAPL$Ajustado)

# Creo un rango de fechas para que sea movil

rango_fecha = (hoy + 1) : (hoy + 30)

precio = as.numeric(NA) # es flexible y se adapta al vector que lo junte

# creo un data frame con las dos variables anteriories

rango_fecha = as.data.frame(cbind(precio, rango_fecha))

rango_fecha$fecha = as.Date(rango_fecha$rango_fecha)

rango_fecha$rango_fecha = NULL

# creo un data frame para el pronostico

AAPL_P = as.data.frame(cbind(precio = AAPL$Ajustado,fecha = AAPL$Fecha))

AAPL_P$fecha = as.Date(AAPL_P$fecha)

# junto los data frame finales

AAPL_P = rbind(AAPL_P, rango_fecha)


# creamos un duplicado de la culumna fecha


AAPL_P$fecha_dup = AAPL_P$fecha


# separamos

AAPL_P = AAPL_P %>% separate(fecha, c('año', 'mes', 'dia'))

# para que las redes neuronales funcionen las variables tienen que ser numéricas

AAPL_P$año = as.numeric(AAPL_P$año)
AAPL_P$mes = as.numeric(AAPL_P$mes)
AAPL_P$dia = as.numeric(AAPL_P$dia)

# redes neuronales, hay que escalar año, mes y día

set.seed(1994) 

AAPL_P.sc = as.data.frame(cbind(AAPL_P$precio, AAPL_P$fecha_dup, scale(AAPL_P[,c(2:4)])))

names(AAPL_P.sc)[1] = 'precio'
names(AAPL_P.sc)[2] = 'fecha'
AAPL_P.sc$fecha = as.Date(AAPL_P.sc$fecha)


set.seed(1994)  # para que no cambie el escalado y R tome la misma aleatoriedad
        

# Tomamos una parte del data set original preservando la estructura
# tomamos solo las filas que tengan datos creando un vector 

train = createDataPartition(na.omit(subset(AAPL_P, AAPL_P$fecha_dup < today()))$precio,
                            p = 0.7, list = F)
# no quiero que sea una lista, que sea un vector, que filtre por precio y la proporción
# que sea del 70% y que no tenga en cuenta los na

# el test va a ser para testear el modelo, le sacamos las filas del train,
# por lo tanto tendra menos filas

test = rbind(AAPL_P[-train, ], subset(AAPL_P, AAPL_P$fecha_dup >= today()))


# hay que escalar tambien el test

test.sc = as.data.frame(cbind(test$precio, test$fecha_dup, scale(test[,c(2,3,4)])))

names(test.sc)[1] = 'precio'
names(test.sc)[2] = 'fecha'
test.sc$fecha = as.Date(test.sc$fecha)


# se construye el modelo

# Primero pasar a numericos

AAPL_P.sc$año = as.numeric(AAPL_P.sc$año)
AAPL_P.sc$mes = as.numeric(AAPL_P.sc$mes)
AAPL_P.sc$dia = as.numeric(AAPL_P.sc$dia)

# neuralnet requiere necesariamente que los datos estén escalados y se requieren variables
# muy explicativas para la variable dependiente


mod = neuralnet(formula = precio ~ año + mes + dia, data = AAPL_P.sc[train,],
                hidden = 2, threshold = 0.01, stepmax = 1e+08, rep = 1,
                linear.output = TRUE) # usamos solo las filas del train

# testeamos el modelo que acabamos de crear con test.sc

plotnet(mod)


prediccion = compute(mod, test.sc)

datos = cbind(pred$net.restult, test.sc)

error_abs = RMSE(datos$precio, datos$pred$net.result, na.rm = TRUE)
error_por = error_abs / datos[datos$fecha == max(na.omit(datos)$fecha),]$precio


ggplot() + geom_line(data = datos, aes(x = fecha, y = precio), color = "blue") +
  geom_line(data = datos, aes(x = fecha, y = pred$net.result), color = "red") 

link = "https://www.youtube.com/watch?v=RBr3yRdE_LA"


