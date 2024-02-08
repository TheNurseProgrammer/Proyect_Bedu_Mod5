library("ggplot2")
library("zoo")
library("dplyr")
library("forecast")
library("astsa")
library("tidyverse")
library("lubridate")
library("tseries")

df <- read.csv ("./superstore_data_limpio.csv")

head (df)
summary(df)
colnames(df)

# Función para sumar el ganacias por mes
sum_profit <- function(data) {
  # Nos aseguramos que 'order_date' está en formato de fecha
  data$order_date <- as.Date(data$order_date)
  # Agrupa los datos por mes y suma la ganacia
  result <- data %>%
    group_by(year_month = format(order_date, "%Y-%m")) %>%
    summarize(total_profit = sum(profit, na.rm = TRUE))
  return(result)
}

result <- sum_profit (df)
result
#Se aplica cambio de formato a la fecha
result$year_month <- as.yearmon(result$year_month)
result

#!<-------Inicio de analisis con series de tiempo y visualizacion------->

# Crear una serie de tiempo y se visualiza
time_series <- ts(result$total_profit, start = c(min(result$year_month), 1), frequency = 12)

plot (time_series, type = "o", lty = "dashed",col ="red", main= "Ganancias")

#Separar los componentes de la serie de tiempo y visualizarla   
time_series_compose <- decompose (time_series, type = "mult")
summary (time_series_compose)


plot(time_series_compose, type = "o" , lty = "dashed",col ="purple", xlab = "Tiempo", 
     sub = "Descomposición de los datos de la ganacia",cex.main=1.5, cex.sub=1.2, bg = "lightgray")
axis.Date(1, at = time_series, format = "%b %Y", las = 2)

#Se aplica la prueba de Dickey & Fuller
#El p-value es de 0.01 lo que indica que laseries es estacionaria para aplicar ARIMA
adf.test(time_series,alternative="stationary")

par(mfrow=c(2,1),mar = c(4,4,4,1)+.1)
acf(ts(time_series,frequency = 1))
pacf(ts(time_series,frequency = 1))

arima_model <- arima(time_series, order = c(5, 0, 5))

summary(arima_model)
tsdiag (arima_model)
# Obtener los residuos del modelo ARIMA
residuos_arima <- residuals(arima_model)
plot (residuos_arima)
# Realizar el test de Ljung-Box sobre los residuos
Box.test(residuos_arima, type = "Ljung-Box")
# Se realiza la prediccion con el modelo y se grafica
predicciones <- forecast(arima_model, h = 6)
plot(predicciones, main = "Predicciones ARIMA", xlab = "Fecha", ylab = "Total Profit")

#!<-------Final de analisis con series de tiempo y visualizacion------->


#!<-------Inicio de prueba de hipotesis y visualizacion------->
contingency_table <- table(df$sub.category, df$market)

# Realizar la prueba de chi cuadrado
result_chi_square <- chisq.test(contingency_table)

# Mostrar los resultados
print(result_chi_square)

#!<-------Fin de prueba de hipotesis y visualizacion------->