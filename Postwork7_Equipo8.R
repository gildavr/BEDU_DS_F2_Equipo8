"Postwork S7
Requisitos
Tener instalado R y RStudio
Haber trabajado con el Prework y el Work
Desarrollo
Utilizando el siguiente vector numérico, realiza lo que se indica:"

url = "https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-07/Data/global.txt"

Global <- scan(url, sep="")

head(Global)
class(Global)

# la funcion scan() Lee datos en un vector o lista desde la consola o archivo.

"1. Crea un objeto de serie de tiempo con los datos de Global. La serie debe ser mensual comenzando 
en Enero de 1856"


Global.ts <- ts(Global, start = c(1856,1), frequency = 12)
class(Global.ts)

"2. Realiza una gráfica de la serie de tiempo anterior"

plot(Global.ts, 
     main = "Datos en el tiempo de V(t)", 
     xlab = "Tiempo",
     ylab = "V(t)",
     sub = "A partir de Enero de 1856 ")

"3. Ahora realiza una gráfica de la serie de tiempo anterior, transformando a la primera diferencia"
diff(Global.ts)

plot(diff(Global.ts), xlab = "", ylab ="" )
title(main = "Datos en el tiempo de V(t)" , xlab = "Tiempo", ylab = "Diff Log-V", 
      sub = "Gráfica de la serie transformando a la primera diferencia")

"4. ¿Consideras que la serie es estacionaria en niveles o en primera diferencia?"
#Es estacionaria en Primera Diferencia

"5.  Con base en tu respuesta anterior, obtén las funciones de autocorrelación y autocorrelación parcial?"

acf(diff(Global.ts))

pacf(diff(Global.ts))

"6. De acuerdo con lo observado en las gráficas anteriores, se sugiere un modelo ARIMA
con AR(1), I(1) y MA desde 1 a 4 rezagos. Estima los diferentes modelos ARIMA propuestos:"
"arima(x, order = c(0L, 0L, 0L))
order = (p, d, q)
p: Número de rezagos en el término de error (del ruido blanco)
d: Número de diferencias aplicadas a la variable para hacerla estacionaria
q: Número de términos autorregresivos (de la variable dependiente)
"
arima(Global.ts, order = c(1, 1, 1))  # 1 rezago
arima(Global.ts, order = c(2, 1, 1))  # 2 rezagos
arima(Global.ts, order = c(3, 1, 1))  # 3 rezagos
arima(Global.ts, order = c(4, 1, 1))  # 4 rezagos

# el mejor modelo es quien tiene el 'aic' mas pequeño. 
# en este ejemplo el mejor es el modelo 1 (1 rezago)

"7.Con base en el criterio de Akaike, estima el mejor modelo ARIMA y realiza una 
predicción de 12 periodos (meses)"

ajuste <- arima(Global.ts, order = c(1, 1, 1))
coefficients(ajuste)  

prediccion <- predict(ajuste, n.ahead = 12 )

prediccion$pred
