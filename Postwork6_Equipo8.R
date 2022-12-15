# Postwork - Sesión 6. 

# Supongamos que nuestro trabajo consiste en aconsejar a un cliente sobre como 
# mejorar las ventas de un producto particular, y el conjunto de datos con el 
# que disponemos son datos de publicidad que consisten en las ventas de aquel 
# producto en 200 diferentes mercados, junto con presupuestos de publicidad para
# el producto en cada uno de aquellos mercados para tres medios de comunicación 
# diferentes: TV, radio, y periódico. No es posible para nuestro cliente incrementar
# directamente las ventas del producto. Por otro lado, ellos pueden controlar el 
# gasto en publicidad para cada uno de los tres medios de comunicación. Por lo 
# tanto, si determinamos que hay una asociación entre publicidad y ventas, entonces 
# podemos instruir a nuestro cliente para que ajuste los presupuestos de publicidad, 
# y así indirectamente incrementar las ventas.

# En otras palabras, nuestro objetivo es desarrollar un modelo preciso que pueda
# ser usado para predecir las ventas sobre la base de los tres presupuestos de
# medios de comunicación. Ajuste modelos de regresión lineal múltiple a los datos 
# advertisement.csv y elija el modelo más adecuado siguiendo los procedimientos 
# vistos.

# Considera:

# Y: Sales (Ventas de un producto) [Variable dependiente]
# X1: TV (Presupuesto de publicidad en TV para el producto) [Variable independiente]
# X2: Radio (Presupuesto de publicidad en Radio para el producto) [Variable independiente]
# X3: Newspaper (Presupuesto de publicidad en Periódico para el producto) [Variable independiente]



# Lectura de datos y revisión de la estructura del df. 
adv <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-06/data/advertising.csv")
head(adv)
str(adv)
summary(adv)
str(na.omit(adv))
# Se observa un df con 200 observaciones y 4 variables numericas, no se 
# existen datos 'NA'.



# Generación de matriz de correlación entre variables dependiente e independiendes. 
round(cor(adv), 4)
#               TV  Radio Newspaper  Sales
# TV        1.0000 0.0548    0.0566 0.9012
# Radio     0.0548 1.0000    0.3541 0.3496
# Newspaper 0.0566 0.3541    1.0000 0.1580
# Sales     0.9012 0.3496    0.1580 1.0000
# Se puede observar que X1 (TV) tiene la mayor correlación con Y (Sales) (0.9012), 
# posteriormente X2 (Radio) con 0.3496 y por último X3 (Newspaper) con 0.158. 
# En todos los casos la correlación es positiva. 



# Gráficas de dispersión entre la variable dependiente e independientes con la 
# función pairs ().
pairs(~ Sales + TV + Radio + Newspaper, 
      data = adv, gap = 0.4, cex.labels = 1.5)
# De forma visual se observa que hay una tendencia positiva entre las variables
# TV y Sales, lo cual coincide con la matriz de correlación. En cuanto a las 
# variables Radio y Newspaper respecto a Sales, la tendencia en menos clara. 



# Estimación por Mínimos Cuadrados Ordinarios (OLS).

# Se planterán distintos modelos lineales para determinar cual de todos es 
# el que mejor precide las ventas en función del presupuesto destinado a 
# publicidad en diferentes medios de comunicación. 

# La ecuación de la recta del modelo 1 (m1) queda de la siguiente forma: 
# Sales = beta0 + beta1*TV + beta2*Radio + beta3*Newspaper + e


# Estimación del MODELO 1: m1. 
attach(adv)
m1 <- lm(Sales ~ TV + Radio + Newspaper)

# Análisis del modelo 1 (m1)
summary(m1)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.6251241  0.3075012  15.041   <2e-16 ***
# TV          0.0544458  0.0013752  39.592   <2e-16 ***
# Radio       0.1070012  0.0084896  12.604   <2e-16 ***
# Newspaper   0.0003357  0.0057881   0.058    0.954    

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 1.662 on 196 degrees of freedom
# Multiple R-squared:  0.9026,	Adjusted R-squared:  0.9011 
# F-statistic: 605.4 on 3 and 196 DF,  p-value: < 2.2e-16

# Planteamiento de hipótesis modelo 1 (m1): 
# Ho: El modelo no es valido (beta_i = 0). 
# Ha: El modelo es valido (beta_i =!0). 
# RESULTADOS: 
# Valor de p-value general del modelo: 2.2e-16. 
# Toma de decisión: Para cualquier nivel de significancia estándar existe 
# evidencia estadística para rechazar Ho; por lo que se puede concluir que el 
# modelo 1 (m1) es valido en general.
#
# Por otro lado, la variable menos representativa es 'Newspaper' ya que su valor 
# de Pr(>|t|) es  0.954 que es mayor que cualquier valor de significancia estándar, 
# por lo que se puede concluir que se puede descartar esta variable del modelo.
#
# Valor de Adjusted R-squared:  0.9011. 
# Se puede concluir que en modelo 1 (m1) las variables independientes explican el 
# 90.11% del comportamiento de la variable dependiente. 


# Estimación MODELO 2: m2.
# La ecuación de la recta del modelo 2 (m2) queda de la siguiente forma:  
# Sales = beta0 + beta1*TV + beta2*Radio + e

# Se realiza la actualización de las variables a procesar.  
m2 <- update(m1, ~.-Newspaper)

# Análisis del modelo 2 (m2). 
summary(m2)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.630879   0.290308   15.95   <2e-16 ***
# TV          0.054449   0.001371   39.73   <2e-16 ***
# Radio       0.107175   0.007926   13.52   <2e-16 ***

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 1.657 on 197 degrees of freedom
# Multiple R-squared:  0.9026,	Adjusted R-squared:  0.9016 
# F-statistic: 912.7 on 2 and 197 DF,  p-value: < 2.2e-16

# Planteamiento de hipótesis modelo 2 (m2): 
# Ho: El modelo no es valido (beta_i = 0). 
# Ha: El modelo es valido (beta_i =!0). 
# RESULTADOS: 
# Valor de p-value general del modelo m2: 2.2e-16. 
# Toma de decisión: Para cualquier nivel de significancia estándar existe 
# evidencia estadística para rechazar Ho; por lo que se puede concluir que el 
# modelo m2 es valido en general.
#
# Los valores de Pr(>|t|) para TV y Radio (2.2e-16 para ambas) aportan evidencia
# estadística suficiente para rechazar Ho y concluir que estas variables son 
# significativas para el modelo.  
#
# Valor de Adjusted R-squared:  0.9016. 
# Se puede concluir que en modelo 2 (m2) las variables independientes explican el 
# 90.16% del comportamiento de la variable dependiente. Lo cual es ligeramente 
# mayor respecto al modelo 1 (m1).  


# Estimación MODELO 3: m3.
# Se estimo un tercer y último modelo (m3) para ver el comportamiento por separado 
# las variables que presenta la mayor correlación (Sales-TV).  
# La ecuación de la recta del modelo 3 (m3) queda de la siguiente forma:  
# Sales = beta0 + beta1*TV + e

# Se realiza la actualización de las variables a procesar.  
m3 <- update(m2, ~.- Radio)

# Análisis del modelo 3 (m3). 
summary(m3)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 6.974821   0.322553   21.62   <2e-16 ***
# TV          0.055465   0.001896   29.26   <2e-16 ***

# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 2.296 on 198 degrees of freedom
# Multiple R-squared:  0.8122,	Adjusted R-squared:  0.8112 
# F-statistic: 856.2 on 1 and 198 DF,  p-value: < 2.2e-16

# Planteamiento de hipótesis modelo 3 (m3): 
# Ho: El modelo no es valido (beta_i = 0). 
# Ha: El modelo es valido (beta_i =!0). 
# RESULTADOS: 
# Valor de p-value general del modelo m3: 2.2e-16. 
# Toma de decisión: Para cualquier nivel de significancia estándar existe 
# evidencia estadística para rechazar Ho; por lo que se puede concluir que el 
# modelo m3 es valido en general.
#
# El valor de Pr(>|t|) para TV es de 2.2e-16 lo cual aporta evidencia
# estadística suficiente para rechazar Ho y concluir que esta variable es
# significativa para el modelo.  
#
# Valor de Adjusted R-squared: 0.8112. 
# Se puede concluir que en el modelo 3 (m3) la variable independiente explica el 
# 81.12% del comportamiento de la variable dependiente.  
# En terminos del valor 'Adjusted R-squared' de los tres modelos desarrollados se 
# puede afirmar que el modelo más adecuado es el modelo 2 (m2). 


# Supuestos de los residuos a comprobar. 
# 1) El término de error no tiene correlación significativa con las variables 
#    explicativas. En caso contrario, tendríamos un problema de endogeneidad.
# 2) El término de error sigue una distribución normal.
StanRes <- rstandard(m2)
par(mfrow = c(2, 2))
plot(m2$fitted.values, Sales, ylab = "Valores ajustados")
plot(TV, StanRes, ylab = "Residuales Estandarizados")
# Para la variable TV al parecer existe una correlación media con los residuales
plot(Radio, StanRes, ylab = "Residuales Estandarizados")

# Prueba de normalidad
qqnorm(StanRes)
qqline(StanRes)
# Graficamente los residuos se ajustan a una distribución normal

# Ho: La variable distribuye como una normal
# Ha: La variable no distribuye como una normal
shapiro.test(StanRes)
# Existe evidencia estadística para no rechazar Ho a un 95% de confianza, por lo
# que los residuos se distribuyen normalmente

# Una vez validados estos supuestos, podemos utilizar nuestro modelo estimado 
# para realizar predicciones y obtener su intervalo de confianza
data <- data.frame(
  TV = c(281.4,8.6,220.3,18.4),
  Radio = c(28.8,49.4,28.1,19.2)
)
# Resultados
predict(m2, newdata = data, interval = "confidence", level = 0.95)


# CONCLUSIÓN FINAL. 
# EL mejor modelo que se pudo desarrollar fue el m2 por las siguientes razones. 
# 1. El modelo es valido de acuerdo al valor de p-value general calculado. 
# 2. El modelo es capaz de predecir el 90.16% del comportamiento de las ventas 
#    a partir de los presupuestos destinados para TV y Radio. 
# 3. Se aconseja reducir o eliminar el presupuesto destinado a publicidad vía
#    Newspaper ya que no existe evidencia estadística suficiente para validar 
#    que este rubro tenga un impacto significativo en las ventas.
# 4. El modelo 2 (m2) queda de la siguiente forma: 
#     Sales = 4.6308 + 0.0544*TV + 0.1071*Radio