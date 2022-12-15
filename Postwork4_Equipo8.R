#POSTWORK SESIÓN 4 Postwork: Llamadas internacionales

#Utilizando la variable total_intl_charge de la base de datos
#telecom_service.csv de la sesión 3, realiza un análisis probabilístico. Para ello,
#debes determinar la función de distribución de probabilidad que más se
#acerque el comportamiento de los datos. Hint: Puedes apoyarte de medidas
#descriptivas o técnicas de visualización.

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")
str(df)
intl_ch_mean <- mean(df$total_intl_charge)
intl_ch_sd <- sd(df$total_intl_charge)
hist(df$total_intl_charge)
# Por el tipo de datos, que son variables aleatorias continuas, las medidas descriptivas y el histograma
# podemos determinar que la función de distribución de probabilidad que más se le acerca es la distribución normal


#Una vez que hayas seleccionado el modelo, realiza lo siguiente:
#  1. Grafica la distribución teórica de la variable aleatoria total_intl_charge
curve(dnorm(x, mean = intl_ch_mean, sd = intl_ch_sd), from = 0, to = 5
      ,col="blue", main = "Densidad Normal:\nTotal International Charge")

#2. ¿Cuál es la probabilidad de que el total de cargos internacionales sea
#menor a 1.85 usd?
pnorm(q = 1.85, mean = intl_ch_mean, sd = intl_ch_sd)
# [1] 0.1125002

#  3. ¿Cuál es la probabilidad de que el total de cargos internacionales sea
#mayor a 3 usd?
pnorm(q = 3, mean = intl_ch_mean, sd = intl_ch_sd, lower.tail = FALSE)
# [1] 0.3773985

#  4. ¿Cuál es la probabilidad de que el total de cargos internacionales esté
#entre 2.35usd y 4.85 usd?
pnorm(q = 4.85, mean = intl_ch_mean, sd = intl_ch_sd) - pnorm(q = 2.35, mean = intl_ch_mean, sd = intl_ch_sd)
# [1] 0.7060114

#  5. Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales
#más alto que podría esperar?
qnorm(p = 0.48, mean = intl_ch_mean, sd = intl_ch_sd)
# [1] 2.726777

# 6. ¿Cuáles son los valores del total de cargos internacionales que dejan
# exactamente al centro el 80% de probabilidad?

qnorm(p = 0.10, mean = intl_ch_mean, sd = intl_ch_sd); qnorm(p = 0.90, mean = intl_ch_mean, sd = intl_ch_sd)
