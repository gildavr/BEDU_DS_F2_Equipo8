# Postwork sesión 8. Análisis de la Inseguridad Alimentaria en México

# OBJETIVO
# Realizar un análisis estadístico completo de un caso
# Publicar en un repositorio de Github el análisis y el código empleado

# REQUISITOS
# Haber realizado los works y postworks previos
# Tener una cuenta en Github o en RStudioCloud

# DESARROLLO
# Un centro de salud nutricional está interesado en analizar estadísticamente y
# probabilísticamente los patrones de gasto en alimentos saludables y no saludables
# en los hogares mexicanos con base en su nivel socioeconómico, en si el hogar
# tiene recursos financieros extrar al ingreso y en si presenta o no inseguridad
# alimentaria. Además, está interesado en un modelo que le permita identificar los
# determinantes socioeconómicos de la inseguridad alimentaria.

# La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012)
# levantada por el Instituto Nacional de Salud Pública en México. La mayoría de
# las personas afirman que los hogares con menor nivel socioeconómico tienden
# a gastar más en productos no saludables que las personas con mayores niveles
# socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar
# presente cierta inseguridad alimentaria.

# La base de datos contiene las siguientes variables:
# nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio",
# 4 "Medio alto", 5 "Alto"
# area (Zona geográfica): 0 "Zona urbana", 1 "Zona rural"
# numpeho (Número de persona en el hogar)
# refin (Recursos financieros distintos al ingreso laboral): 0 "no", 1 "sí"
# edadjef (Edad del jefe/a de familia)
# sexoje (Sexo del jefe/a de familia): 0 "Hombre", 1 "Mujer"
# añosedu (Años de educación del jefe de familia)
# ln_als (Logarítmo natural del gasto en alimentos saludables)
# ln_alns (Logarítmo natural del gasto en alimentos no saludables)
# IA (Inseguridad alimentaria en el hogar): 0 "No presenta IA", 1 "Presenta IA"

library(skimr)
library(tidyverse)
library(cowplot)
library(corrplot)

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-08/Postwork/inseguridad_alimentaria_bedu.csv")

# 1.-Plantea el problema del caso


# 2.-Realiza un análisis descriptivo de la información
# Revisamos la estructura del Dataframe
str(df);glimpse(df)

# Usamos funciones que nos regresen los estadísticos del Dataframe
skim_without_charts(df);summary(df)

# Observamos los primeros y los últimos datos del Dataframe
head(df);tail(df)

# Valores nulos por columna
sapply(df, function(x) sum(is.na(x)))
# Valores nulos por renglón
sum(!complete.cases(df))

# Se quitan los valores nulos
df1 <- drop_na(df)
skim_without_charts(df1)
summary(df1)
head(df1);tail(df1)

# nse5f (Nivel socieconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio",
# 4 "Medio alto", 5 "Alto"
# revisamos los datos
table(df1$nse5f)
# graficamos la variable
ggplot(df1,aes(nse5f)) + geom_bar(fill = "blue") +
  labs(x = "Niveles",y = "Observaciones",title = "Nivel Socioeconómico del Hogar")
# El nivel "Medio alto" tiene 4,364 observaciones siendo la categoría que contiene el
# mayor número de observaciones
# El nivel "Bajo" tiene 3,553 observaciones siendo la categoría con menos observaciones


# area (Zona geográfica): 0 "Zona urbana", 1 "Zona rural"
# revisamos los datos
table(df1$area)
# graficamos la variable
ggplot(df1,aes(area)) + geom_bar(fill = "blue") +
  labs(x = "Zona",y = "Observaciones",title = "Zona Geográfica")
# La zona urbana tiene el mayor número de observaciones con 13,959, para la zona
# rural se tienen 6,321 observaciones

# numpeho (Número de persona en el hogar)
table(df1$numpeho)
# graficamos la variable
ggplot(df1,aes(numpeho)) + geom_bar(fill = "blue") +
  labs(x = "Número de Personas",y = "Observaciones",title = "Número de Personas en el Hogar")
# 19 personas en el hogar es número máximo en las observaciones teniendo un registro
# de 2 observaciones, la categoría con el mayor número de observaciones es de 4
# personas en el hogar con 4,857 registros.

# refin (Recursos financieros distintos al ingreso laboral): 0 "no", 1 "sí"
table(df1$refin)
# graficamos la variable
ggplot(df1,aes(refin)) + geom_bar(fill = "blue") +
  labs(x = "Recursos Financieros",y = "Observaciones",title = "Recursos Financieros Distintos al Ingreso Laboral")
# 16,421 registros no cuentan con recursos financieros distintos a su ingreso laboral,
# solo 3,859 de ellos cuentan con recursos extras al ingreso laboral

# edadjef (Edad del jefe/a de familia)
summary(df1$edadjef)
# graficamos la variable
ggplot(df1,aes(edadjef)) + geom_histogram(bins = 22,fill = "blue",alpha = 0.9) +
  geom_vline(aes(xintercept=mean(edadjef)),color="red", linetype="dashed", size=1) +
  labs(x = "Edad",y = "Frecuencia",title = "Edad del Jefe de Familia")
# La edad del jefe de familia va desde un rango de los 18 hasta los 101 años, sin embargo,
# la media de edad es de 47.3 años, por otro lado notamos que al menos el 75%
# de los encuestados son menores de 57 años.

# edad por nivel socioeconómico
ggplot(df1,aes(edadjef,color = factor(nse5f),fill = factor(nse5f))) +
         geom_histogram(bins = 22,alpha = 0.5) + 
         facet_grid(nse5f ~ .) +
         labs(x = "Edad",y = "Frecuencia",title = "Edad del Jefe de Familia")

# edad por zona geográfica
ggplot(df1,aes(edadjef,color = factor(area),fill = factor(area))) +
  geom_histogram(bins = 22,alpha = 0.5) + 
  facet_grid(area ~ .) +
  labs(x = "Zona",y = "Frecuencia",title = "Zona Geográfica")

# edad por numero de personas por hogar
head(table(df1$edadjef,df1$numpeho),30)
ggplot(df1,aes(edadjef,color = factor(numpeho),fill = factor(numpeho))) +
  geom_histogram(bins = 22,alpha = 0.5) +
  labs(x = "Número de Personas",y = "Frecuencia",title = "Número de Personas en el Hogar")

# Recursos Financieros Distintos al Ingreso Laboral por edad
ggplot(df1,aes(edadjef,color = factor(refin),fill = factor(refin))) +
  geom_histogram(bins = 22,alpha = 0.5) + 
  facet_grid(refin ~ .) +
  labs(x = "Recursos Financieros",y = "Frecuencia",title = "Recursos Financieros Distintos al Ingreso Laboral")

# edad por sexo del jefe de familia
ggplot(df1,aes(edadjef,color = factor(sexojef),fill = factor(sexojef))) +
  geom_histogram(bins = 22,alpha = 0.5) + 
  facet_grid(sexojef ~ .) +
  labs(x = "Edad",y = "Frecuencia",title = "Sexo del Jefe de Familia")

# edad por años de educación del jefe de familia
head(table(df1$edadjef,df1$añosedu),30)
ggplot(df1,aes(edadjef,color = factor(añosedu),fill = factor(añosedu))) +
  geom_histogram(bins = 22,alpha = 0.5) + 
  labs(x = "Edad",y = "Frecuencia",title = "Años de Educación del Jefe de Familia")

# edad por gasto en alimentos saludables
# Ln
g1 <- ggplot(df1) +
  geom_point(aes(edadjef,ln_als)) +
  geom_vline(aes(xintercept=mean(edadjef)),color="red", linetype="solid", size=1) +
  geom_hline(aes(yintercept=mean(ln_als)),color="blue", linetype="solid", size=1) +
  labs(x = "Edad",y = "Gasto",title = "Gasto en Alimentos Saludables")
# sin Ln
g2 <- ggplot(df1) +
  geom_point(aes(edadjef,exp(ln_als))) +
  geom_vline(aes(xintercept=mean(edadjef)),color="red", linetype="solid", size=1) +
  geom_hline(aes(yintercept=mean(exp(ln_als))),color="blue", linetype="solid", size=1) +
  labs(x = "Edad",y = "Gasto",title = "Gasto en Alimentos Saludables")

plot_grid(g1, g2, labels = c('Ln', 'No Ln'), label_size = 9)

# edad por gasto de alimentos no saludables
# con Ln
g3 <- ggplot(df1) +
  geom_point(aes(edadjef,ln_alns)) +
  geom_vline(aes(xintercept=mean(edadjef)),color="red", linetype="solid", size=1) +
  geom_hline(aes(yintercept=mean(ln_alns)),color="blue", linetype="solid", size=1) +
  labs(x = "Edad",y = "Gasto",title = "Gasto en Alimentos No Saludables")
# sin Ln
g4 <- ggplot(df1) +
  geom_point(aes(edadjef,exp(ln_alns))) +
  geom_vline(aes(xintercept=mean(edadjef)),color="red", linetype="solid", size=1) +
  geom_hline(aes(yintercept=mean(exp(ln_alns))),color="blue", linetype="solid", size=1) +
  labs(x = "Edad",y = "Gasto",title = "Gasto en Alimentos No Saludables")

plot_grid(g3, g4, labels = c('Ln', 'No Ln'), label_size = 9)

# sexoje (Sexo del jefe/a de familia): 0 "Hombre", 1 "Mujer")
table(df1$sexojef)
ggplot(df1,aes(factor(sexojef))) + geom_bar(fill = "blue") +
labs(x = "Sexo",y = "Observaciones",title = "Sexo del Jefe de Familia")
# 15,887 observaciones corresponden a hombres y 4,393 observaciones a mujeres
# en el sexo del jefe de familia

# añosedu (Años de educación del jefe de familia)
table(df1$añosedu)
ggplot(df1,aes(factor(añosedu))) + geom_bar(fill = "blue") +
  labs(x = "años",y = "Observaciones",title = "Años de Educación del Jefe de Familia")
# la categoria con mayores observaciones es 9 años de educación y con 25 
# observaciones es la categoría de 24 años de educación del jefe de familia


# ln_als (Logarítmo natural del gasto en alimentos saludables)
# Ln
skim_without_charts(df1$ln_als)
ggplot(df1,aes(ln_als)) + geom_histogram(fill = "blue",bins = 38)
# No Ln
skim_without_charts(exp(df1$ln_als))
ggplot(df1,aes(exp(ln_als))) + geom_histogram(fill = "blue",bins = 30)

# ln_alns (Logarítmo natural del gasto en alimentos no saludables)
# Ln
skim_without_charts(df1$ln_alns)
ggplot(df1,aes(ln_alns)) + geom_histogram(fill = "blue",bins = 13)
# No Ln
skim_without_charts(exp(df1$ln_alns))
ggplot(df1,aes(exp(ln_alns))) + geom_histogram(fill = "blue",bins = 40)

# IA (Inseguridad alimentaria en el hogar): 0 "No presenta IA", 1 "Presenta IA"
table(df1$IA)
ggplot(df1,aes(factor(IA))) + geom_bar(fill = "blue") + 
  labs(x = "Inseguridad Alimentaria",y = "Observaciones",
       title = "Inseguridad Alimentaria en el hogar")
# 14,427 observaciones padecen inseguridad alimentaria en el hogar y sólo
# 5,853 registros no pertenecen al grupo de inseguridad alimentaria en el hogar

# 3.-Calcula probabilidades que nos permitan entender el problema en México
# Calculamos estadísticos descriptivos de la variable ln_alns
summary(df1$ln_alns)
mean <- mean(df1$ln_alns)
sd <- sd(df1$ln_alns)
# Graficamos para observar la distribución y el boxplot
hg <-ggplot(df1, aes(x = ln_alns)) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "blue") +
  stat_function(fun = dnorm, args = list(mean = mean(df1$ln_alns),
                                         sd = sd(df1$ln_alns)),
                colour = "red",size = 1) +
  labs(x = "Gasto en alimentos no saludables",y = "Densidad",
       title = "Histograma y Función de probabilidad")

bg <- ggplot(df1,aes(ln_alns)) + geom_boxplot(fill = "blue",notch = TRUE) +
  coord_flip() +
  labs(title = "Boxplot")

plot_grid(hg, bg, labels = c('1', '2'), label_size = 9)

# ¿El comportamiento es el mismo si se revisan por grupo de la variable
# "Inseguridad Alimentaria en el hogar"?
# Filtramos el dataframe por inseguridad alimentaria
NIA <- df1 %>% filter(IA == 0)
summary(NIA$ln_alns)
NIAmean <- mean(NIA$ln_alns)
NIAsd <- sd(NIA$ln_alns)

SIA <- df1 %>% filter(IA == 1)
summary(SIA$ln_alns)
SIAmean <- mean(SIA$ln_alns)
SIAsd <- sd(SIA$ln_alns)

# Graficamos para observar la distribución y el boxplot
NIA_hg <-ggplot(NIA, aes(x = ln_alns)) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "blue") +
  stat_function(fun = dnorm, args = list(mean = mean(NIA$ln_alns),
                                         sd = sd(NIA$ln_alns)),
                colour = "red",size = 1) +
  labs(x = "Gasto en alimentos no saludables",y = "Densidad",
       title = "Histograma y Función de probabilidad")

NIA_bg <- ggplot(NIA,aes(ln_alns)) + geom_boxplot(fill = "blue",notch = TRUE) +
  coord_flip() +
  labs(title = "Boxplot")

plot_grid(NIA_hg, NIA_bg, labels = c('1', '2'), label_size = 9)

# Graficamos para observar la distribución y el boxplot
SIA_hg <-ggplot(SIA, aes(x = ln_alns)) + 
  geom_histogram(aes(y =..density..),
                 colour = "black", 
                 fill = "blue") +
  stat_function(fun = dnorm, args = list(mean = mean(SIA$ln_alns),
                                         sd = sd(SIA$ln_alns)),
                colour = "red",size = 1) +
  labs(x = "Gasto en alimentos no saludables",y = "Densidad",
       title = "Histograma y Función de probabilidad")

SIA_bg <- ggplot(NIA,aes(ln_alns)) + geom_boxplot(fill = "blue",notch = TRUE) +
  coord_flip() +
  labs(title = "Boxplot")

plot_grid(SIA_hg, SIA_bg, labels = c('1', '2'), label_size = 9)

# Continuamos calculando probabilidades sobre la variable ln_alns dentro del grupo
# que pertenece a inseguridad alimentaria
x <- seq(-4, 4, 0.01)*SIAsd + SIAmean
y <- dnorm(x, mean = SIAmean, sd = SIAsd)

# ¿Cuál es la probabilidad de que el gasto en alimentos no saludables sea menor a 6 unidades?
pnorm(6,SIAmean,SIAsd)
plot(x, y, type = "l", xlab="", ylab="", xlim = c(0, 8))
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 4.03, " y ", sigma == 1.01)))
polygon(c(min(x), x[x<=6], 6), c(0, y[x<=6], 0), col="blue")

# ¿Cuál es la probabilidad de el gasto en alimentos no saludables sea mayor a 3 unidades?
pnorm(3,SIAmean,SIAsd,lower.tail = FALSE)
plot(x, y, type = "l", xlab="", ylab="", xlim = c(0, 8))
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 4.03, " y ", sigma == 1.01)))
polygon(c(3, x[x>=3], max(x)), c(0, y[x>=3], 0), col="blue")

# ¿Cuál es la probabilidad de que el gasto en alimentos no saludables esté entre 4u. y 5u.?
pnorm(5,SIAmean,SIAsd) - pnorm(4,SIAmean,SIAsd)
plot(x, y, type = "l", xlab="", ylab="", xlim = c(0, 8))
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 4.03, " y ", sigma == 1.01)))  
polygon(c(4, x[x>=4 & x<=5], 5), c(0, y[x>=4 & x<=5], 0), col="blue")

# Con una probabilidad de 0.7, ¿cuál es el gasto en alimentos no saludables más
# alto que podría esperarse?
qnorm(0.7,SIAmean,SIAsd)  
exp(qnorm(0.7,SIAmean,SIAsd))
# El gasto más alto esperado con el 0.7 de probabilidad es de 96.09 unidades monetarias.

# ¿Cuáles son los valores del total del gasto en alimentos no saludables que dejan exactamente al centro el 80%
# de probabilidad?
a <- qnorm(.1,SIAmean,SIAsd)
b <- qnorm(.1,SIAmean,SIAsd,lower.tail = FALSE)
plot(x, y, type = "l", xlab="", ylab="", xlim = c(0, 8))
title(main = "Densidad de Probabilidad Normal", sub = expression(paste(mu == 4.03, " y ", sigma == 1.01)))  
polygon(c(a, x[x>=a & x<=b], b), c(0, y[x>=a & x<=b], 0), col="blue")
pnorm(b,SIAmean,SIAsd) - pnorm(a,SIAmean,SIAsd)
round(exp(a),2);round(exp(b),2)
# Los valores van desde 15.36 unidades monetarias hasta 207.25.

# 4.-Plantea hipótesis estadísticas y concluye sobre ellas para entender el
# problema en México

# 5.-Estima un modelo de regresión, lineal o logístico, para identificiar los
# determinanres de la inseguridad alimentaria en México

# calculamos la matriz de correlación
c1 <- round(cor(df1),3)
# graficamos la matriz de correlación
p.mat <- cor.mtest(df1)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(c1, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat$p, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

# Se propone un modelo de regresión lógistica para tratar de predecir la
# variable IA, es decir, la variable dependiente será IA, también se tomarán
# las variables que tengan un valor > +-0.10 en la matriz de correlación para
# ser variables independientes.
log_m <- glm(IA ~ factor(nse5f) + factor(añosedu) + ln_als +
             ln_alns + factor(area) + factor(numpeho), 
                      data = df1, 
                      family = "binomial")
summary(log_m)
AIC1 <- log_m$aic

# A diferencia del modelo de regresión lineal, no es posible calcular el R2 de una 
# regresión logística, sin embargo, se puede calcular la bondad de ajuste con base en 
# la log-verosimilitud del modelo nulo y el modelo actual
pr2 <- (log_m$null.deviance - log_m$deviance)/log_m$null.deviance
round(pr2,4)

# Realizamos otro modelo quitanto la variable area ya que no fue significativa 
# en el modelo
log_m1 <- glm(IA ~ factor(nse5f) + factor(añosedu) + ln_als +
               ln_alns + factor(numpeho), 
             data = df1, 
             family = "binomial")
summary(log_m1)
AIC2 <- log_m1$aic

p1r2 <- (log_m1$null.deviance - log_m1$deviance)/log_m1$null.deviance
round(p1r2,4)

# Finalmente, ajustaremos un modelo más con las variables con un valor de 
# correlación > +-0.20.
log_m2 <- glm(IA ~ factor(nse5f) + factor(añosedu), 
              data = df1, 
              family = "binomial")
summary(log_m2)
AIC3 <- log_m2$aic

p2r2 <- (log_m2$null.deviance - log_m2$deviance)/log_m2$null.deviance
round(p2r2,4)

# Creamos el dataframe con los resultados de los modelos
AICT <- cbind(round(AIC1,2),round(AIC2,2),round(AIC3,2))
R2T <- cbind(round(pr2,4),round(p1r2,4),round(p2r2,4))
Var <- cbind("6","5","2")
Scores <- data.frame(rbind(AICT,R2T,Var))
colnames(Scores) <- c("Modelo1","Modelo2","Modelo3")
rownames(Scores) <- c("AIC","PR2","No.Variables")
Scores
# Dado los valores del criterio de información Akaike, el pseudo r2 y el número
# de variables en el modelo elegiremos el modelo 1 para realizar predicciones.
data_log <- data.frame(
  nse5f = factor(c(5,2,4,3,3)),
  area = factor(c(1,1,1,0,1)),
  numpeho = factor(c(4,5,2,19,13)),
  añosedu = factor(c(3,12,15,19,24)),
  ln_als = log(c(56,201,145,34,15)),
  ln_alns = log(c(56,122,98,76,200))
)
predict(log_m, newdata = data_log,type = "response")
# Según las probabilidades calculadas por el modelo, las 5 observaciones 
# caen en la categoría de inseguridad alimentaria en el hogar.

# También se creará un modelo de regresión lineal para la variable dependiente
# alns tomando en consideración los valores de correlación > 0.10.
# Modelo de regresión lineal por Estimación por Mínimos Cuadrados Ordinarios (OLS)
attach(df1)
lr <- lm(ln_alns ~ factor(nse5f) + factor(area) + factor(añosedu) + factor(IA) + ln_als)
summary(lr)
# El valor de r2ajustada es de 0.1552 el cual es un valor realmente muy bajo, 
# por lo que determinamos que el modelo no es bueno ni funcional.

# Se ajustará un modelo con sólo una variable independiente para ver cuál es el
# comportamiento del mismo
lr1 <- lm(ln_alns ~ ln_als)
summary(lr1)
# Con solo una variable el valor de r2ajustada es de 0.1078 el cuál no difiere
# con mucho con el modelo con 6 variables.
# Como conclusión, ninguno de los dos modelos es bueno para ser empledo para 
# predicciones.

# 6.-Escribe tu análisis en un archivo README.MD y tu código en un script de R
# y publica ambos en un repositorio de Github.

# NOTA: Todo tu planteamiento deberá estár correctamente desarrollado y deberás
# analizar e interpretar todos tus resultados para poder dar una conclusión final
# al problema planteado.