# Postwork Sesión 3

#### Objetivo

#- Realizar un análisis descriptivo de las variables de un dataframe

#### Requisitos

#1. R, RStudio
#2. Haber realizado el prework y seguir el curso de los ejemplos de la sesión
#3. Curiosidad por investigar nuevos tópicos y funciones de R

#### Desarrollo

"Utilizando el dataframe `boxp.csv` realiza el siguiente análisis descriptivo. 
No olvides excluir los missing values y transformar las variables a su
tipo y escala correspondiente."

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/boxp.csv")

names(df) #Observamos el nombre de las columnas
head(df) #Información de los primeros 5 datos
View(df) #Ver la tabla en una ventana nueva de R

class(df) #Consulta del tipo de objeto que es DF

summary(df) # Resumen descriptivo del objeto DF

complete.cases(df) # Verificar si hay datos incompletos

sum(complete.cases(df)) #Hay datos incompletos

# Se excluyeron las observaciones que no estuvieran completas en sus tres variables
# Se obtuvo un nuevo df con 591 observaciones completas. 

#Metodo 1 para limpiar registros
df.clean <- df[complete.cases(df),] #Limpiar registros incompletos

#Metodo 2 para limpiar registros
df.clean.2 <- na.omit(df) #Limpiar registros incompletos

df.clean.2


#Conversión de datos a tipo factor
df.clean.2$Categoria <- factor(df.clean.2$Categoria)
df.clean.2$Grupo <- factor(df.clean.2$Grupo)


#1) Calcula e interpreta las medidas de tendencia central de la variable `Mediciones`

df.clean.2.media = mean(df.clean.2$Mediciones)
df.clean.2.media

#Media: 62.88

df.clean.2.mediaTruncada= mean(df.clean.2$Mediciones, trim = 0.25)
df.clean.2.mediaTruncada

#Media truncada : 50.05

max(df.clean.2$Mediciones)- min(df.clean.2$Mediciones)
#Rango : 287.8


df.clean.2.mediana = median(df.clean.2$Mediciones)
df.clean.2.mediana

#Mediana : 49.3

library(DescTools)

df.clean.2.moda = Mode(df.clean.2$Mediciones)
df.clean.2.moda


#Moda: 23.3

#Interpretación. 
# La variable Mediciones tiene una media con valor 62.88 lo que indica que los datos
# tienden a ese valor. 
# La mediana indica que por arriba del valor 49.3 se encuentran el 50% de los datos,
# el otro 50% de los datos se encuentran por debajo de dicho valor.
# La moda indica que el valor que más se repite es 23.3 con 6 observaciones en total. 


#2) Con base en tu resultado anterior, ¿qué se puede concluir respecto al 
#sesgo de `Mediciones`?

"R: Hay valores muy altos (outliners) que alteran la media hacia arriba, 
la media truncada en 0.25 se acerca más a la mediana.

Moda < mediana < media
El sesgo es hacia la derecha" 

library(moments)
skewness(df.clean$Mediciones)

#La función skewness() de la librería moments
# comprueba el sesgo a la derecha debido a que se obtuvo un valor mayor a cero (1.73)

#3) Calcula e interpreta la desviación estándar y los cuartiles de 
#la distribución de `Mediciones`

df.desvEst <- sd(df.clean.2$Mediciones)
df.desvEst

df.cuartiles <- quantile(df.clean.2$Mediciones, probs = c(0.25,0.5,0.75))
df.cuartiles

# El 25% de las mediciones tiene un valor menor o igual a 23.45
# El 50% de las mediciones tiene un valor menor o igual a 49.30
# El 75% de las mediciones tiene un valor menor o igual a 82.85

"Interpretación: El valor de 53.76972 de la desviación estándar 
comparado con  los valores de la media,
mediana y los cuartiles , es muy grande, por lo que 
nos indica que los datos están muy dispersos"

#IQR :59.4
#Rango : 287.8

#El rango es mas de 4 veces mayor que el IQR.
#otra señal de la dispersión de los datos

"4) Con ggplot, realiza un histograma separando la distribución de `Mediciones` por `Categoría`
¿Consideras que sólo una categoría está generando el sesgo?"


k = ceiling(sqrt(length(df.clean.2$Mediciones)))

g <- ggplot(df.clean.2, aes(x = Mediciones,  color = Categoria, fill = Categoria))+
  geom_histogram(bins = k, alpha = 0.5)+
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25))+
  labs(title = "Histograma de frecuencias. \nVariable: Mediciones", 
       x = "Mediciones por Categoria", 
       y = "Frecuencia") +
  theme_classic()
g
# En el histograma se observa que los datos de la categoria C3 tienen un rango
#mayor , mientra que los de la categoria C1 y C2 tienen a estar mas concentrados,
#en el sesgo de la derecha


# De forma adicional, se puede graficar una curva de densidad de la variable 
# 'Mediciones' para validar la distribución de los datos por 'Categoría'


g2 <- ggplot(df.clean, aes(x = Mediciones, color = Categoria, fill = Categoria)) +
  geom_density(alpha = 0.2) + 
  scale_y_continuous(limits =c(0, 0.02)) +
  labs(title = "Densidad de frecuencias. \nVariable: Mediciones", 
       x = "Mediciones por Categoria", 
       y = "Densidad") +
  theme_classic()
g2



"5) Con ggplot, realiza un boxplot separando la distribución de `Mediciones` por `Categoría` 
y por `Grupo` dentro de cada categoría. ¿Consideras que hay diferencias entre categorías? ¿Los grupos al interior de cada categoría 
podrían estar generando el sesgo?"


g3<-ggplot(df.clean.2, aes(y=Mediciones,x=Categoria, color = Grupo) )+ 
  geom_boxplot()+
  labs( title= "Boxplot distribución de los datos por categoria y grupo")
  theme_classic()
  
  g3

  # Interpretación.
  # Se puede observar que las categorías C2 y C3 del grupo 'Uno' tienen una mediana
  # mayor que las medianas de las mismas categorías del grupo 'Dos'. Para la categoría
  # C1, la diferencia no es tan significativa en ambos grupos.
  # También se puede ver que el sesgo a la derecha en la distribución de los datos se 
  # podría deber al comportamiento que tienen las categorías del grupo 'Uno' y la 
  # cantidad de valores atípicos presentes para todos los grupos y categorías. 
  
  # Adicionamente, se pueden realizar gráficos de boxplot cambiando el acomodo de las 
  # variables pero la interpretación se mantiene.  
  
  g4 <- ggplot(df.clean.2, aes(x = Mediciones, y = Categoria, color = Grupo))+
    geom_boxplot()
  g4
  
  "¿Los grupos al interior de cada categoría podrían estar generando el sesgo?"
  #si, porque en el diagrama boxplot se observa que las cajas
  #del grupo 0 están mas largas que las del grupo 1, lo que implica
  #que los datos estan mas alejados de la mediana.
  
  #También hay mas valores atipicos en el grupo 0 que en grupo 1
