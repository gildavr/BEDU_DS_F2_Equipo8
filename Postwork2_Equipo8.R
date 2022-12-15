# Postwork Sesión 2.

#### Objetivo

"- Conocer algunas de las bases de datos disponibles en `R`
- Observar algunas características y manipular los DataFrames con `dplyr`
- Realizar visualizaciones con `ggplot`
#### Requisitos

1. Tener instalado R y RStudio
2. Haber realizado el prework y estudiado los ejemplos de la sesión."

#### Desarrollo

"1) Inspecciona el DataSet iris disponible directamente en la librería de ggplot. 
Identifica las variables que contiene y su tipo, asegúrate de que no hayan datos faltantes y 
que los datos se encuentran listos para usarse."

library(ggplot2)

names(iris)
head(iris)

df.iris <- iris

View(iris)

str(iris)

dim(df.iris)
# Tiene 5 variables de las cuales 4 son numéricas y 1 categórica con 3 etiquetas o factores, 
# y 150 observaciones en total en el dataframe

class(iris$Sepal.Length)
class(iris$Sepal.Width)
class(iris$Petal.Length)
class(iris$Petal.Width)

"Devuelve por cada observacion TRUE si la observacion está completa y 
False si le falta algun dato"
df.iris.cc<-complete.cases(iris)

"Haciendo la suma, podemos comparar que el numero de true sea igual al numero de 
observaciones"
sum(df.iris.cc)

"2) Crea una gráfica de puntos que contenga `Sepal.Lenght` en el eje horizontal, 
`Sepal.Width` en el eje vertical, que identifique `Species` por color y que el tamaño 
de la figura está representado por `Petal.Width`. 
Asegúrate de que la geometría contenga `shape = 10` y `alpha = 0.5`."

ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species, size=Petal.Width))+geom_point(shape=10, alpha=0.5)

"3) Crea una tabla llamada `iris_mean` que contenga el promedio de todas las variables 
agrupadas por `Species`."


library(dplyr)

iris_mean <- iris %>%
             mutate(Species = factor(Species, labels = c("setosa", "versicolor", "virginica")))%>%
             group_by(Species)%>%
             summarize(Sepal.Length.mean = mean(Sepal.Length),
                       Sepal.Width.mean = mean(Sepal.Width),
                       Petal.Length.mean = mean(Petal.Length),
                       Petal.Width.mean = mean(Petal.Width))



"4) Con esta tabla, agrega a tu gráfica anterior otra geometría de puntos para agregar 
los promedios en la visualización. Asegúrate que el primer argumento de la geometría 
sea el nombre de tu tabla y que los parámetros sean `shape = 23`, `size = 4`, 
`fill = 'black'` y `stroke = 2`. También agrega etiquetas, temas y los cambios 
necesarios para mejorar tu visualización."

g<-ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species, size=Petal.Width))+geom_point(shape=10, alpha=0.5)

g <- g +  geom_point(data=iris_mean,aes(x=Sepal.Length.mean,
                                        y=Sepal.Width.mean),
                     shape=23,size=4,fill ="black",stroke=2)
g <- g + labs(title = "Comparación de tamaños",
              x = "Largo del sepalo",
              y = "Ancho del sepalo")

g <- g + theme_classic()

