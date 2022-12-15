"Postwork Sesión 5"

"DESARROLLO

El data frame iris contiene información recolectada por Anderson sobre 50 
flores de 3 especies distintas (setosa, versicolor y virginca), 
incluyendo medidas en centímetros del largo y ancho del sépalo así como de 
los pétalos."

"Estudios recientes sobre las mismas especies muestran que:"

"En promedio, el largo del sépalo de la especie setosa (Sepal.Length) 
es igual a 5.7 cm"


"En promedio, el ancho del pétalo de la especie virginica (Petal.Width) 
es menor a 2.1 cm"

"En promedio, el ancho del pétalo de la especie virginica (Petal.Width) 
es menor a 2.1 cm"

"En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más
grande que el promedio del largo del pétalo de la especie versicolor."

"En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies."

"Utilizando pruebas de inferencia estadística, 

concluye si existe evidencia suficiente para concluir que los datos
recolectados por Anderson están en línea con los nuevos estudios."


"Utiliza 99% de confianza para toda las pruebas, en cada caso realiza
el planteamiento de hipótesis adecuado y concluye."


df<-iris

  
"1. En promedio, el largo del sépalo de la especie setosa (Sepal.Length) 
es igual a 5.7 cm"

#Ho : sepal.length.setosa = 5.7
#Ha: sepal.length.setosa != 5.7


t.test(x = df[df$Species == "setosa", "Sepal.Length"], 
       alternative = "two.sided",
       mu = 5.7)

#p-value < 2.2e-16
#p-value < 0.01

# A un nivel de confiaza del 99% cuento con los elementos estadisticos 
#suficientes para rechazar la hipotesis nula

#El promedio del Sepal.Length de la especie setosa , es distinto a 5.7 cm


"2. En promedio, el ancho del pétalo de la especie virginica (Petal.Width) 
es menor a 2.1 cm"

#Ho: Petal.Width.virginica >= 2.1
#Ha: Petal.Width.virginica < 2.1

t.test(x = df[df$Species == "virginica", "Petal.Width"], 
       alternative = "less",
       mu = 2.1)

# p-value = 0.03132
# p-value > 0.01

# A un nivel de confianza del 99% no cuento con los elementos estadisticos
# suficientes para rechazar la hipotesis nula

# El ancho del petalo de la especie virginica es mayor o igual a 2.1 cm

"3. En promedio, el largo del pétalo de la especie virgínica es 1.1 cm más
grande que el promedio del largo del pétalo de la especie versicolor."

#Ho: Petal.Length.virginica <= petal.length.versicolor +1.1
#Ha: Petal.Length.virginica > petal.length.versicolor +1.1

#Primero analizamos si las varianzas son iguales

#Ho : Varianzas iguales
#Ha: Varianzas distintas

var.test(df[df$Species == "virginica", "Petal.Length"], 
         df[df$Species == "versicolor", "Petal.Length"], 
         ratio = 1, alternative = "two.sided")

# p-value = 0.2637
# p-value > 0.01

# A un nivel de confianza del 99% no tengo elementos estadisticos suficientes
# para rechazar la hipoteis nula , por lo que las varianzas son iguales

t.test(x = df[df$Species == "virginica", "Petal.Length"],
       y = df[df$Species == "versicolor", "Petal.Length"],
       alternative = "greater",
       mu = 1.1, var.equal = TRUE)

# p-value = 0.03202
# p-value > 0.01 No rechazo Ho

# A un nivel de confianza del 99% no cuento con los elementos estadisticos
#suficientes para rechazar Ho

# El promedio del largo del petalo de la virginica es menor o igual  1.1 cm mas
# que el promedio del largo del petalo del a versicolor


"4. En promedio, no existe diferencia en el ancho del sépalo entre las 3 especies."

#Ho: Sepal.With.setosa = Sepal.with.virginica = sepal.with= versicolor
#Ha:  Al menos una es distinta


boxplot(Petal.Width ~ Species,
        data = df)

anova <- aov(Petal.Width ~ Species,
             data = df)

summary(anova)

#Pr(>F)<2e-16
#Pr(>F) < 0.01 Rechazo Ho

#Con un nivel de confianza del 99% , tenemos los elementos estadisticos
#para rechazar Ho.

# El ancho del petalo entre las tres especies es distinto
