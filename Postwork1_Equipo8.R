#1
#Importa los datos de soccer de la temporada 2019/2020 de la primera división de la liga española a R,
#los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php
df = read.csv(file.choose(),header = TRUE)
head(df)
tail(df)
#2
#Del data frame que resulta de importar los datos a R, extrae las columnas que contienen los números de goles anotados
#por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG)
names(df)
# Dataframe con nombres
df1 <- df[,c("HomeTeam","AwayTeam","FTHG","FTAG")]
colnames(df1) <-c("EqCasa","EqVisitante","GolCasa","GolVisitante")
names(df1)
str(df1)
summary(df1)




# Dataframe sin nombres para postwork
df2 <- df[,c("FTHG","FTAG")]
colnames(df2) <-c("GolCasa","GolVisitante")
names(df2)
str(df2)
summary(df2)

#3
#Consulta cómo funciona la función table en R al ejecutar en la consola ?table
?table

#4
#Posteriormente elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
#¿Cuántos goles tuvo el partido con mayor empate?
#R: 4 goles por cada equipo
table(df2)
table(df2)[5,5]
#¿En cuántos partidos ambos equipos empataron 0 a 0?
#R: 33 partidos
table(df2)
table(df2)[1,1]
#¿En cuántos partidos el equipo local (HG) tuvo la mayor goleada sin dejar que el equipo visitante (AG) metiera un solo gol?
#R: En un solo partido anotando 6 goles por 0 del equipo visitante
table(df2)
table(df2)[7,1]
#La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)
total <- sum(table(df2))
round(marginSums(table(df2),1)/total,3)
#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
round(marginSums(table(df2),2)/total,3)
#La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como
#visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)
round(table(df2)/total,3)

#Notas para los datos de soccer: https://www.football-data.co.uk/notes.txt