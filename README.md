# Proyecto final del Equipo 8 del curso de Ciencia de Datos Módulo Estadística con R
En este repositorio se encuentran los postworks del módulo.
El Postwork número 8 contiene el proyecto final que se describe a continuación

# Estudio de inseguridad alimenticia en México
Utilizando datos de la Encuesta Nacional de Salud y Nutrición (2012) levantada por el Instituto Nacional de Salud Pública en México. 

## Desarrollo

Un centro de salud nutricional está interesado en analizar estadística y probabilísticamente los patrones de gasto en alimentos saludables y no saludables en los hogares mexicanos con base en su nivel socioeconómico, en si el hogar tiene recursos financieros extras al ingreso y en si presenta o no inseguridad alimentaria. Además, está interesado en un modelo que le permita identificar los determinantes socioeconómicos de la inseguridad alimentaria.

La base de datos es un extracto de la Encuesta Nacional de Salud y Nutrición (2012) levantada por el Instituto Nacional de Salud Pública en México. La mayoría de las personas afirman que los hogares con menor nivel socioeconómico tienden a gastar más en productos no saludables que las personas con mayores niveles socioeconómicos y que esto, entre otros determinantes, lleva a que un hogar presente cierta inseguridad alimentaria.

La base de datos contiene las siguientes variables:

- nse5f (Nivel socioeconómico del hogar): 1 "Bajo", 2 "Medio bajo", 3 "Medio", 4 "Medio alto", 5 "Alto"
- area (Zona geográfica): 0 "Zona urbana", 1 "Zona rural"
- numpeho (Número de personas en el hogar)
- refin (Recursos financieros distintos al ingreso laboral): 0 "no", 1 "sí"
- edadjef (Edad del jefe/a de familia)
- sexoje (Sexo del jefe/a de familia): 0 "Hombre", 1 "Mujer"
- añosedu (Años de educación del jefe de familia)
- ln_als (Logaritmo natural del gasto en alimentos saludables)
- ln_alns (Logaritmo natural del gasto en alimentos no saludables)
- IA (Inseguridad alimentaria en el hogar): 0 "No presenta IA", 1 "Presenta IA"

## Estudio
En este estudio identificaremos los patrones de gasto de alimentos saludables y no saludables con base en el nivel económico de los hogares mexicanos. 

Para este estudio algunas de las preguntas que resolvimos, entre otras, son las siguientes:

- ¿Cuánto gastan los hogares en productos saludables? **Respuesta:** Los hogares gastan cómo mínimo 3 unidades monetarias hasta un máximo de 5460 unidades monetarias, teniendo un promedio de 594 unidades monetarias, sin embargo, menos del 75% de las observaciones gastan menos de 760 unidades monetarias.
- ¿Cuánto gastan los hogares en productos no saludables? **Respuesta:** El gasto más alto esperado con el 0.7 de probabilidad es de 96.09 unidades monetarias. Los valores del total del gasto en alimentos no saludables que dejan exactamente al centro el 80% van desde 15.36 unidades monetarias hasta 207.25 unidades monetarias.
- ¿Existe una correlación entre el gasto de productos no saludables y el nivel socioeconómico? **Respuesta:** Encontramos que sí existe una correlación entre el gasto de productos no saludables y el nivel socioeconómico, sin embargo, no es la única variable que tiene correlación con el gasto de productos no saludables.   
- ¿Cuántos hogares tienen ingresos extra? **Respuesta:** De la muestra de 20,280 registros, 16,421 registros no cuentan con recursos financieros distintos a su ingreso laboral, mientras que solo 3,859 registros cuentan con recursos extras al ingreso laboral. Además encontramos que en promedio los hogares que no cuentan con un ingreso extra gastan más en alimentos no saludables.
- ¿Cuántos hogares tienen inseguridad alimentaria? **Respuesta:** De la muestra de 20,280, 14,427 observaciones padecen inseguridad alimentaria en el hogar y sólo 5,853 registros no pertenecen al grupo de inseguridad alimentaria en el hogar. Y encontramos que en promedio los hogares que no tienen inseguridad alimentaria gastan más en alimentos no saludables que los hogares que tienen inseguridad alimentaria. 
- ¿Existe correlación múltiple entre las variables de nivel socioeconómico, ingreso extra y consumo de productos no saludables que produzcan inseguridad alimentaria? **Respuesta:** Al realizar el modelo observamos que si existe correlación de estas variables para pronosticar que producen inseguridad alimentaria, hay otras variables que participan para generar un mejor modelo.

En la exploración de datos nos hemos planteado varias hipótesis siendo la siguiente la principal hipótesis que deseamos probar.

> Hipótesis:
>
> H0: Gasto en productos no saludables en hogares de menor nivel socioeconómico <= Gasto en productos no saludables en hogares de mayor nivel socioeconómico.
>
> HA: Gasto en productos no saludables en hogares de menor nivel socioeconómico > Gasto en productos no saludables en hogares de mayor nivel socioeconómico.

Probando hipótesis entre los niveles socioeconómicos contiguos, encontramos que para un nivel de confianza de 95 % el promedio de gasto de alimentos no saludables es mayor para el nivel socioeconómico más alto de los dos evaluados.
    
- Para nivel Alto es mayor que Medio Alto. 
- Para nivel Medio Alto es mayor que Medio.
- Para nivel Medio es mayor que Medio Bajo.
- Para nivel Medio Bajo es mayor que Bajo.

## Modelo
Analizamos tres modelos diferentes para encontrar la mejor opción que nos permita identificar cuáles son los determinantes socioeconómicos que producen inseguridad alimenticia. 

Tratamos de predecir la variable Inseguridad Alimentaria (IA), como la variable dependiente, y tomar las variables que tuvieron un valor > +-0.10 en la matriz de correlación para usarlas como variables independientes. Analizamos los modelos de regresión logística y dado los valores del criterio de información Akaike, el pseudo r2 y el número de variables elegimos un modelo con 6 variables independientes para realizar predicciones.

```
log_m <- glm(IA ~ factor(nse5f) + factor(añosedu) + ln_als +
             ln_alns + factor(area) + factor(numpeho), 
                      data = df1, 
                      family = "binomial")
```

