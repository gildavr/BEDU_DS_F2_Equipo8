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

Para este estudio se resuelven las siguentes preguntas:

- ¿Cuánto gastan los hogares en productos saludables?
- ¿Cuánto gastan los hogares en productos no saludables?
- ¿Existe una correlación entre el consumo de productos no saludables y el nivel socio-económico?
- ¿Cuántos hogares tienen ingresos extra?
- ¿Cuántos hogares tienen inseguridad alimenticia?
- ¿Existe correlación multiple entre las variables de nivel socio-económico, ingreso extra y consumo de productos no saludables que produzcan inseguridad alimenticia?

En la exploración de datos nos hemos planteado varias hipótesis siendo la siguiente la principal hiótesis que deseamos probar.

> Hipótesis:
>
> H0: Gasto en productos no saludables en hogares de menor nivel socioeconómico <= Gasto en productos no saludables en hogares de mayor nivel socioeconómico.
>
> HA: Gasto en productos no saludables en hogares de menor nivel socioeconómico > Gasto en productos no saludables en hogares de mayor nivel socioeconómico.

## Modelo
Analizamos tres modelos diferentes para encontrar la mejor opción que nos permita identificar cuáles son los determinantes socio-económicos que producen inseguridad alimenticia. 

Tratamos de predecir la variable Inseguridad Alimenticia (IA), como la variable dependiente, y tomar las variables que tuvieron un valor > +-0.10 en la matriz de correlación para usarlas como variables independientes. Analizamos los modelos de regresión lógistica y dado los valores del criterio de información Akaike, el pseudo r2 y el número de variables el modelo que elegimos es el modelo 1, con 6 variables independientes, para realizar predicciones.

```
log_m <- glm(IA ~ factor(nse5f) + factor(añosedu) + ln_als +
             ln_alns + factor(area) + factor(numpeho), 
                      data = df1, 
                      family = "binomial")
```

