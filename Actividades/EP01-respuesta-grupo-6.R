# Actividad 1 IME
# Grupo 6: Angel Avendaño, Jorge González y Juan Vásquez

# Se cargan los paquetes necesario, en caso de no estar instalados se instalan.
if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
  require(dplyr)
}

# Se carga el archivo de datos CSV
datos <- read.csv2(file.choose(new = FALSE))
# -----------------------------------------
# Las variables cargadas son regiones y número de contagiados por fecha.

# Las regiones son del tipo categórica nominal y de escala nominal,
# ya que no existe una relación de orden mas que el nombre.

# Las fechas son del tipo numérica discreta y de escala de razón,
# debido a que existe el cero verdadero y además puede tomar sólo valores
# enteros positivos.

# ------------- Pregunta 6.1 -------------
# ¿Qué día se produjo el mayor número de casos con síntomas en la región de Ñuble entre el 01-nov-2020 y el 
# 31-may-2021?
# Se filtra el dataframe por la región del Ñuble
datos1 <- datos %>% filter(Region == "Ñuble")

# Se obtiene el indice de la fecha 01/11/2020
index1 <- which(names(datos1)=="X01.11.2020")

# Se obtiene el indice de la fecha 31/05/2021
index2 <- which(names(datos1)=="X31.05.2021")

# Se seleccionan los datos dentro del rango de fechas y almacenan en la variable datosInteres
datosInteres <- datos1[index1:index2]

# Se obtiene el día con mayor número de casos en Ñuble y se muestra por pantalla
resultado <- names(datosInteres[which.max(datosInteres)])
strResultado <- paste("El día en que se produjo el mayor número de casos con síntomas en la región de Ñuble entre el 01-nov-2020 y el 31-may-2021 fue el día:",resultado)
print(strResultado)

### El día con el mayor número de casos con síntomas en la región del Ñuble
### entre el 1 de noviembre del 2020 y 31 de mayo del 2021, fue el 31 de marzo del 2021.

# ------------- Pregunta 6.2 -------------
# Se calcula la suma mensual de cada mes entre el 1 de noviembre del 2020 y 31 de mayo del 2021 
nov <- rowSums(datosInteres[, which(colnames(datosInteres) == "X01.11.2020"):which(colnames(datosInteres) == "X30.11.2020")])
dic <- rowSums(datosInteres[, which(colnames(datosInteres) == "X01.12.2020"):which(colnames(datosInteres) == "X31.12.2020")])
ene <- rowSums(datosInteres[, which(colnames(datosInteres) == "X01.01.2021"):which(colnames(datosInteres) == "X31.01.2021")])
feb <- rowSums(datosInteres[, which(colnames(datosInteres) == "X01.02.2021"):which(colnames(datosInteres) == "X28.02.2021")])
mar <- rowSums(datosInteres[, which(colnames(datosInteres) == "X01.03.2021"):which(colnames(datosInteres) == "X31.03.2021")])
abr <- rowSums(datosInteres[, which(colnames(datosInteres) == "X01.04.2021"):which(colnames(datosInteres) == "X30.04.2021")])
may <- rowSums(datosInteres[, which(colnames(datosInteres) == "X01.05.2021"):which(colnames(datosInteres) == "X31.05.2021")])

# Se crea un vector con la suma de los totales mensuales
total <- c(nov, dic, ene, feb, mar, abr, may)

# Se crea el dataframe con el total por mes y se renombran las columnas con el mes y año correspondiente
sumaTotalMeses <- data.frame(total, row.names = c("Noviembre 2020", "Diciembre 2020",
                                                  "Enero 2021", "Febrero 2021",
                                                  "Marzo 2021", "Abril 2021",
                                                  "Mayo 2021"))
# Se muestra el resultado por pantalla
print(sumaTotalMeses)

