# Actividad 1 IME
# Grupo 6: Angel Avenda�o, Jorge Gonz�lez y Juan V�squez

# Se cargan los paquetes necesario, en caso de no estar instalados se instalan.
if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
  require(dplyr)
}

# Se carga el archivo de datos CSV
datos <- read.csv2(file.choose(new = FALSE))
# -----------------------------------------
# Las variables cargadas son regiones y n�mero de contagiados por fecha.

# Las regiones son del tipo categ�rica nominal y de escala nominal,
# ya que no existe una relaci�n de orden mas que el nombre.

# Las fechas son del tipo num�rica discreta y de escala de raz�n,
# debido a que existe el cero verdadero y adem�s puede tomar s�lo valores
# enteros positivos.

# ------------- Pregunta 6.1 -------------
# �Qu� d�a se produjo el mayor n�mero de casos con s�ntomas en la regi�n de �uble entre el 01-nov-2020 y el 
# 31-may-2021?
# Se filtra el dataframe por la regi�n del �uble
datos1 <- datos %>% filter(Region == "�uble")

# Se obtiene el indice de la fecha 01/11/2020
index1 <- which(names(datos1)=="X01.11.2020")

# Se obtiene el indice de la fecha 31/05/2021
index2 <- which(names(datos1)=="X31.05.2021")

# Se seleccionan los datos dentro del rango de fechas y almacenan en la variable datosInteres
datosInteres <- datos1[index1:index2]

# Se obtiene el d�a con mayor n�mero de casos en �uble y se muestra por pantalla
resultado <- names(datosInteres[which.max(datosInteres)])
strResultado <- paste("El d�a en que se produjo el mayor n�mero de casos con s�ntomas en la regi�n de �uble entre el 01-nov-2020 y el 31-may-2021 fue el d�a:",resultado)
print(strResultado)

### El d�a con el mayor n�mero de casos con s�ntomas en la regi�n del �uble
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

# Se crea el dataframe con el total por mes y se renombran las columnas con el mes y a�o correspondiente
sumaTotalMeses <- data.frame(total, row.names = c("Noviembre 2020", "Diciembre 2020",
                                                  "Enero 2021", "Febrero 2021",
                                                  "Marzo 2021", "Abril 2021",
                                                  "Mayo 2021"))
# Se muestra el resultado por pantalla
print(sumaTotalMeses)

