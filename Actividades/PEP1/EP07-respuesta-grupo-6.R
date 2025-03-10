# Actividad 7 IME
# Grupo 6: Angel Avenda�o, Jorge Gonz�lez y Juan V�squez

# Importar paquetes.

if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE)
  require(tidyverse)
}

if(!require(RVAideMemoire)){
  install.packages("RVAideMemoire",dependencies = TRUE)
  require(RVAideMemoire)
}

if(!require(rcompanion)){
  install.packages("rcompanion",dependencies = TRUE)
  require(rcompanion)
}

# -------------------- PREGUNTA 1 ----------------------------------

# 1. Estudios cient�ficos han descubierto que la inteligencia musical est� altamente relacionada con la inteligencia
# matem�tica. Pensando en mejorar la oferta de actividades culturales y recreativas, una Universidad ha
# examinado la preferencia de talleres de un grupo de 8 estudiantes de carreras cient�ficas y 11 de carreras
# humanistas, encontrando que 6 de los primeros y 2 de los segundos participaron de talleres musicales. �Existe
# relaci�n entre el tipo de carrera que cursan los estudiantes y su participaci�n en talleres musicales?

# H0 : Hay independencia tipo de carrera que cursan los estudiantes y su participaci�n en talleres musicales.
# H1 : Hay una relaci�n entre el tipo de carrera que cursan los estudiantes y su participaci�n en talleres musicales.

# Construir la tabla de contingencia.
carrera <- c(rep ("Cientifica", 8), rep("Humanista", 11))
talleres <- c(rep("Participa", 8), rep("No Participa", 11))
datos <- data.frame(talleres, carrera)
tabla <- xtabs(~., datos)
print (tabla)

# Aplicar prueba exacta de Fisher.
alfa <- 0.05
prueba <- fisher.test(tabla, 1 - alfa)
print(prueba)

# Respuesta: Aplicando la prueba de Fisher para muestras peque�as se obtiene un p-valor < alfa (1.323x10^-5 < 0.05),
# es decir, con un 95% de confianza se rechaza la hip�tesis nula obteniendo que existe independencia entre las variables
# estudiadas.

# -------------------- PREGUNTA 2 ----------------------------------

# 2. Un pol�mico estudio realizado con 12 pares de gemelos abandonados por sus padres, donde uno de los
# gemelos fue adoptado por una familia funcional y el otro creci� en un centro del estado, ha encontrado que,
# al llegar a la edad adulta:
# - Solo en una pareja de gemelos, ambos presentaron trastornos psicol�gicos.
# - En dos casos, solo el gemelo adoptado present� trastornos psicol�gicos.
# - En 5 de los casos, solo el gemelo que creci� en el centro estatal present� trastornos psicol�gicos.
# - En los casos restantes, ambos gemelos est�n libres de problemas psicol�gicos.
# �Influye el entorno en que un ni�o crece en la aparici�n de trastornos psicol�gicos en la edad adulta?

# H0 : El entorno en el que el ni�o crece no influye en los trastornos psicologicos que desarrolla en la adultez
# H1 : El entorno en el que el ni�o crece influye en los trastornos psicologicos que desarrolla en la adultez

# Se define la tabla de datos.
entorno <- c(rep("Estado", 12), rep("Familia", 12))
trastorno <- c(rep("Presenta", 9), rep("No Presenta", 15))

# Se pasa a data frame.
datos1 <- data.frame(entorno, trastorno)
tabla1 <- xtabs(~., datos1)
print(tabla1)

# Se realia la prueba de Fisher con un alfa de 0.05.
alfa <- 0.05
prueba1 <- fisher.test(tabla1, 1- alfa)
print(prueba1)

# Respuesta: Con un p-valor de 0.0003365 y un alfa de 0.05 se rechaza la hip�tesis nula con un 95% de confianza,
# es decir, el entorno en el que el ni�o crece si influye en los trastornos psicol�gicos que desarrolla en la etapa de
# la adultez.

# -------------------- PREGUNTA 3 ----------------------------------

# 3. El 21 de marzo de 2022 se realiz� un estudio acerca de la aprobaci�n al presidente Gabriel Boric entre los
# estudiantes de una prestigiosa universidad a fin de comparar los resultados con los obtenidos en la misma
# encuesta a nivel nacional, obteni�ndose los resultados que se muestran en la tabla. �Refleja la opini�n
# estudiantil la percepci�n del pa�s?

# H0 : La aprobaci�n de los estudiantes refleja la percepci�n del pa�s sobre el presidente.
# H1 : La aprobaci�n de los estudiantes no refleja la percepci�n del pa�s sobre el presidente.

# Se definen los vectores para crear la tabla.
pais <- c(5046, 3421, 706)
estudiantes <- c(125, 77, 21)

# Se crea la tabla.
tabla2 <- as.table(rbind(pais, estudiantes))

# Se renombran las filas y las columnas.
dimnames(tabla2) <- list(grupo = c("Nacional", "Estudiantes"),
                        aprobacion = c("Aprueba", "Desaprueba", "Ninguna"))
print(tabla2)

# Se verifica si se esperan mas de 5 observaciones por grupo (Aprueba, Desaprueba, Ninguna)
n_pais <- sum(pais)
n_estudiantes <- sum(estudiantes)
proporciones <- round(pais/n_pais, 3)
esperados <- round(proporciones * n_estudiantes, 3)
print(esperados)

# Se realiza la prueba de chi-cuadrado
prueba2 <- chisq.test(tabla2, correct = FALSE)
print(prueba2)

# Respuesta: Con la prueba chi-cuadrado de bondad de ajuste como evidencia se falla al rechazar la hip�tesis nula
# con un p-valor = 0.516, en otras palabras, se puede afirmar con un 95% de confianza (alfa = 0.05) que efectivamente
# la muestra de los estudiantes elegida representa la opini�n a nivel nacional del presidente Gabriel Boric.

# -------------------- PREGUNTA 4 ----------------------------------

# La Facultad de Ingenier�a desea saber si existe diferencia significativa en el desempe�o de los estudiantes en
# asignaturas cr�ticas de primer semestre. Para ello, le ha entregado un archivo de datos que, para 3 asignaturas,
# indica si una muestra de 50 estudiantes aprob� o reprob�. �Qu� puede concluir la Facultad? Indicaci�n:
# obtenga la muestra a partir del archivo EP07 Datos.csv, usando la semilla 453. Considere un nivel de
# significaci�n ??=0,05.

# H0: La proporci�n de desempe�o entre las 3 asignaturas cr�ticas es la misma para los estudiantes.
# H1: La proporci�n de desempe�o entre las 3 asignaturas cr�ticas es distintas para los estudiantes.

# Se lee el archivo de los datos.
archivo <- read.csv2(file.choose(new = FALSE))

# Se fija la semilla.
set.seed(453)

# Se extrae una muestra del total de datos.
muestra <- sample_n(archivo, 50)

# Se genera un vector para indexar la muestra.
instancia <- 1:50

# Se reemplazan los resultados por valores num�ricos.
muestra[muestra == "A"] <- 1
muestra[muestra == "R"] <- 0

# Se agrega la indexaci�n a la muestra.
datos2 <- cbind(instancia, muestra)

# Se lleva la muestra a datos en formato largo.
datos2 <- datos2 %>% pivot_longer(c("Calculo", "Algebra", "Fisica"),
                                  names_to = "asignaturas",
                                  values_to = "resultado")

datos2[["instancia"]] <- factor(datos2[["instancia"]])
datos2[["asignaturas"]] <- factor(datos2[["asignaturas"]])

# Se aplica la prueba Q de Cochran.
prueba3 <- cochran.qtest(resultado ~ asignaturas | instancia,
                         data = datos2, alpha = 0.05)

print(prueba3)

# Procedimiento post-hoc con correcci�n de Bonferroni.
post_hoc_1 <- pairwiseMcnemar(resultado ~ asignaturas | instancia,
                              data = datos2, method = "bonferroni")
cat("\nBONFERRONI\n")
print(post_hoc_1)

# Procedimiento post-hoc con correcci�n de Holm.
post_hoc_2 <- pairwiseMcnemar(resultado ~ asignaturas | instancia,
                              data = datos2, method = "holm")
cat("\nHOLM\n")
print(post_hoc_2)

# Respuesta: Una vez realizada la prueba Q de Cochrane se pudo rechazar la hip�tesis nula con un 95% de confianza,
# sin embargo, hay que realizar las pruebas post-hoc para asegurarnos cual de las asignaturas es la que tiene
# diferente proporci�n de desempe�o en caso de ser cierto. Una vez aplicadas dichas pruebas se puede obtener que
# el bloque en el que se pueden ver diferencias notorias (con un p-valor = 0.019) es en el bloque Algebra - Fisica.
# En conclusi�n la mayor diferencia de desempe�o de los alumnos las tienen en las asignaturas de Algebra y F�sica.
