# Actividad 4 IME
# Grupo 6: Angel Avenda�o, Jorge Gonz�lez y Juan V�squez
# Importar paquetes.
if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
  require(dplyr)
}
if(!require(tidyr)){
  install.packages("tidyr",dependencies = TRUE)
  require(tidyr)
}
if(!require(ggpubr)){
  install.packages("ggpubr",dependencies = TRUE)
  require(ggpubr)
}
# Cargar datos.
# Se carga el archivo de datos CSV.
datos <- read.csv2(file.choose(new = FALSE), encoding = "UTF-8")
datos2 <- datos
datos3 <- datos
datos4 <- datos

### ---------- Pregunta 1 ----------
# 1. El Comit� Ol�mpico cree que el mejor tiempo medio de los atletas orientales antes de ingresar al programa de
# entrenamiento es de 19,75 segundos. �Soportan los datos esta afirmaci�n?

# H0: el tiempo promedio de los atletas orientales es igual a 19,75 segundos antes de ingresar al programa de
#     entrenamiento.
# HA: el tiempo promedio de los atletas orientales es diferente a 19,75 segundos antes de ingresar al programa de
#     entrenamiento.

# Denotando como � al  tiempo medio de los atletas orientales antes de ingresar al programa de entrenamiento:
# H0: � = �0, esto es � = 19,75 segundos
# HA: � != �0 , esto es � != 19,75 segundos


# Verificar distribuci�n muestral usando la prueba de normalidad de Shapiro - Wilk para comprobar que se cumplan las
# condiciones que nos indican que es correcto realizar el c�lculo del estad�stico de prueba y p-valor
datos <- datos %>% filter(Raza == "Oriental")


# Establecer los datos conocidos
n <- length (datos[["Previo"]])
grados_libertad <- n - 1
valor_nulo <- 19.75


# Verificar si la distribuci�n se acerca a la normal
g <- ggqqplot (datos,
               x = "Previo",
               color = "steelblue",
               xlab = "Te�rico",
               ylab = "Muestra",
               title = "Gr�fico Q-Q muestra v/s distr.normal")

print (g)

# Fijar un nivel de significaci�n.
alfa <- 0.025

# Luego aplicamos t
# Calcular el estad�stico de prueba.
cat ("\t Prueba t para una muestra \n\n")
media <- mean (datos[["Previo"]])
cat (" Media =", media , "M$\n")
desv_est <- sd(datos[["Previo"]])
error <- desv_est / sqrt (n)
t <- ( media - valor_nulo ) / error
cat

# ------------------------------- EL VALOR DEBE SER 2*P POR LAS DOS COLAS
# Calcular el valor p.
p <- 2*pt(t, df = grados_libertad , lower.tail = TRUE)
cat ("p =", p , "\n")

# Construir el intervalo de confianza .
t_critico <- qt(alfa , df = grados_libertad , lower.tail = FALSE)
superior <- media + t_critico * error
cat (" Intervalo de confianza = (-Inf , ", superior ,"]\n", sep = "")

# Aplicar la prueba t de Student con la funci�n de R una vez que comprobamos
# todas las condiciones para poder obtener el p-valor. -----------
prueba <- t.test((datos[["Previo"]]) ,
                 alternative = "two.sided",
                 mu = valor_nulo,
                 conf.level = 1 - alfa)

print (prueba)

# Respuesta : El mejor tiempo medio de los atletas orientales antes de ingresar al programa de
# entrenamiento es de 19.4941 segundos, por lo tanto se falla al intentar rechazar la hip�tesis nula. Sin embargo, la media resultante
# es muy cercana a la media de H0, donde le intervalo de confianza es ]-inf, 20.02571], por lo que se sugiere aumentar el tama�o de la muestra, para lograr asi tener una 
# respuesta mas precisa.

# Puesto que p > alfa, se falla en rechazar H0. Es decir, no hay evidencia
# suficiente para concluir que existe una diferencia entre los tiempos promedios.




### ---------- Pregunta 2 ----------
# 2. �Sugieren los datos que la mejor marca de los atletas negros se reduce en menos de 4,17 
# segundos tras el entrenamiento?

# H0 : el tiempo promedio de los atletas negros se reduece en 4,17 segundos tras el entrenamiento.
# HA : el tiempo promedio de los atletas negros se reduece en mas de 4,17 segundos tras el entrenamiento.

# Denotando como � al  tiempo medio de la diferencia entre los tiempos antes del entrenamiento y posterior:
# H0 : � = 4.17 segundos
# HA : � > 4.17 segundos

# Filtrar y obtener la diferencia entre los tiempos de antes del entrenamiento y posterior.
datos2 <- datos2 %>% filter(Raza == "Negra")

diferencia <- as.data.frame(datos2[["Previo"]] - datos2[["Posterior"]])
names(diferencia)[1] <- "Diferencia"

#valor nulo
valor_nulo2 <- 4.17


# Verificar distribuci�n muestral usando la prueba de normalidad de Shapiro - Wilk nuevamente para comprobar que la
# muestra obtenida se distribuye de manera aproximadamente normal permnitiendonos prosegir con el estudio.
normalidad2 <- shapiro.test(diferencia[["Diferencia"]])
print (normalidad2)


# Fijar un nivel de significaci�n.
alfa <- 0.025

# Aplicar la prueba t de Student con la funci�n de R para obtener el p-valor.
prueba <- t.test (diferencia,
                  alternative = "greater",
                  mu = valor_nulo2 ,
                  conf.level = 1 - alfa)

print(prueba)

# Respuesta: Se puede afirmar con un 97,5% de confianza que la diferencia de tiempo de los atletas negros antes y despu�s 
# del entrenamiento es menor a 4,17 segundos, sin embargo, se neceitar�a una muestra de mayor tama�o para mejorar
# la precisi�n.
# En otras palabras se rechaza la hip�tesis nula a favor de la hip�tesis alternativa, sin embargo, se sugiere aumentar
# el tama�o de la muestra por las razones anteriormente mencionadas.


### ---------- Pregunta 3 ----------
# 3. �Es posible afirmar que, en promedio, los atletas negros superan a los orientales por m�s de 1,76 segundos
# despu�s del entrenamiento?

# H0: la diferencia de los promedios entre los atletas negros y orientales es igual a 1,76 segundos.
# HA: la diferencia de los promedios entre los atletas negros y orientales es mayor a 1,76 segundos.

# Denotando como � al  tiempo medio de los atletas negros y orientales despues de ingresar al programa de entrenamiento:
# H0: �0 - �N = 1.76 segundos 
# HA: �0 - �N > 1.76 segundos

datos4 <- datos4 %>% filter(Raza == "Oriental")
datos3 <- datos3 %>% filter(Raza == "Negra")

#Establecer los datos conocidos
# Para Raza Oriental
n2 <- length (datos4[["Posterior"]])
grados_libertad2 <- n2 - 1

# Para Raza negra
n3 <- length (datos3[["Posterior"]])
grados_libertad3 <- n3 - 1

#hipotesis nula
valor_nulo_3 <- 1.76

# Verificar distribuci�n muestral usando la prueba de normalidad de Shapiro - Wilk.
normalidad4 <- shapiro.test(datos4[["Posterior"]])
print (normalidad4)

normalidad3 <- shapiro.test(datos3[["Posterior"]])
print (normalidad3)


# Fijar un nivel de significaci�n.
alfa <- 0.025


# Aplicar la prueba t de Student con la funci�n de R.
prueba <- t.test (x = (datos4[["Posterior"]]) ,
                  y = (datos3[["Posterior"]]) ,
                  paired = FALSE ,
                  alternative = "greater",
                  mu = valor_nulo_3 ,
                  conf.level = 1 - alfa)

print (prueba)

# p es menor que el nivel de significaci�n se considera evidencia suficiente para rechazar la hip�tesis
# nula, por lo que se acepta la hip�tesis alterativa.

# Respuesta: Se puede afirmar con un 97,5% de confianza que los atletas negros superan a los orientales
# por m�s de 1,76 segundos luego de realizar el entrenamiento, como se mencion� anteriormente, ser�a necesario
# aumentar el tama�o de la muestra para hacer un estudio m�s preciso.



