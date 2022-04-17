# Actividad 6 IME
# Grupo 6: Angel Avendaño, Jorge González y Juan Vásquez

# Importar paquetes.
if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
  require(dplyr)
}
if(!require(Hmisc)){
  install.packages("Hmisc",dependencies = TRUE)
  require(Hmisc)
}

# Almacenar observaciones que en su conjunto pertenecen a la muestra con distintas especialidades y sexo
Mujeres <- c(54, 71, 35, 30, 45, 44, 56, 21, 17)
Hombres <- c(52, 66, 41, 42, 65, 62, 88, 40, 35)
Especialidad <- c("Pediatría", "Obstetricia","Dermatología",
                  "Psiquiatría", "Medicina Interna", "Oncología",
                  "Neurología", "Anestesiología", "Radiología")


# ---------------- PREGUNTA 1 ----------------------------------

# 1. Estudios previos habían determinado que la proporción de autoras en la especialidad de oncología era de
# 32%. ¿Respaldan estos datos tal estimación?


# H0 : La proporción de autoras en la especialidad de oncología es de un 32%.
# H1 : La proporción de autoras en la especialidad de oncología no es de un 32%.

# Denotando como p como la proporción de autoras en las especialidad de oncología:

# H0 : p = p0, esto es p =  30 % de autoras en la especialidad de oncología.
# H1 : p != p0, esto es p != 30 % de autoras en la especialidad de oncología.

# se procede a generar el data frame
datos <- data.frame(rbind(Mujeres, Hombres))
# asignación de nombres por columnas
colnames(datos) <- Especialidad

# cantidad de mujeres en oncología.
datos1 <- c(datos$Oncología)[1]
filtro1 <- data.frame(datos1)

# se asignan los totales de cada fila y columnas, tanto mujeres como hombres.
datos$total <- rowSums(datos)
totalC <- colSums(datos)
datos <- rbind(datos,totalC)
print(datos)

# se asigna el nivel de significación
alfa <- 0.05

# método de Wilson para la diferencia entre dos proporciones.
prop.test(datos$Oncología[1]/datos$Oncología[3], n = datos$Oncología[3], p = 0.32, alternative = "two.sided",  conf.level = 1 - alfa)

# Respuesta : Dado que el valor p resultante es p = 6.322e-12, por lo que se logra rechazar la hipótesis
# nula con un nivel de significación ?? = 0,05 en favor de la hipótesis alternativa. En consecuencia, podemos
# concluir con 95 % de confianza que la proporción de autoras en la especialidad de oncología no es de un 32%.


# ---------------- PREGUNTA 2 ----------------------------------

# 2. Según estos datos, ¿es igual la proporción de autoras en las áreas de oncología y dermatología?

# H0 : La proporción de autoras en las áreas es igual a la proporción de oncología y dermatología
# H1 : La proporción de autoras en las áreas es distinta a la proporción de oncología y dermatología

# Denotando como po como la proporción de autoras en las especialidad de oncología y, pd la proporción
# de autoras en la especialidad de dermatología:

# H0 : po - pd = 0 , esto es po = pd en cuanto a la proporción de autoras en las áreas de oncología y dermatología.
# H1 : po - pd != 0, esto es po != pd en cuanto a la proporción de autoras en las áreas de oncología y dermatología.


# Fijar valores conocidos (hombres,mujeres)
n <-c(datos$Oncología[3] , datos$Dermatología[3])
exitos <- c(datos$Oncología[1] , datos$Dermatología[1])
alfa <- 0.05

# método de Wilson para la diferencia entre dos proporciones.
prueba <- prop.test(exitos, n = n , alternative = "two.sided", conf.level = 1 - alfa)
print (prueba)

# Respuesta : Dado que el valor p resultante es p = 0.6468, se se falla al rechazar la hipótesis
# nula con un nivel de significación ?? = 0,05. En consecuencia, podemos concluir con 95 % de confianza que
# la proporción de autoras en la especialidad de oncología y dermatología es, en efecto el mismo.

# ---------------------------- PREGUNTA 3 --------------------------------------------------------

# 3. Suponiendo que la diferencia en la proporción de autoras en la especialidad de psiquiatría y la de obstetricia
# es de 0,18. ¿A cuántos autores deberíamos monitorear para obtener un intervalo de confianza del 99% y poder
# estadístico de 90%, si se intenta mantener aproximadamente la misma proporción de gente estudiada en cada
# caso?


# Calcular el tamaño de las áreas
n_psiq <- datos$Psiquiatría[3] 
n_obst <- datos$Obstetricia[3]

# Calcular la proporción de las áreas
p_psiq <- datos$Psiquiatría[1]/n_psiq
p_obst <- datos$Obstetricia[1]/n_obst

# Las proporciones son 0.41 para Psiquiatría y 0.52 para Obstetricia
# Por lo cual se debe estimar una proporción similar que cumpla la diferencia de estas en un 0.18
p2_psiq <- 0.38
p2_obst <- 0.56

# se define el nivel de significación
alpha <- 0.01
# se define el poder dado
poder <- 0.9

fraccion <- n_psiq / (n_psiq + n_obst)

# Como el valor n de ambas especialidades son distintos (por las proporciones y totales)
# mediante bsamsize se calcula el valor para cada especialidad.
prueba3 <- bsamsize(p2_psiq, p2_obst, fraction = fraccion, alpha = alpha, power = poder)
print (prueba3)

# Resultados: El número de autores que se deberían monitorear para obtener un intervalo de confianza del 99% y poder
# estadístico de 90%, donde se intenta mantener aproximadamente la misma proporción de gente estudiada en cada
# caso es N_Psiquiatría: 173 (para el area de Psiquiatría) y, N_Obstetricia: 329 (para el area de Obstetricia),
# ambos valores aproximandamente.

