# Actividad 6 IME
# Grupo 6: Angel Avenda�o, Jorge Gonz�lez y Juan V�squez

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
Especialidad <- c("Pediatr�a", "Obstetricia","Dermatolog�a",
                  "Psiquiatr�a", "Medicina Interna", "Oncolog�a",
                  "Neurolog�a", "Anestesiolog�a", "Radiolog�a")


# ---------------- PREGUNTA 1 ----------------------------------

# 1. Estudios previos hab�an determinado que la proporci�n de autoras en la especialidad de oncolog�a era de
# 32%. �Respaldan estos datos tal estimaci�n?


# H0 : La proporci�n de autoras en la especialidad de oncolog�a es de un 32%.
# H1 : La proporci�n de autoras en la especialidad de oncolog�a no es de un 32%.

# Denotando como p como la proporci�n de autoras en las especialidad de oncolog�a:

# H0 : p = p0, esto es p =  30 % de autoras en la especialidad de oncolog�a.
# H1 : p != p0, esto es p != 30 % de autoras en la especialidad de oncolog�a.

# se procede a generar el data frame
datos <- data.frame(rbind(Mujeres, Hombres))
# asignaci�n de nombres por columnas
colnames(datos) <- Especialidad

# cantidad de mujeres en oncolog�a.
datos1 <- c(datos$Oncolog�a)[1]
filtro1 <- data.frame(datos1)

# se asignan los totales de cada fila y columnas, tanto mujeres como hombres.
datos$total <- rowSums(datos)
totalC <- colSums(datos)
datos <- rbind(datos,totalC)
print(datos)

# se asigna el nivel de significaci�n
alfa <- 0.05

# m�todo de Wilson para la diferencia entre dos proporciones.
prop.test(datos$Oncolog�a[1]/datos$Oncolog�a[3], n = datos$Oncolog�a[3], p = 0.32, alternative = "two.sided",  conf.level = 1 - alfa)

# Respuesta : Dado que el valor p resultante es p = 6.322e-12, por lo que se logra rechazar la hip�tesis
# nula con un nivel de significaci�n ?? = 0,05 en favor de la hip�tesis alternativa. En consecuencia, podemos
# concluir con 95 % de confianza que la proporci�n de autoras en la especialidad de oncolog�a no es de un 32%.


# ---------------- PREGUNTA 2 ----------------------------------

# 2. Seg�n estos datos, �es igual la proporci�n de autoras en las �reas de oncolog�a y dermatolog�a?

# H0 : La proporci�n de autoras en las �reas es igual a la proporci�n de oncolog�a y dermatolog�a
# H1 : La proporci�n de autoras en las �reas es distinta a la proporci�n de oncolog�a y dermatolog�a

# Denotando como po como la proporci�n de autoras en las especialidad de oncolog�a y, pd la proporci�n
# de autoras en la especialidad de dermatolog�a:

# H0 : po - pd = 0 , esto es po = pd en cuanto a la proporci�n de autoras en las �reas de oncolog�a y dermatolog�a.
# H1 : po - pd != 0, esto es po != pd en cuanto a la proporci�n de autoras en las �reas de oncolog�a y dermatolog�a.


# Fijar valores conocidos (hombres,mujeres)
n <-c(datos$Oncolog�a[3] , datos$Dermatolog�a[3])
exitos <- c(datos$Oncolog�a[1] , datos$Dermatolog�a[1])
alfa <- 0.05

# m�todo de Wilson para la diferencia entre dos proporciones.
prueba <- prop.test(exitos, n = n , alternative = "two.sided", conf.level = 1 - alfa)
print (prueba)

# Respuesta : Dado que el valor p resultante es p = 0.6468, se se falla al rechazar la hip�tesis
# nula con un nivel de significaci�n ?? = 0,05. En consecuencia, podemos concluir con 95 % de confianza que
# la proporci�n de autoras en la especialidad de oncolog�a y dermatolog�a es, en efecto el mismo.

# ---------------------------- PREGUNTA 3 --------------------------------------------------------

# 3. Suponiendo que la diferencia en la proporci�n de autoras en la especialidad de psiquiatr�a y la de obstetricia
# es de 0,18. �A cu�ntos autores deber�amos monitorear para obtener un intervalo de confianza del 99% y poder
# estad�stico de 90%, si se intenta mantener aproximadamente la misma proporci�n de gente estudiada en cada
# caso?


# Calcular el tama�o de las �reas
n_psiq <- datos$Psiquiatr�a[3] 
n_obst <- datos$Obstetricia[3]

# Calcular la proporci�n de las �reas
p_psiq <- datos$Psiquiatr�a[1]/n_psiq
p_obst <- datos$Obstetricia[1]/n_obst

# Las proporciones son 0.41 para Psiquiatr�a y 0.52 para Obstetricia
# Por lo cual se debe estimar una proporci�n similar que cumpla la diferencia de estas en un 0.18
p2_psiq <- 0.38
p2_obst <- 0.56

# se define el nivel de significaci�n
alpha <- 0.01
# se define el poder dado
poder <- 0.9

fraccion <- n_psiq / (n_psiq + n_obst)

# Como el valor n de ambas especialidades son distintos (por las proporciones y totales)
# mediante bsamsize se calcula el valor para cada especialidad.
prueba3 <- bsamsize(p2_psiq, p2_obst, fraction = fraccion, alpha = alpha, power = poder)
print (prueba3)

# Resultados: El n�mero de autores que se deber�an monitorear para obtener un intervalo de confianza del 99% y poder
# estad�stico de 90%, donde se intenta mantener aproximadamente la misma proporci�n de gente estudiada en cada
# caso es N_Psiquiatr�a: 173 (para el area de Psiquiatr�a) y, N_Obstetricia: 329 (para el area de Obstetricia),
# ambos valores aproximandamente.

