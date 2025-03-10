# Actividad 5 IME
# Grupo 6: Angel Avenda�o, Jorge Gonz�lez y Juan V�squez

# Se sabe que una m�quina que envasa detergentes industriales llena bidones con un volumen de producto que
# sigue una distribuci�n normal con desviaci�n est�ndar de 1 litro. Usando una muestra aleatoria de 100 botellas,
# el ingeniero a cargo de la planta requiere determinar si la m�quina est� llenando los bidones con una media de
# 10 litros

##################
#Error tipo I: rechazar H0 en favor de HA cuando H0 es en realidad verdadera.
#Error tipo II: no rechazar H0 en favor de HA cuando HA es en realidad verdadera
##################

if(!require(ggpubr)){
  install.packages("ggpubr",dependencies = TRUE)
  require(ggpubr)
}

if(!require(pwr)){
  install.packages("pwr",dependencies = TRUE)
  require(pwr)
}

desv_estandar <- 1
n <- 100


# 1. Si el ingeniero est� seguro de que el verdadero volumen medio no puede ser superior a 10 litros y
# piensa rechazar la hip�tesis nula cuando la muestra presente una media menor a 9,5 litros, �cu�l es la probabilidad
# de que cometa un error de tipo I?

# tenemos  una var=1 ; 

# exito = es HA

# H0: � = �0, esto es � = 10 litros
# HA: � < �0, esto es � < 10 litros

# Funcion de distribuci�n acumulada (es decir, la probabilidad de que la variable tome valores menores 
# o iguales que un valor dado
alfa_izq <- pnorm(9.5, mean = 10, sd = 1, lower.tail = TRUE)

# 2. Si el verdadero volumen medio de los bidones fuera de 9,7 litros, �cu�l ser�a la probabilidad de que el
# ingeniero, que obviamente no conoce este dato, cometa un error de tipo II?

# Error tipo II: no rechazar H0 en favor de HA cuando HA es en realidad verdadera
# se busca el "beta"


# H0 : � = 10 litros
# HA : � = 9.7 litros

# Calcular el error est�ndar .
SE <- desv_estandar/sqrt (n)

mu_verdadera <- 10

# Definir cantidad de puntos a usar en el eje x.
numero_puntos <- 100

# Gr�ficar la distribuci�n muestral de la media de las diferencias si  la hip�tesis nula fuera verdadera

# Generar una distribución t en torno al valor nulo.
x <- seq(mu_verdadera - 5.2*SE, mu_verdadera + 5.2*SE , length.out = numero_puntos)
y <- dnorm (x , mean = mu_verdadera, sd = SE)
distr1 <- data.frame(x, y)


# Graficar la distribuci�n
g <- ggplot (data = distr1, aes (x))

# - Agregar la distribuci�n normal.
g <- g + stat_function(fun = dnorm, args = list (mean = mu_verdadera, sd = SE),
                       colour = "red", size = 1)

# - Quitar etiquetas del eje y.  
g <- g + ylab("")

# - Quitar marcas del y.
g <- g + scale_y_continuous (breaks = NULL)

# - Agregar marcas y etiquetas al eje x.
g <- g + scale_x_continuous(name = "volumen [litros]",breaks = seq (5,15,2))
# - Dar formato con fondo blanco.
g <- g + theme_pubr()

# - Rotar etiquetas del eje x.
g <- g + theme(axis.text.x = element_text(angle = 30, size = 5))

# - Agregar la media bajo la hipotesis nula.
g <- g + geom_vline(xintercept = mu_verdadera,
                      colour = "red", linetype = "longdash")

# - Agregar titulo
g <- g + ggtitle("Distribuci�n de las medias muestrales bajo la hipotesis nula")

print(g)

# Colorear la regi�n de rechazo de la hip�tesis nula .
Z_critico <- qnorm (0.05 , mean = mu_teo , sd = SE , lower.tail = FALSE)
q_critico_inferior <- mu_verdadera - Z_critico


g2 <- g + geom_area(data = subset(distr1, x <= mu_verdadera), aes(y = y),
                    colour = "red", fill = "red", alpha = 0.5)

g2 <- g2 + ggtitle("Distribuci�n de las medias muestrales bajo la hipotesis nula")

print(g2)

# Ahora,la media te�rica.
mu_teo <- 9.7

g3 <- g2 + stat_function (
  fun = dnorm ,
  args = list (mean = mu_teo , sd = SE),
  colour = "blue", size = 1)

# Colorear la regi�n de la nueva curva situada en la regi�n de rechazo de la curva original.
x1 <- seq(mu_teo - 5.2*SE, mu_teo + 5.2*SE , length.out = numero_puntos)
y1 <- dnorm (x1 , mean = mu_teo , sd = SE)
print(y1)
distr2 <- data.frame (x1 , y1)
g3 <- g3 + geom_area (data = subset (distr2,
                                      x > Z_critico) ,
                       aes (x = x1 , y = y1) ,
                       colour = "blue",
                       fill = "blue",
                       alpha = 0.2)
print (g3)

# Calcular el poder de acuerdo al an� lisis te�rico.
poder <- pnorm (q_critico_inferior,
                mean = 0.3 ,
                sd = SE ,
                lower.tail = FALSE)

# Calcular el area de Z dado el valor critico de la cola superior de la media de H0 y la media te�rica.
z_beta <-  (Z_critico - mu_verdadera)/SE

# Calcular la probabilidad de cometer un error tipo II.
beta  <-  pnorm(z_beta, 0, 1)
poder <- 1 - beta

# Respuesta : Si el verdadero volumen medio de los bidones fuera de 9,7 litros, la probabilidad de que el
# ingeniero  cometa un error de tipo II es de 0.08768546.

# 3. Como no se conoce el verdadero volumen medio, genere un gr�fico del poder estad�stico con las condiciones
# anteriores, pero suponiendo que el verdadero volumen medio podr�a variar de 9,3 a 10 litros.

# Se genera la variaci�n del volumen medio.
x3 <- seq(9.3, 10, 0.01)

# Se define la cola inferior.
y_inf <- pnorm(9.3, mean = x3, sd = SE,
               lower.tail = TRUE)

# Se define la cola superior.
y_sup <- pnorm(10, mean = x3, sd = SE,
               lower.tail = FALSE)

# Se juntan para el gr�fico.
y3 <- y_inf + y_sup

# Se genera el data frame.
distr3 <- data.frame(x = x3, y = y3)
# Se genera el gr�fico.
g3 <- ggplot(distr3, aes(x, y))
# Se le cambia el color a la linea.
g3 <- g3 + geom_line(colour = "firebrick1")
# Se etiquetan ambos ejes
g3 <- g3 + ylab("Poder estad�stico")
g3 <- g3 + xlab("Volumen medio verdadero [L]")
g3 <- g3 + theme_pubr()
# Se formatea el texto.
g3 <- g3 + theme(axis.text.x = element_text(angle = 30, size = 10))
# Se le asigna titulo.
g3 <- g3 + ggtitle("Pregunta 3")
# Se muestra.
print(g3)

# 4. Considerando un volumen medio de 10 litros, �cu�ntos bidones deber�an revisarse para conseguir un poder
# estad�stico de 0,8 y un nivel de significaci�n de 0,05?

power.t.test(delta = 0.3, sd = 1, sig.level = 0.05,
             power =0.8, type = "one.sample",alternative = "one.sided")

# Respuesta: Como se puede ver en el calculo de poder de la prueba t, se deber�an revisar aproximadamente 70 bidones
# para conseguir un poder estad�stico de 0.8 y un nivel de significancia de 0.05, es decir, podr�a ocurrir error tipo I
# el 5% de las veces.

#5. �Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de cometer un error de 
# tipo I a un 1% solamente?

power.t.test(delta = 0.3, sd = 1, sig.level = 0.01,
             power =0.8, type = "one.sample",alternative = "one.sided")

# Respuesta: Si el ingeniero decidiera poner el nivel de significancia en 0.01 deber�an revisar aproximadamente 114
# bidones, sin embargo, al disminuir la probabilidad de cometer errores de tipo I se aumenta la probabilidad de cometer
# errores tipo II, y para el contexto del ejercicio ser�a lo mejor ya que si se rechazara H0 significa que los bidones
# no se est�n llenando hasta los 10 y hay que aumentar el llenado, sobrepasando los 10 litros (cometiendo error tipo I).