# Actividad 2 IME
# Grupo 6: Angel Avenda�o, Jorge Gonz�lez y Juan V�squez

# La medida escogida para responder la pregunta fue la mediana
# debido a que es un valor m�s representativo que la media (promedio), dado que la
# distrbuci�n de los datos no es sim�trica.

# �Van los ingresos de las mujeres de la RM increment�ndose con la edad?
# R: En el gr�fico de barras se evidencia que desde los 18 a los 35 a�os aprox
# existe un aumento en los ingresos medianos de las mujeres en la RM, sin embargo, 
# a partir desde los 35 comienza a disminuir hasta llegar a los 75 a�os aproximadamente.
# Adem�s, luego de los 65 a�os se aprecia una fuerte disminucion del ingreso debido
# a la edad de jubilaci�n.


# Se cargan los paquetes necesario, en caso de no estar instalados se instalan.
if(!require(dplyr)){
  install.packages("dplyr",dependencies = TRUE)
  require(dplyr)
}

if(!require(ggpubr)){
  install.packages("ggpubr",dependencies = TRUE)
  require(ggpubr)
}

# Se carga el archivo de datos CSV
datos <- read.csv2(file.choose(new = FALSE), encoding="UTF-8")

# Se filtra del dataframe respecto a mujeres de la RM
datos <- datos %>% filter(sexo == "Mujer" & region == "Regi�n Metropolitana de Santiago")

#Se ordenan los datos filtrados por edad de forma ascendente
datosFiltro <- datos[order(datos$edad),]

# Se agrupan los datos seg�n edad para si obtener la mediana por edad
resultado <- datosFiltro %>% group_by(edad) %>% summarise_at(vars(ytot), list(med = median))

# Del resultado obtenido se genera un gr�fico de barras.
grafico <- ggbarplot(resultado ,
             x = "edad",
             y = "med",
             fill = c("blue"),
             title = "Edad mujeres RM v/s Ingreso mediano",
             xlab = "Edad de las mujeres RM",
             ylab = "Ingreso medio")

# Se muestra el gr�fico
print(grafico) 

