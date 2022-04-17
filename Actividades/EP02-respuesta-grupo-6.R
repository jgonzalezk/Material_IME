# Actividad 2 IME
# Grupo 6: Angel Avendaño, Jorge González y Juan Vásquez

# La medida escogida para responder la pregunta fue la mediana
# debido a que es un valor más representativo que la media (promedio), dado que la
# distrbución de los datos no es simétrica.

# ¿Van los ingresos de las mujeres de la RM incrementándose con la edad?
# R: En el gráfico de barras se evidencia que desde los 18 a los 35 años aprox
# existe un aumento en los ingresos medianos de las mujeres en la RM, sin embargo, 
# a partir desde los 35 comienza a disminuir hasta llegar a los 75 años aproximadamente.
# Además, luego de los 65 años se aprecia una fuerte disminucion del ingreso debido
# a la edad de jubilación.


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
datos <- datos %>% filter(sexo == "Mujer" & region == "Región Metropolitana de Santiago")

#Se ordenan los datos filtrados por edad de forma ascendente
datosFiltro <- datos[order(datos$edad),]

# Se agrupan los datos según edad para si obtener la mediana por edad
resultado <- datosFiltro %>% group_by(edad) %>% summarise_at(vars(ytot), list(med = median))

# Del resultado obtenido se genera un gráfico de barras.
grafico <- ggbarplot(resultado ,
             x = "edad",
             y = "med",
             fill = c("blue"),
             title = "Edad mujeres RM v/s Ingreso mediano",
             xlab = "Edad de las mujeres RM",
             ylab = "Ingreso medio")

# Se muestra el gráfico
print(grafico) 

