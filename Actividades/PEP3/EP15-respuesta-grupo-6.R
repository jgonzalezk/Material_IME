# Actividad 15 IME
# Grupo 6: Angel Avendaño, Jorge González y Juan Vásquez

##----------------- Librerias -----------------------------------
if(!require(tidyverse)){
  install.packages("tidyverse",dependencies = TRUE)
  require(tidyverse)
}
if(!require(leaps)){
  install.packages("leaps",dependencies = TRUE)
  require(leaps)
}
if(!require(pROC)){
  install.packages("pROC",dependencies = TRUE)
  require(pROC)
}
if(!require(caret)){
  install.packages("caret",dependencies = TRUE)
  require(caret)
}

##-------------- Funciones

get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}

##-------------- Importando datos

poblacion <- read.csv2(file.choose(new = FALSE), encoding="utf8")


summary(poblacion)

#1.-  Definir la semilla a utilizar, que corresponde a los primeros cinco dígitos del RUN del integrante de mayor edad 
#del equipo.

set.seed(3734)


# 2. Seleccionar una muestra de 100 personas, asegurando que la mitad tenga estado nutricional “sobrepeso” y la 
# otra mitad “no sobrepeso”. 


# Crear la variable IMC (índice de masa corporal) como el peso de una persona (en kilogramos) dividida
# por el cuadrado de su estatura (en metros).
poblacion$IMC <- poblacion$Weight / ((poblacion$Height / 100) * (poblacion$Height / 100))

# Crear la variable dicotómica EN (estado nutricional) de acuerdo al valor de IMC de cada persona.
poblacion$EN <- ifelse(poblacion$IMC >= 23, "sobrepeso", "nosobrepeso")

# Obtener las muestras al sexo corespodiente y estado nutricional
muestraSP <- poblacion %>% filter(EN == "sobrepeso")
muestraSP <- sample_n(muestraSP, size = 50, replace = FALSE)

muestraNSP <- poblacion %>% filter(EN == "nosobrepeso")
muestraNSP <- sample_n(muestraNSP, size = 50, replace = FALSE)

# Union de las muestras en un frame
muestraEvaluacion <- rbind(muestraSP, muestraNSP)
#nrow(muestraEvaluacion)

# 3. Usando las herramientas del paquete leaps, realizar una búsqueda exhaustiva para seleccionar entre dos y 
# ocho predictores que ayuden a estimar la variable Peso (Weight), obviamente sin considerar las nuevas 
#variables IMC ni EN, y luego utilizar las funciones del paquete caret para construir un modelo de regresión 
#lineal múltiple con los predictores escogidos y evaluarlo usando bootstrapping.

#colnames(muestraEvaluacion)
# Ajustar modelo con todos los subconjuntos .

muestraEvaluacion_2 = select(muestraEvaluacion, -c(IMC ,EN))

modelos <- regsubsets(Weight ~ . , data = muestraEvaluacion_2 , method = "exhaustive",
                         nbest = 5 , nvmax = 20)
print(plot(modelos))

#+ A continuacion se encuentran los tres mejores seleccionadot por Adj.R2 CP BIC,
#+  se opta por el modelo con menos variables.

res.sum <- summary(modelos)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)



get_model_formula(66, modelos, "Weight") # Adj.R2 
get_model_formula(61, modelos, "Weight") # CP
get_model_formula(31, modelos, "Weight") # BIC

 mejor <- get_model_formula(31, modelos, "Weight") # 
 
 
# Entranmiento y validación por bootstraping

# Define training control
train.control <- trainControl(method = "boot", number = 400 ,
                              search= "random",returnResamp='all' ) # verboseIter = TRUE
# Train the model
model <- train(mejor, data = muestraEvaluacion_2, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

summary(model)


#+ Podemos apreciar que el entramiento por Bootstraping entrega un modelo robusto, 
#+ con todas sus variables significativas. El R cuadrado ajustado es de 0.9736,
#+ lo cual indica que se explica gran parte de la varianza con las variables 
#+ seleccionadas 
#+ 

# 4. Haciendo un poco de investigación sobre el paquete caret, en particular cómo hacer Recursive Feature 
# Elimination (RFE), construir un modelo de regresión lineal múltiple para predecir la variable IMC que incluya 
# entre 10 y 20 predictores, seleccionando el conjunto de variables que maximice R2 y que use cinco 
#repeticiones de validación cruzada de cinco pliegues para evitar el sobreajuste (obviamente no se debe 
#considerar las variables Peso, Estatura ni estado nutricional –Weight, Height, EN respectivamente).

muestraEvaluacion_3 = select(muestraEvaluacion, -c(Weight, Height, EN ))


control <- rfeControl(functions = lmFuncs, # linear regression
                      method = "repeatedcv", # repeated cv
                      repeats = 5, # number of repeats
                      number = 5) # number of folds


result_rfe1 <- rfe(x = select(muestraEvaluacion_3, -c(IMC)), 
                   y = muestraEvaluacion_3$IMC, 
                   sizes = c(10:20),
                   rfeControl = control)


#+ A continuacion se muestra que el modelo con 20 vairables maximiza el R2, por 
#+ lo tanto las siguientes variables son seleccionadas añcanzando un R2 de 0.8373. 
#+ 

print(result_rfe1)

plot(result_rfe1, type = c("g", "o"), metric = "Rsquared")

predictors(result_rfe1)

data_rfe_1 <- cbind(muestraEvaluacion_3[,predictors(result_rfe1)],muestraEvaluacion_3$IMC)

summary(lm(`muestraEvaluacion_3$IMC` ~ .  ,data = data_rfe_1))


# 5. Usando RFE, construir un modelo de regresión logística múltiple para la variable EN que incluya el conjunto, 
# de entre dos y seis, predictores que entregue la mejor curva ROC y que utilice validación cruzada dejando uno 
# fuera para evitar el sobreajuste (obviamente no se debe considerar las variables Peso, Estatura –Weight y 
# Height respectivamente– ni IMC).


muestraEvaluacion$EN<-ifelse(muestraEvaluacion$EN=="sobrepeso",1,0)
muestraEvaluacion$EN <- factor(muestraEvaluacion$EN)

muestraEvaluacion_4 <- select(muestraEvaluacion, -c(Weight, Height, IMC))

lrFuncs$summary <- twoClassSummary


control_1 <- rfeControl(functions = lrFuncs, # logistic regression
                      method = "cv", # repeated cv
                      repeats = 5,
                      number = 5,
                      returnResamp="final", )

muestraEvaluacion_4$EN <- factor(muestraEvaluacion_4$EN)

result_rfe2 <- rfe(x = select(muestraEvaluacion_4, -c(EN)), 
                   y = muestraEvaluacion_4$EN, 
                   sizes = c(2:6),
                   rfeControl = control_1,
                   metric= "ROC"
                   )


#+ A continuacion se encuentran las variables seleccionadas que maximizan el AUc 
#+ en la regresión, además de la gráfica con respecto a la métrica y el entrenamiento.
#+ 

print(result_rfe2)

plot(result_rfe2, type = c("g", "o"), metric ="ROC")

predictors(result_rfe2)

data_rfe_2 <- cbind(muestraEvaluacion_4[,predictors(result_rfe2)],muestraEvaluacion_4$EN)

summary(glm(`muestraEvaluacion_4$EN` ~ ., family = binomial(link = "logit")  ,data = data_rfe_2))
# 6. Pronunciarse sobre la confiabilidad y el poder predictivo de los modelos.

 #+ Con respecto al primer modelo de regresiones que intenta predecir la varibale
 #+ Weight con, podemos concluir que este es robusto, ya que todas sus variables 
 #+ son significativas y su R2 ajustado es de 0.97.

 #+ el segundo modelo, que intenta predecir el ÏMC, no consigue obtener todas sus 
 #+ variables significativas por medio de la RFE, debido a que esta máximiza el R2
 #+ el cual no penaliza el numero de variables del modelo, afectando asi la
 #+  parsimonia de este. Sin embargo, entrega un modelo de R2 maximo de 0.84.
  
 #+ EL ultimo modelo, que corresponde a la regresion logistica, tampoco
 #+ obtiene variables significativas por medio de este método, si logra obtener
 #+ un buen AUC máximo por medio de validacion cruzada.
 #+ 
 