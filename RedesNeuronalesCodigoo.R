# Caso de estudio
#Vamos a utilizar una red neuronal para predecir a los clientes con un alto riesgo en una cartera de clientes de un banco.

#install.packages("neuralnet")
library(neuralnet)
?neuralnet
library(liver)

###1)analisis descriptivo
data("risk")
summary(risk)
risk
###2)Preprocesamiento y limpieza
#- No puede haber variables categóricas
#- No puede haber valores faltantes

#prueba de escalar las variables en el training y testing

## Modificamos las variables categóricas a numéricas
risk$risk <- (risk$risk=="bad risk")*1
risk$mortgage <- (risk$mortgage=="yes")*1
risk$soltero <- (risk$marital=="single")*1
risk$casado <- (risk$marital=="married")*1
risk$marital<-NULL

risk
###3) Particionamos conjunto de entrenamiento y testeo
t <- sample(nrow(risk), 146)
train <- risk[t,]
test <- risk[-t,]

train
nrow(risk)
nrow(train)
##ESCALAMOS, normalizamos de antemano para ayudar a la red al darle una escala optima
#a mano
maximo <- max(train$income)
minimo <- min(train$income)
train$income <- (train$income - minimo)/(maximo - minimo)
test$income <- (test$income - minimo)/(maximo - minimo)
#con funcion
#train$income <- scale(x=train$income , center=FALSE)
#test$income <- scale(x=test$income, center=FALSE)
formula 2 
###4) Entrenamos la red
nn <- neuralnet(risk ~ .,
                data=train,
                hidden = 3, 
                linear.output = FALSE)

#graficamos
plot(nn)

### 5)Predicciones
pred <- predict(nn, train)
#Matriz de confusion de entrenamiento
table(train$risk, pred[, 1] > 0.5)

pred <- predict(nn, test)

##Matriz de confusion del testing
table(test$risk, pred[, 1] > 0.5)


# Producción: predecir casos nuevos con los pesos calibrados
nuevo1 <- data.frame(age=34,income=30000, mortgage=0, 
                     nr.loans=3,risk=NA, soltero=0,  casado=1 )

nuevo1$income <- (nuevo1$income - minimo)/(maximo - minimo)
pred <- predict(nn, nuevo1)
ifelse(pred>0.5, "Rechazado", "Aceptado")


#Otro ejercicio
nn <- neuralnet((Species == "setosa") + 
                  (Species == "versicolor") + 
                  (Species == "virginica")~ Petal.Length + Petal.Width, 
                hidden=3,
                data=train, 
                linear.output = FALSE)

#Predecimos y anlizamos la matriz de confusión.
pred <- predict(nn, test)
table(test$Species, apply(X=pred,MARGIN =  1,FUN= which.max))

plot(nn)

softplus <- function(x) log(1 + exp(x))

nn <- neuralnet((Species == "setosa") + 
                  (Species == "versicolor") + 
                  (Species == "virginica")~ Petal.Length + Petal.Width, 
                hidden=3,
                data=train, 
                linear.output = FALSE,
                act.fct = softplus)
plot(nn)

#TAREA, ANALIZAR MATRICES DE CONFUNSION DE AMBAS PARA VER COMO APORTA LA FUNCION ACTIVADORA
