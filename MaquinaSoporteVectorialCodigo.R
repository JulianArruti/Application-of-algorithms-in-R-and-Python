
#install.packages("pROC")o
#install.packages("caret")
#install.packages("mlr")

library(liver)
library(e1071)
library(pROC)
library(caret)

library(mlr)
#importar los datos
data( risk ) 
summary(risk)
risk
##limpiarlos y transformarlos si es necesario, requisitos solo normalizar porque acepta variable categoricas
#Escalado de las variables continuas


risk$age <- scale(risk$age)
risk$income <- scale(risk$income)
#saving variables for later desnormalization
scaled_nr_loans <- scale(risk$nr.loans)
risk$nr.loans <- scaled_nr_loans

nr_loans_media <- attr(scaled_nr_loans, "scaled:center")
nr_loans_desviacion <- attr(scaled_nr_loans, "scaled:scale")

#antes de correrlo --> tunear: encontrar los parametros optimos
#tune([lineal, polinomial, radial, y otraMas\
       #cost= varios)

m.tune <- tune(svm, risk~., data=risk, 
               ranges=list(kernel=c("linear", "polynomial","radial", "sigmoid"), 
                           cost=c(.1,.2,.5,1,2,4,5)))

# Obtener el resultado de summary
summary_result <- summary(m.tune)

# Crear un data frame con todos los resultados
summary_order <- as.data.frame(summary_result$performances)

# Ordenar el data frame por el error
summary_order_ordered <- summary_order[order(summary_order$error), ]
summary_order_ordered ##at 5 of cost it seems the best

###el mejor
# Obtener el resultado de summary
summary_result <- summary(m.tune)

# Crear un data frame con la información relevante
summary_order <- data.frame(summary_result$best.parameters, error = summary_result$best.performance)

# Ordenar el data frame por el error
summary_order_ordered <- summary_order[order(summary_order$error), ]
summary_order_ordered

###
#dividir en entrenamiento y testing
selection = sample(1:nrow(risk), nrow(risk)*.6) 

training = risk[selection,]

testing = risk[-selection,]



#corremos el algoritmo
modelo1 <- svm(risk ~ ., data = training, 
               kernel='radial', cost=5,
               probability=T,
               scale=FALSE)
modelo1

##Determinar si el algoritmo esta midiendo correctamente, se llevaran a cabo las siguientes pruebas:

#Predicciones y matriz de confunsion
predicciones.m1<-predict(modelo1,training)

table(predicciones.m1,training$risk)

#Ahora veamos en el testing set
predicciones.m1<-predict(modelo1,testing)

# Matriz de confusión 
table(predicciones.m1,testing$risk)


##Visualizar Curvas Roc
# Asumamos que 'clases_reales' son las clases reales de tus datos de prueba,  y 'predicciones' son las probabilidades predichas por tu modelo
predicciones.testing <- attr(predict(modelo1, newdata = testing, probability = TRUE), "probabilities")[, "bad risk"]

# Calcular la curva ROC
roc_obj <- roc(testing$risk, predicciones.testing)

# Imprimir el AUC
auc(roc_obj)

# Dibujar la curva ROC
plot(roc_obj)



#Seems that the model is able to make good predictions, at cost 5, it was tested at cost 6 but the specificity lightly increased and the fails in the test set increased, so 5 is the best i could find.
#Categorica
predicciones <- predict(object=modelo1, 
                        newdata=testing, #could be df with new clients
                        probability = TRUE, #poner el argumento en el modelo
                        decision.values = TRUE)
probabilidades <- attr(predicciones, "probabilities")

clase <- attr(predicciones, "decision.values")

clase_binaria <- ifelse(clase > 0, "Good Risk", "Bad Risk")
clase_binaria


#Regresiones
modeloReg <- svm(nr.loans ~ age+income, data = training, 
                 kernel="radial", cost=2,
                 scale=FALSE,
                 type=  "eps-regression")
modeloReg 

prediccionesReg <- predict(object=modeloReg, 
                           newdata=testing)
prediccionesReg #nos predice los valores pero estan normalizados, si quisieramos presentarlos

#Using pre saved values of ds and media
prediccionesReg_originales <- prediccionesReg * nr_loans_desviacion + nr_loans_media
prediccionesReg_originales










### instancias de chequeo preeliminar
##Validación cruzada estratificada
#necesitamos que no haya espacio entre los datos de la variable objetivo para usar esta liberia
risk$risk <- gsub(" ", "_", risk$risk)

# Configurar la validación cruzada estratificada
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE)


# Asumamos que 'clases' son las clases de tus datos, y 'datos' son tus datos de entrada
modelo_validacion <- train(risk ~ ., data = risk, method = "svmRadial", 
                           trControl = ctrl, 
                           tuneGrid = expand.grid(C = seq(1, 5, by = 0.5), 
                                                  sigma = 0.2249719))
modelo_validacion

# Configurar el remuestreo bootstrap
ctrl_bootstrap <- trainControl(method = "boot", number = 1, classProbs = TRUE)

# Entrenar el modelo
modelo_validacion_bootstrap <- train(risk ~ ., data = risk, method = "svmRadial", trControl = ctrl_bootstrap)




# Crear una tarea de aprendizaje
tarea <- makeClassifTask(data = risk, target = "risk")

# Crear un aprendiz
aprendiz <- makeLearner("classif.svm", predict.type = "prob")

# Calcular la curva de aprendizaje
curva <- generateLearningCurveData(learner = aprendiz, task = tarea, resampling = cv3, measures = list(auc))

# Dibujar la curva de aprendizaje
plotLearningCurve(curva)



###Bayes ingenuo, aplicacion en risk dataset

library(e1071)

modelo <- naiveBayes(risk ~ ., data = df)
modelo


# realizamos la prediccion con el modelo en el training
predicciones.m1<-predict(modelo,training)

# Matriz de confusión 
table(predicciones.m1,training$risk)

#probabilidades
predicciones.m1<-predict(modelo,risk,type = "raw")


typeof(predicciones.m1)
predicciones.m1

# Redondear cada valor en la matriz, NO SE RECORREN, SE MODIFICAN AL PARECER.
for (i in 1:nrow(predicciones.m1)) {
  predicciones.m1[i, ] <- round(predicciones.m1[i, ], digits = 2)
}
predicciones.m1[] 
