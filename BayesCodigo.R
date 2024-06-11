
#install.packages("liver")
library(liver)
data(risk)
df <- risk

## Particiono en trining y testing
#Nota: También se puede utilizar el modelo con variables continuas, pero las probabilidades condicionadas que calcula el modelo no son tan simples de interpretar.

#data sets, selecciono el 60% para entrenar
train <- sample(1:nrow(risk), nrow(risk)*.6)
training <- risk[train,]
testing <- risk[-train,]


install.packages("e1071")
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
