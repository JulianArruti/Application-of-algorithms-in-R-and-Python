
library(liver)
data("bank")

#1) Análisis descriptivo

# Preprocesamiento de los datos
#arboles puede procesar tanto variables categoricas como numericas y combinadas, es muy util
#COMO SOLO UNA PRIMERA VISTA, TE DARA EL IMPACTO DE CADA VARIABLE, aunq no sepas como ya q es
#No supervisado

## preprocesamiento no es necesario


#2) Distribución de la varaible objetivo

#Queremos saber cuantos casos hay en el conjunto de datos con clientes que fueron contactados y suscribieron a un depósito.
table(bank$deposit)
print(bank)


#3) Particionar el conjunto de datos: Vamos a seleccionar un 70% para entrenar y un 30% para testear.
seleccionTest <- runif(n=nrow(bank))
print(seleccionTest)
bank_test <- bank[seleccionTest >.7 ,]
bank_tr <- bank[seleccionTest <= .7 ,] #Conjunto de entrenamiento sin balancea

##Observamos comno quedaron distribuidas las varaibles objetivos en el training y testing.
table(bank_tr$deposit)
table(bank_test$deposit)


#4) Creamos el árbol: 2 ejemplos, con un subconjunto o con todos. Se aconseja IR CON TODAS LAS VARIABLES priemro
library(rpart)

#TODOS, primer parametro
t <- rpart(formula = deposit~., method = "class", data = bank_tr)

#subconjunto
#f <- "deposit ~ age + job + marital +
#education + default + balance + housing + loan +
#contact + day + month"
#t <- rpart(formula = f, method = "class", data = bank_tr)

## Visualizamos el árbol

#install.packages("rpart.plot")
library(rpart.plot)
prp(t,type=5, box.palette = "Reds", tweak = 1.2, varlen = 20)
text(t, use.n = TRUE, cex = .8, xpd = NA)

plot(t, uniform = TRUE, compress = TRUE, branch = .2)
text(t, use.n = TRUE, cex = .8, xpd = NA)


# 5)Predicciones
predArbol1_tr <- predict (t, newdata = bank_tr, type="prob")
predArbol1_test <- predict (t, newdata = bank_test, type="prob")

## Matrices de confusión
table(ifelse(predArbol1_tr[,"yes"]>0.5, "yes", "no"), bank_tr$deposit)
table(ifelse(predArbol1_test[,"yes"]>0.5, "yes", "no"), bank_test$deposit)


#CONTROLES, CALIBRAR PARAMETROS DEL ALGORITMO
#-   minsplit : mínimo numero de observaciones par dividir un nodo
#-   maxdepth : es la profundidad máxima del nodo
#-   minbucket : mínimo numero de observaciones de un nodo


### Definimos nuevos parámetros
#Quiero un árbol más frondoso
#probar eliminar la variable month
t <- rpart(formula = deposit~.-month, method = "class", 
           data = bank_tr, control = controles)

controles <- rpart.control(minsplit = 20, minbucket = 20, 
                           cp =0.001, maxcompete = 4, 
                           maxsurrogate = 5,
                           usesurrogate = 2, 
                           xval = 10,surrogatestyle = 0,
                           maxdepth = 8)

# viasulaziation
prp(t,type=5, box.palette = "Reds", tweak = 1.2, varlen = 20)
text(t, use.n = TRUE, cex = .8, xpd = NA)

plot(t, uniform = TRUE, compress = TRUE, branch = .2)
text(t, use.n = TRUE, cex = .8, xpd = NA)

# Predicciones
predArbol1_tr <- predict (t, newdata = bank_tr, type="prob")
predArbol1_test <- predict (t, newdata = bank_test, type="prob")

table(ifelse(predArbol1_tr[,"yes"]>0.5, "yes", "no"), bank_tr$deposit)

table(ifelse(predArbol1_test[,"yes"]>0.5, "yes", "no"), bank_test$deposit)

# Curvas ROC
library(pROC)
roc_tr <- roc(response = bank_tr$deposit, predictor = predArbol1_tr[,1])
roc_test <- roc(response = bank_test$deposit, predictor = predArbol1_test[,1])
plot(roc_tr,col = "red",      legacy.axes = TRUE, print.auc.y = 0.9, print.auc = TRUE)
plot(roc_test, col = "blue", add = TRUE, print.auc.y = 0.65, print.auc = TRUE)

legend("bottom", c("Training", "Testing"),
       lty = c(1,1), lwd = c(2, 2), col = c("red", "blue"), cex = 0.75)


#Ahora tengo que determinar cuanto es la mejora que se obtiene de utilizar el algoritmo
# Valuación del negocio

#Ejemplo en el call center, en donde tengo costo de llamada y ganancia x venta
#- Costos Variables: 1000 AR\$ x contacto 
#- Ingresos: 2000 AR\$ x venta


#Me fijo mi piso, que seria sin algoritmo y simplemente llamar a gente
#Que pasa si contactamos al azar una determinada cantidad de clientes?

#eligiendo un umbral predeterminado de 0,5
umbral<-0.5
TP <- sum(predArbol1_test[,2]>umbral & bank_test$deposit == "yes")
FP <- sum(predArbol1_test[,2]>umbral & bank_test$deposit == "no")
FN <- sum(predArbol1_test[,2]<umbral & bank_test$deposit == "yes")
TN <- sum(predArbol1_test[,2]<umbral & bank_test$deposit == "no")


azar <- sample(nrow(bank_test), FP+TP)
muestraAzar<-ifelse(bank_test$deposit[azar]=="yes", 1, 0)

ResultadoAzar <- sum(muestraAzar*2000)  - 1000 * (FP+TP)
cat("La campaña tiene un beneficio esperado de $" , ResultadoAzar," contactando a ", FP+TP,  sep="")



# Optimizar el umbral: como no voy a ir uno por uno, hago una funcion que chequee los umbrales

#Defino una función para valorizar

res <- function(pp,hh,uu){
  2000 * sum(pp>uu & hh == "yes") - 1000 * sum(pp>uu)
}
res(predArbol1_test[,2],bank_test$deposit,umbral)
res(predArbol1_test[,2],bank_test$deposit,.9)
res(predArbol1_test[,2],bank_test$deposit,.1)

#Y si movemos el umbral

u <- seq(from=0, to=1, by=0.03) #Creo una secuencia de la cual tomar los umbrales
vres <- numeric(length(u)) #Me genero un vector para guardar resulados
for(i in 1:length(u)){
  vres[i] <-  res(predArbol1_test[,2],bank_test$deposit,u[i])}

summary(vres)

plot(y=vres, x=u, main="Resultados", ylab="umbrales")

#Los umbrales que generan el máximo:
which(vres == max(vres))
u[which(vres == max(vres))]

data.frame(u, vres)



library(party)
?ctree
?ctree_control

t2 <- party::ctree(formula=deposit~., data=bank_tr, 
                   controls = ctree_control(maxdepth = 3))
plot(t2)

plot(t2, type = "simple")#nos da la media en cada nodo terminal


#Obtener las predicciones
predicciones_test_ <- predict(t2,newdata=bank_test, type="response")#Obtenemos la respuesta
predicciones_test_Nodo <- predict(t2,newdata=bank_test, type="node")#Obtenemos el nodo final
predicciones_test_Prob <- sapply(predict(t2, newdata=bank_test,type="prob"),'[[',2)#Obtenemos el score
