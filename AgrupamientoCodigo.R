#Importacion y observacion
library(liver)
data("churnCredit")
summary(churnCredit)
#eleccion de variables a modificar
#se eleigio churnCredit$open.to.buy, churnCredit$transaction.count, churnCredit$transaction.amount.12
datos <-churnCredit[,c("open.to.buy", "transaction.amount.12", "transaction.count")]

datos
summary(datos) #ACA SE VERAN NA valores, NO TIENE que haber esos valores para kmeans
print(datos)
#graficos
plot(churnCredit$open.to.buy, churnCredit$transaction.count)
plot(churnCredit$open.to.buy, churnCredit$transaction.amount.12)
plot(churnCredit$transaction.amount.12, churnCredit$transaction.count)

#los datos anteriores no estan normalizados
datos3 <- scale(datos, scale = T, center = T)

#graficos de datos normalizados
plot(datos3$open.to.buy, datos3$transaction.amount.12)
plot(datos3$transaction.count, datos3$transaction.amount.12)
plot(datos3$open.to.buy, datos3$transaction.count)


#uso de k-means: eleccion de clusters y vista de resultados

guardo <- numeric(10)
for (i in 1:10){
  clusters_ <- kmeans(x=datos3,centers=i)
  guardo[i]<-clusters_$tot.withinss
}
plot(x=1:10, y=guardo, type="b", xlab="clusters", ylab="Distancias internas")
#Lo optimo es 4

kmeans <- kmeans(x=datos3,centers=4)
kmeans$centers

#graficos de dispersion ETIQUETADOS con los centros, para inferir problemas

#este graficos es 
plot(datos3$transaction.count, datos3$transaction.amount.12, 
     pch="", xlab = "transaction.count", ylab="transaction.amount.12")
text(datos3$transaction.count,datos3$transaction.amount.12, 
     labels=kmeans_3$cluster, cex=0.5, col=kmeans_3$cluster)
#Grafico los centros
points(x=kmeans_3$centers[,3], y=kmeans_3$centers[,2], pch=16, col=1:4)

#este graficos es
plot(datos3$open.to.buy, datos3$transaction.count, pch="", x
     xlab = "open.to.buy", ylab="transaction.count")
text(datos3$open.to.buy,datos3$transaction.count, 
     labels=kmeans_3$cluster, cex=0.5, col=kmeans_3$cluster)
#Grafico los centros
points(x=kmeans_3$centers[,1], y=kmeans_3$centers[,3], pch=16, col=1:4)

#Si se quisiera agregar nuevos datos o un df

#conclusiones



#ADICIONAL
nuevos <- matrix(c(8000,7500, 50,   6000, 10000, 90), 
                 nrow=2, ncol=3, byrow = T)

distancias <- matrix(ncol=4, nrow=2, 0)

nuevos.normalizados <-nuevos
#nuevos.normalizados[,1]<-nuevos.normalizados[,1]+ medias[1] / desvio[1]
#nuevos.normalizados[,2]<-nuevos.normalizados[,2]+ medias[2] / desvio[2]
#nuevos.normalizados[,3]<-nuevos.normalizados[,3]+ medias[3] / desvio[3]
#con la funcion scale esos 3

nuevos <- matrix(c(8000,7500, 50,   6000, 10000, 90), 
                 nrow=2, ncol=3, byrow = T)

distancias <- matrix(ncol=4, nrow=2, 0)
for (j in 1:2){
  for (i in 1:4){
    distancias[j,i]<-sum((nuevos.normalizados[j,]-kmeans_3$centers[i,])**2)
  } #aca aunq parece choclo es (dato(xj) - centro(xi))**2 es un bascara
}
distancias
apply(distancias, 1, which.min)#la funcion which.min nos dice en que posicion esta el minimo
#otros algoritmos
## PAM

## clara
