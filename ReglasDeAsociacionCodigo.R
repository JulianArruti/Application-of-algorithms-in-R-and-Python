#Reglas de Asociacion en productos de supermercado
#install.packages("arules")
library(arules)
help(apriori)

data(Groceries)
inspect(Groceries[1:3])

## Crear tranascciones a partir de un data frame
a_df <- data.frame(
  age   = as.factor(c(6, 8, NA, 9, 16)), 
  grade = as.factor(c("A", "C", "F", NA, "C")),
  pass  = c(TRUE, TRUE, FALSE, TRUE, TRUE))  

a_df

## hacemos la transformación
trans1 <- as(a_df, "transactions") 
inspect(trans1)


#### Generar las reaglas desde el archivo transaccional
rules <- apriori(trans1, parameter = list(supp = 0.2, conf = 0.5, target = "rules"))
summary(rules)
inspect(rules)

'''
1 -   `data` es un objeto de clase transaccional
2-   `target` es un string que define el tipo de asociación.
3-   `support` es el mínimo soporte requerido para la regla
4-   `confidence` es la mínima confianza requerida a la regla
5-   `maxtime` es el tiempo máximo para que busque reglas. Si toma valor 0 no tiene límite de tiempo.
6-   `minlen` es el mínimo número de items en la regla
7-   `maxlen`es el máximo número de items en la regla
3, 4, 2
'''


'### Métricas

-   SOPORTE: Canastas en la que aparecen los items X e Y
-   CONFIANZA: Canastas en las que aparece X e Y sobre las canastas que aparece X, (P{X, Y}/(P{X})
-   LIFT: Es la confianza de la regla sobre la P{Y} en caso de independencia
-   COVERAGE: Soporte del precedente P(X)

Por ejemplo: REGLA: {X} => {Y}

| Support | Confidence | Coverage | Lift | Count |
|---------|------------|----------|------|-------|
| 0.25    | 1          | 0.25     | 4    | 1     |

-   Soporte: El set de items (X e Y) se observa en el 25% de las canastas
-   Confianza: El set de items {Y} se observa en el 100% de las canastas que aparece X. (P{X, Y}/P{X})
-   Coverage: Es el soporte del precedente P(X).
-   Lift: Soprte de la regla dividido El soporte de X por el soporte de Y. (P{X, Y}/(P{X}\*P{y}).
'''
##Exploratory 
reglas <- apriori(data = Groceries,
                  parameter = list(support = 0.01,
                                   confidence = 0.50,
                                   target = "rules", minlen=3))
summary(reglas)


inspect(reglas)
#Ordenar las relgas por el lift y Ordenar las relgas por el support
inspect(sort(x = reglas, decreasing = TRUE, by = "lift"))

inspect(sort(x = reglas, decreasing = TRUE, by = "support"))



# Análisis de los items

num_items <- size(Groceries) #Cantidad de items de cada canasta
summary(num_items)
hist(num_items)

#¿Cuál es el item más vendido?
frecuencia_items <- itemFrequency(x = Groceries, type = "relative")
frecuencia_items <- itemFrequency(x = Groceries, type = "absolute")

#Cosntruimos un DF con las frecuencias:
df<- data.frame(item=names(frecuencia_items), frecuencia=frecuencia_items, row.names = NULL)

which.max(frecuencia_items)#nos dice en que posición está el máximo
frecuencia_items[which.max(frecuencia_items)]#Nos dice en cuantos canastos esta el producto

#Hacemos un gráfico de barras con los principales 10 productos
barplot(height = sort(frecuencia_items, decreasing = TRUE)[1:10])



### Elijamos un objetivo
#Now lets find a product of interest, after seing the rules lets focus on yogurth, could be selected from the start if the customer ask
reglas_con_yogurt <- apriori(data = Groceries,
                             parameter = list(support = 0.01,
                                              confidence = 0.30,
                                              # Se especifica que se creen reglas
                                              target = "rules"),
                             appearance = list(rhs = "yogurt"))

inspect(reglas_con_yogurt)


#¿Que contengan yogurt OOOOO root vegetables?
reglas_yogurt_y_rootVeg <- arules::subset(x = reglas,
                                          subset = lhs %in% c("yogurt","root vegetables"))
inspect(reglas_yogurt_y_rootVeg)

#¿Que contengan yogurt YYYYY root vegetables?
reglas_yogurt_y_rootVeg <- arules::subset(x = reglas,
                                          subset = lhs %ain% c("yogurt","root vegetables"))
inspect(reglas_yogurt_y_rootVeg)


























#### Uso de clusters y reglas de asociacion para realizar segmentacion de clientes en...

# cargamos la librería para kmeans
#if (! ("plyr" %in% rownames (installed.packages ()))) {
#  install.packages ("plyr", dependencies = TRUE)
#}
library (plyr)
library(readxl)

#setwd("C://Users/Julian/Desktop/DataSets")  ##pendiente
#getwd()
raw.data <- read_excel("C://Users/Julian/Desktop/DataSets/TransactionsRetail/Online Retail.xlsx", sheet = "Online Retail", na = "NA")

data = raw.data
head(data)
nrow(data) 
summary(data) #Length: 541909 #CustomerID: NA's 135080 
str(data)

##preprocesamiento
#convert to date the variable invoice date
data$InvoiceDateFF<-as.Date(substr(as.character(data$InvoiceDate),1,10)) #se reviso con str(data) que se cambiara correctamente
range(data$InvoiceDateFF)  # [1] "2010-12-01" "2011-12-09" igual que visto en summary
#there are return that count has a negative value for clients
data$item.return <- grepl("C", as.character(data$InvoiceNo), fixed=TRUE)  
data$purchase.invoice <- ifelse(data$item.return=="TRUE", 0, 1)

##FUN para transformar a factor, funciono igual sin hacerlo, pero averiguar en que casos si
'''
Deberia averiguar que significan los valores negativos y si hay que tratarlos, lo de aca abajo no es necesario

#lets clean the negativa values, they are from the returns, they were put in like C
returns = data$UnitPrice[data$UnitPrice<0]

data <- data[which(data$UnitPrice < 0),]
unique(data$Description)
df_C <- grepl("C", as.character(data$InvoiceNo), fixed=TRUE)  
df_cc <- data[df_C,]
length(unique(df_cc$Description))
'''



#Como hay estudios previos de que los segmentos varían por ciudad, seleccionaremos una en particular:
table(data$Country)
dataold3 <- data  





# y ahora seleccionamos la ciudad  
data <- subset(data, Country == "United Kingdom")  
# las filas que quedan son:  
nrow(data)  # [1] 349806
# Y el número de Invoice y CustomerID únicos son:  
length(unique(data$InvoiceNo))  # [1] 19140  
length(unique(data$CustomerID))  # [1] 38


# obtencion de metricas y variables RFM ---------------------------  #  
# primero crearemos un dataset de clientes que completaremos después  
customers <- as.data.frame(unique(data$CustomerID))  
# dejamos la columna con el nombre de CustomerID  
names(customers) <- "CustomerID"  

# ahora el cálculo del recency  
data$recency <- as.Date("2011-12-10") - data$InvoiceDateFF  #  

# removemos los retornos, considerando sólo las compras: *purchase*, y lo dejamos en una tabla temporal
temp <- subset(data, purchase.invoice == 1)
# obtenemos el número de días desde la compra más reciente y lo dejamos en otra tabla temporal  
recency <- aggregate(recency ~ CustomerID, data=temp, FUN=min, na.rm=TRUE)  
# eliminamos la tabla temporal temp
remove(temp)
# agregamos el recency a la tabla de clientes que creamos  
customers <- merge(customers, recency, by="CustomerID", all=TRUE, sort=TRUE) 
# eliminamos la tabla temporal recency 
remove(recency)  
# dejamos como numérico el recency en la tabla de clientes  
customers$recency <- as.numeric(customers$recency) 


# ahora el cálculo del frecuency  
# seleccionamos algunos atributos  
customer.invoices <- subset(data, select = c("CustomerID","InvoiceNo", "purchase.invoice")) 
# eliminamos los duplicados  
customer.invoices <- customer.invoices[!duplicated(customer.invoices), ]   ###copiar
# ordenamos con CustomerID  
customer.invoices <- customer.invoices[order(customer.invoices$CustomerID),] 
row.names(customer.invoices) <- NULL  
# obtenemos el número de facturas por año solo para las compras  
annual.invoices <- aggregate(purchase.invoice ~ CustomerID, data=customer.invoices, FUN=sum, na.rm=TRUE)  
#cambiamos el nombre de la columna que agregó los datos a frecuency  
names(annual.invoices)[names(annual.invoices)=="purchase.invoice"] <- "frequency"  ##analizar forma, lo que hace nomas es cambiar nombre
# lo agregamos a los datos del cliente  
customers <- merge(customers, annual.invoices, by="CustomerID", all=TRUE, sort=TRUE)  

# eliminamos los datasets que ya no necesitamos  
remove(customer.invoices, annual.invoices)  



# removamos los clientes que no tienen ninguna compra en el año pasado  #copado ir viendo como se me tendria q ocurrir eso
customers <- subset(customers, frequency > 0)  

# calculemos el total gastado en cada item de cada factura  
data$Amount <- data$Quantity * data$UnitPrice  # agreguemos el total de ventas por cliente  
annual.sales <- aggregate(Amount ~ CustomerID, data=data, FUN=sum, na.rm=TRUE)  
# cambiemos el nombre de la columna a monetary 
names(annual.sales)[names(annual.sales)=="Amount"] <- "monetary"   

# agreguemos la columna monetary a nuestro dataset de clientes  
customers <- merge(customers, annual.sales, by="CustomerID", all.x=TRUE, sort=TRUE)  
# eliminemos el dataset temporal  
remove(annual.sales)

summary(customers)
head(customers)

# eliminamos el atributo anterior de fecha  
data$InvoiceDate <-NULL
#data.isna().sum()

"""
Esto va arriba en 1) Como ...united stats has more data, but lets check that this data is a good represation of the data set....
La variable ventas_por_cliente es la misma que annual sales, deberias unificar para que coincida

# Calculamos el total gastado por cada cliente
ventas_por_cliente <- aggregate(Amount ~ CustomerID + Country, data=data, FUN=sum)

# Ordenamos los clientes por el total gastado en cada país
ventas_ordenadas <- ventas_por_cliente[order(ventas_por_cliente$Country, -ventas_por_cliente$Amount),]

# Inicializamos un dataframe para almacenar los resultados
resultados <- data.frame(Country=character(), NumClientes=integer())

# Para cada país, seleccionamos el 20% de los clientes que más aportan
paises <- unique(ventas_ordenadas$Country)
for(pais in paises) {
  ventas_pais <- subset(ventas_ordenadas, Country == pais)
  num_clientes <- nrow(ventas_pais)
  top_20 <- head(ventas_pais, round(num_clientes * 0.2))
  resultados <- rbind(resultados, data.frame(Country=pais, NumClientes=nrow(top_20)))
}


# Mostramos los resultados
print(resultados) #United Kingdom tiene el 91% de clientes que mas aportan

sum(resultados$NumClientes)
"""

hist(customers$monetary)

customers$monetary <- ifelse(customers$monetary < 0, 0, customers$monetary) # reset negative numbers to zero  

hist(customers$monetary) # ahí desaparecieron  

#Ahora aplicaremos la regla clásica del 80/20 para considerar a los clientes más importantes (del orden del 20%) 
# ordenamos los clientes de mayor a menor monetary  
customers <- customers[order(-customers$monetary),]  
# calculamos el corte del 80%  
pareto.cutoff <- 0.8 * sum(customers$monetary)  
# de acuerdo a este corte clasificamos a los clientes 
customers$pareto <- ifelse(cumsum(customers$monetary) <= pareto.cutoff, "Top 20%", "Bottom 80%")  
# dejamos el atributo como factor  
customers$pareto <- factor(customers$pareto, levels=c("Top 20%", "Bottom 80%"), ordered=TRUE)  
# vemos los niveles  
levels(customers$pareto)  # [1] "Top 20%" "Bottom 80%" 
# y vemos la proporción que nos quedaron  
round(prop.table(table(customers$pareto)), 2) #Top 20% Bottom 80%    0.29 0.71
# eliminamos la variable que creamos para el corte  
remove(pareto.cutoff)  
# dejamos los datos ordenados por CustomerID  
customers <- customers[order(customers$CustomerID),]
head(customers)



###VISUALIZACION
# transformación logarítmica de las variables RFM  
customers$recency.log <- log(customers$recency)  
customers$frequency.log <- log(customers$frequency)  
# no podemos tener logaritmo de 0  
customers$monetary.log <- customers$monetary + 0.1   
customers$monetary.log <- log(customers$monetary.log) 
## cálculo de los Z-scores para las variables RFM  
#customers$recency.z <- scale(customers$recency.log, center=TRUE, scale=TRUE) 
#customers$frequency.z <- scale(customers$frequency.log, center=TRUE, scale=TRUE)  
#customers$monetary.z <- scale(customers$monetary.log, center=TRUE, scale=TRUE)    
# visualización con los datos logarítmicos  
scatter.2 <- ggplot(customers, aes(x = frequency.log, y = monetary.log))  
scatter.2 <- scatter.2 + geom_point(aes(colour = recency.log, shape = pareto))  
scatter.2 <- scatter.2 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))  
scatter.2 <- scatter.2 + scale_colour_gradient(name="Log-transformed Recency") 
scatter.2 <- scatter.2 + xlab("Log-transformed Frequency") 
scatter.2 <- scatter.2 + ylab("Log-transformed Monetary Value of Customer")  
scatter.2


customers$recency <- scale(customers$recency, center=TRUE, scale=TRUE) 
customers$frequency <- scale(customers$frequency, center=TRUE, scale=TRUE)  
customers$monetary <- scale(customers$monetary, center=TRUE, scale=TRUE)
head(customers)


#install.packages("ggplot2")
library(ggplot2)  
library(cluster)

# seleccionamos las columnas 9 a la 11 del dataset de clientes para trabajar
preprocessed <- customers[ , 6:8]
head(preprocessed)
# fijamos en un máximo de 10 clusters para probar
j <- 5

# creamos un data frame para alojar los resultados
models <- data.frame (k=integer (), tot.withinss=numeric (), betweenss=numeric (), totss=numeric (), rsquared=numeric ())

#ver si se puede meter en el bucle
'''
#Grafico etiquetando cluster
plot(datos3$transaction.count, datos3$transaction.amount.12, 
     pch="", xlab = "transaction.count", ylab="transaction.amount.12")
text(datos3$transaction.count,datos3$transaction.amount.12, 
     labels=kmeans_3$cluster, cex=0.5, col=kmeans_3$cluster)
'''

# aquí ciclo para iterar kmeans de 1 a 10 y obtener los resultados
for (k in 1:j) {
  # mostramos el k del ciclo
  print (k)

  # ejecutar kmeans
  output <- kmeans (preprocessed, centers = k, nstart = 20)
  
  # agregamos al data set de clientes la participación de en qué cluster queda, para análisis posterior
  var.name <- paste ("cluster", k, sep="_")
  customers[ , (var.name)] <- output$cluster
  customers[ , (var.name)] <- factor (customers[ , (var.name)], levels = c (1:k))
  
  # graficamos los clusters
  cluster_graph <- ggplot (customers, aes (x = frequency.log, y = monetary.log))
  cluster_graph <- cluster_graph + geom_point (aes (colour = customers[ , (var.name)]))
  colors <- c ('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')
  cluster_graph <- cluster_graph + scale_colour_manual (name = "Cluster Group", values=colors)
  cluster_graph <- cluster_graph + xlab ("Log-transformed Frequency")
  cluster_graph <- cluster_graph + ylab ("Log-transformed Monetary Value of Customer")
  title <- paste ("k-means Solution with", k, sep=" ")
  title <- paste (title, "Clusters", sep=" ")
  cluster_graph <- cluster_graph + ggtitle (title)
  print (cluster_graph)
  
  # vemos los centros
  print (title)
  cluster_centers <- ddply (customers, .(customers[ , (var.name)]), summarize, monetary=round (median (monetary),2), frequency=round (median (frequency),1), recency=round (median (recency), 0))
  names (cluster_centers)[names (cluster_centers)=="customers[ , (var.name)]"] <- "Cluster"
  print (cluster_centers)
  cat ("n")
  cat ("n")
  
  # recolectamos la información del modelo corrido
  models[k, ("k")] <- k
  models[k, ("tot.withinss")] <- output$tot.withinss
  models[k, ("betweenss")] <- output$betweenss
  models[k, ("totss")] <- output$totss
  models[k, ("rsquared")] <- round (output$betweenss/output$totss, 3)
  assign ("models", models, envir =.GlobalEnv)
  
  # removemos los datos de la iteración
  remove (output, var.name, cluster_graph, cluster_centers, title, colors)
}

# eliminamos en k
remove (k)



# gráfico de la varianza explicada según el número de clusters  
r2_graph <- ggplot(models, aes(x = k, y = rsquared))  
r2_graph <- r2_graph + geom_point() + geom_line() 
r2_graph <- r2_graph + scale_y_continuous(labels = scales::percent) 
r2_graph <- r2_graph + scale_x_continuous(breaks = 1:j) 
r2_graph <- r2_graph + xlab("k (Number of Clusters)")  
r2_graph <- r2_graph + ylab("Variance Explained")  
r2_graph
# gráfico de la suma de cuadrados por el número de clusters (codo)  
ss_graph <- ggplot(models, aes(x = k, y = tot.withinss))
ss_graph <- ss_graph + geom_point() + geom_line() 
ss_graph <- ss_graph + scale_x_continuous(breaks = 1:j)  
ss_graph <- ss_graph + scale_y_continuous(labels = scales::comma) 
ss_graph <- ss_graph + xlab("k (Number of Clusters)") 
ss_graph <- ss_graph + ylab("Total Within SS")  
ss_graph

# Realizar PCA
pca_resultado <- prcomp(preprocessed_clara) ##de donde pinga sale esto
summary(pca_resultado)
# Ver los coeficientes de las componentes principales
print(pca_resultado$rotation)


##DESEO: UTILIZAR REGLAS DE ASOCIACION PARA IDENTIFICAR PATRONES DE COMPRAS EN LOS COMENTARIO DE LOS PRODUCTOS, VER SI SE PUEDE CONDICIONAR EL 80% Y 20%

# Cargamos la librería arules 
library(arules)

# Supongamos que 'datos_cluster' es el dataframe resultante de k-means
# donde cada fila representa una transacción de un cliente y 
# cada columna representa un producto. Los valores son 1 si el cliente compró el producto y 0 en caso contrario.

# Convertimos los datos a una matriz dispersa
datos_matriz <- as(datos_cluster, "transactions")  ## averiguar nomas que va aca y proceder, falta acomodar y limpiar, averiguar las cosas anotadas.

# Aplicamos el algoritmo Apriori para encontrar reglas de asociación
reglas <- apriori(datos_matriz, parameter = list(supp = 0.001, conf = 0.8))

# Mostramos las reglas de asociación
inspect(reglas)
