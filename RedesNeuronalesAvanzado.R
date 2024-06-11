#Si Quisiera Manual

library(readr)
install.packages("neuralnet")  
install.packages("keras")  
install.packages("mlbench")  
install.packages("dplyr")  
install.packages("magrittr")  

library(neuralnet)
library(keras)
library(mlbench)
library(dplyr)
library(magrittr)


HousingData <- read_csv("C:/Users/Julian/Desktop/DataSets/HousingData.csv")

#View(HousingData) #Te deja ver todo el dataframe,solo un dato


HousingData
str(HousingData)
summary(HousingData)
nrow(HousingData)

complete <- complete.cases(HousingData)
data2 <- HousingData[complete,]


nrow(data2)
summary(data2)
#observacion rapida con neuralnet
n <- neuralnet(MEDV ~ .,
               data = data2,
               hidden = c(12,7),
               linear.output = F,
               lifesign = 'full',
               rep=1)
#grafico rapido de neural net
plot(n,col.hidden = 'darkgreen',     
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

#empecemos a particionar
seleccionTest2 <- runif(n=nrow(data2))
data_train2 <- data2[seleccionTest2 >.7 ,]
data_test2 <- data2[seleccionTest2 <= .7 ,]

?scale
data_train2 <- scale(data_train2, center = T, scale = T)
data_test2 <- scale(data_test2, center = T, scale = T)


data_train2

model <- keras_model_sequential()
model %>%
  layer_dense(units = 5, activation = 'relu', input_shape = c(13)) %>% #Inicial son n column y BUSCAR METODO PARA n COLUMN
  layer_dense(units = 1) #FINAL

model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop', 
                  metrics = 'mae') 

mymodel <- model %>%          
  fit(data_train2,MEDV,
      epochs = 2,
      batch_size = 32,
      validation_split = 0.2)

  
