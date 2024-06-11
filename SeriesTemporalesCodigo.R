library("tseries") 
library("forecast") 

data(AirPassengers)
str(AirPassengers)
AirPassengers

plot(AirPassengers)



#-   La serie tiene tendencia
#-   La serie tiene estacionalidad
#-   La serie presenta variabilidad que crece con el nivel de la serie, lo cual se conoce como heterocedasticidad.
#entonces ahora que a ojo vimos esas caracteristicas, averiguemos los valores
#y si afectan lo suficiente a que la prediccion sea precisa o no



#Deberia hacer otro grafico tomando otras variables
#deberia llevar a cabo un test de hipotesis, para determinar si la tendencia afecta o no
#realizar una descomposicion de las variables para tener medido cuanto es su incremento


###: analisis exploratorio

#grafico parara ver la tendencia: 
#grafico de cajas, analizamos con esto como se distribuye la media movil
boxplot(AirPassengers ~ round(time(AirPassengers)))

##grafico de la periodicidad, vemos que tiene una tendencia alta en los meses 6 a 9 y decae
#cycle(AirPassengers)
boxplot(AirPassengers ~ cycle(AirPassengers))


#correlacion
plot(x=AirPassengers, y = lag(AirPassengers, k=12), type="p")

df<-data.frame(y=AirPassengers[13:144], x=AirPassengers[1:132])  #ACA
lm(y~x, data=df)
##ACA LOS COEFICIENTES NO CAMBIAN PORQUE DEBERIA CMBIAR LOS VALORES DEL ACA
#coefficients: a 1 y 12, entonces la correlacion  es consistente a lo largo del tiempo.
#  (Intercept)            x  
#        12.666        1.073  


#con una idea ya realizada procedo a realizar los tests

?ts

ajuste <- auto.arima(AirPassengers, seasonal=T, stationary=F) 
plot(AirPassengers) 
lines(fitted(ajuste),col=2) 



plot(ajuste$residuals)
hist(ajuste$residuals)

predicciones <-forecast(ajuste, 36) 
plot(predicciones) #parece predecir muy bien


#PARA CASOS QUE NO CUMPLE IDEALIDAD
### Estabilización de la varianza (frecuencia)
#Si fuera necesario estabilizar la variabilidad se suelen tomar logaritmos. Representamos la serie transformada mediante un gráfico:

##Cuando nos encontramos con series que presentan estacionalidad y tendencia lo que podemos hacer es utilizar modelos que descomponen en distintas componentes.
# Métodos que descomponen la serie
ajuste <- tbats(AirPassengers)

#tbats te genera 2 cosas, primero  podes graficar sus componentes para ver la descomposicion
componentes <- tbats.components(ajuste) 
plot(componentes) 

# te genera una serie de datos similar a df, que lo pasas a df y  ???
componentes <- as.data.frame(componentes)
componentes$control<- (componentes$level+componentes$season)+componentes$slope
componentes$control_ <- exp(componentes$control)
componentes$observado <- AirPassengers

ajuste$beta
ajuste$lambda
ajuste$alpha
ajuste$fitted.values


forecast(ajuste, h = 12)


predicciones <- forecast(ajuste,24) 
plot(predicciones) 


##Preguntar que seria el concepto de renormalizacion, que implica por q no entiendo
### X-13-Arima-Seats

#Algunos modelos como el X-13-Arima-Seats descomponen la serie en mayor cantidad de componentes, por ejemplo, carnabal, semana santa, etc.

### Otros métodos de descomposición

```{r}
decomp <- stlm(AirPassengers,s.window = "periodic")
```

### Función para crear una serie con múltiple estacionalidad

```{r}
y.msts <- msts(AirPassengers, seasonal.periods=c(12, 4))
```