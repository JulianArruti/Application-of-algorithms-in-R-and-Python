#Grafico de nube de puntos para observar las relaciones entre las variables
plot(x=vendedores, y = ventasConRelacion, pch=16, col="red", main="¿Observan relación en estos datos?, Si.")

#averiguo si hay correlacion lineal
cor(ventasSinRelacion, vendedores)#como el número es bajo no tenemos indicio de relación




# Modelo de regresión lineal en R
#Para realizar un modelo de regresión lineal en R debemos utilizar la función `lm()` cuyos argumentos más importantes son:
  
#-`formula` es una expresión de la forma var1\~var2+var3+....+var_n donde var1 es la variable que queremos explicar (dependiente) y el resto son las variables explicativas (independientes).
#-`data` indicamos el conjunto de datos del cual provienen los datos

#aplico el modelo
#Calculamos los coeficientes  con de la relación
modeloCon<-lm(ventasConRelacion~vendedores)
summary(modeloCon)
plot(x=vendedores, y = ventasConRelacion,
     pch=16, col="red");abline(modeloCon, col="blue")