#####replica serie de tiempo covid Rangel
library(readxl)
library(tidyverse)
library(lubridate)
library(readxl)
econometriatabla2 <- read_excel("C:/Users/Usuario/Downloads/econometriatabla2.xlsx")
View(econometriatabla2)
attach(econometriatabla2)
colnames(econometriatabla2)=c("inflacion","Salario nominal","Gasto publico","Tipo de cambio real","Deficit o superavit fiscal","Masa monetaria (M1)")
names(econometriatabla2)

##como serie de tiempo
#no es linea recta, exponencial,
inflacion.ts=ts(econometriatabla2,start=c(2000,1) , freq=12 )
inflacion.ts
plot(inflacion.ts)
summary(econometriatabla2)



######series de tiempo######
###arima
install.packages("tseries")
install.packages("astsa")
install.packages("forecast")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("foreign")
install.packages("quantmod")
install.packages("foreign")
install.packages("vars")
install.packages("xts")
install.packages("timsac")
install.packages("mFilter")
install.packages("dynlm")
install.packages("nlme")
library(tseries)
library(astsa)
library(forecast)
library(tidyverse)
library(lubridate)
library(foreign)
library(quantmod)
library(xts)
library(vars)
library(xts)
library(timsac)
library(mFilter)
library(dynlm)
library(readxl)
#prueba dickey-fuller para una raiz unitaria
#para saber si es estacionaria o no
#es importante que exista estacionariedad en la serie 
##prueba de ruido blanco: a travez de la hipotesis ljung box test,"sirve para detectar si el modelo es bueno o no es bueno
##si está bien ajustado  y cumple con los requisitos de ruido blanco.
##media error igual a cero, varianza constante, no estar serialmente correlacionada, despues si podemos realizar pronosticos
econometriatabla2 <- read_excel("C:/Users/Usuario/Downloads/econometriatabla2.xlsx")
View(econometriatabla2)
colnames(econometriatabla2)=c("inflacion","Salario nominal","Gasto publico","Tipo de cambio real","Deficit o superavit fiscal","Masa monetaria (M1)")
attach(econometriatabla2)
names(econometriatabla2)
####variables solas
inflacionsola=c(inflacion)
salariosolo=c(`Salario nominal`)
gastosolo=c(`Gasto publico`)
tipodecambiosolo=c(`Tipo de cambio real`)
deficitsolo=c(`Deficit o superavit fiscal`)
masamonetariasola=c(`Masa monetaria (M1)`)


#convertir mi base de datos a una de serie de tiempo en conjunto e individuales
econometriatabla.ts=ts(econometriatabla2,start=c(2000,1) , freq=12 ) #general
inflacionsola.ts=ts(inflacionsola,start=c(2000,1) , freq=12 )
salariosolo.ts=ts(salariosolo,start=c(2000,1) , freq=12 )
gastosolo.ts=ts(gastosolo,start=c(2000,1) , freq=12 )
tipodecambiosolo.ts=ts(tipodecambiosolo,start=c(2000,1) , freq=12 )
deficitsolo.ts=ts(deficitsolo,start=c(2000,1) , freq=12 )
masamonetariasola.ts=ts(masamonetariasola,start=c(2000,1) , freq=12 )
#cuantas diferencias necesito para volverlas estacionarias en series de tiempo, diferencias o logaritmos
ndiffs(econometriatabla.ts)
ndiffs(inflacionsola.ts)
ndiffs(salariosolo.ts)
ndiffs(gastosolo.ts)
ndiffs(tipodecambiosolo.ts)
ndiffs(deficitsolo.ts)
ndiffs(masamonetariasola.ts)

##ver dato min y max
#ver tabla en series de tiempo
start(econometriatabla.ts); end(econometriatabla.ts) #saber cuando inicia y termina
econometriatabla.ts #todas las columnas de ts
start(inflacionsola.ts); end(inflacionsola.ts) #saber cuando inicia y termina
inflacionsola.ts ###solo la columna inflacion de ts
start(salariosolo.ts); end(salariosolo.ts) #saber cuando inicia y termina
salariosolo.ts
start(gastosolo.ts); end(gastosolo.ts) #saber cuando inicia y termina
gastosolo.ts
start(tipodecambiosolo.ts); end(tipodecambiosolo.ts) #saber cuando inicia y termina
tipodecambiosolo.ts
start(deficitsolo.ts); end(deficitsolo.ts) #saber cuando inicia y termina
deficitsolo.ts
start(masamonetariasola.ts); end(masamonetariasola.ts) #saber cuando inicia y termina
masamonetariasola.ts

#graficas serie de tiempo "falta que esten estacionarios"
plot(econometriatabla.ts,main="Determinantes de la inflación Enero 2000-Diciembre 2020")
plot(inflacionsola.ts,main="Inflacion Enero 2000-Diciembre 2020")
plot(salariosolo.ts,main="Salario nominal Enero 2000-Diciembre 2020")
plot(gastosolo.ts,main="Gasto publico Enero 2000-Diciembre 2020")
plot(tipodecambiosolo.ts,main="tipo de cambio Enero 2000-Diciembre 2020")
plot(deficitsolo.ts,main="Deficit o superavit fiscal Enero 2000-Diciembre 2020")
plot(masamonetariasola.ts,main="Masa monetaria Enero 2000-Diciembre 2020")

#resumen datos
summary(econometriatabla2) #podria servir solo esta
summary(inflacion)
summary(salariosolo)
summary(gastosolo)
summary(tipodecambiosolo)
summary(deficitsolo)
summary(masamonetariasola)

###OJOOOOOOOO, investigar más esta parte
#revisar si tiene estacionariedad "se requiere que sea así"
#seasonplot(econometriatabla.ts,col=rainbow(12),year.labels = TRUE) ##inquietud, en general no se puede porque estan medidas en diferentes unidades
seasonplot(inflacionsola.ts,col=rainbow(12),year.labels = TRUE)
seasonplot(salariosolo.ts,col=rainbow(12),year.labels = TRUE)
seasonplot(gastosolo.ts,col=rainbow(12),year.labels = TRUE)
seasonplot(tipodecambiosolo.ts,col=rainbow(12),year.labels = TRUE)
seasonplot(deficitsolo.ts,col=rainbow(12),year.labels = TRUE)
seasonplot(masamonetariasola.ts,col=rainbow(12),year.labels = TRUE)



#AUTOCORRELACIÓN
#para que una serie de tiempo sea estacionaria se requiere que la media
#y la varianza sean constantes a lo largo del tiempo

##GRAFICAS AUTOCORRELACIÓN
acf(econometriatabla.ts) #en esta relaciona todas, solo escogeriamos la primera fila, y la primera grafica de la segunda fila
acf(inflacionsola.ts)
acf(salariosolo.ts) #va disminuyendo lentamente, no estacionaria
acf(gastosolo.ts) #altibajos
acf(tipodecambiosolo.ts) #va disminuyendo, no estacionaria
acf(deficitsolo.ts) #altibajos
acf(masamonetariasola.ts) #va disminuyendo, no estacionaria

#ver cuantas diferencias hay que hacer "LO MISMO QUE ARRIBA"
ndiffs(econometriatabla.ts)
ndiffs(inflacionsola.ts)
ndiffs(salariosolo.ts)
ndiffs(gastosolo.ts)
ndiffs(tipodecambiosolo.ts)
ndiffs(deficitsolo.ts)
ndiffs(masamonetariasola.ts)


#convertir a estacionarias
#diferencias hasta convertirla
#GRAFICOS TSERIE ESTACIONARIAS
#usar diferenciacion (primera diferencia)
seriediferenciadaeconometriatabla=diff(econometriatabla.ts)
plot(seriediferenciadaeconometriatabla)
seriediferenciadainflacion=diff(inflacionsola.ts)
plot(seriediferenciadainflacion)
seriediferenciadasalario=diff(salariosolo.ts)
plot(seriediferenciadasalario)
seriediferenciadagasto=diff(gastosolo.ts)
plot(seriediferenciadagasto)
seriediferenciatipodecambio=diff(tipodecambiosolo.ts)
plot(seriediferenciatipodecambio)
seriediferenciadeficit=diff(deficitsolo.ts)
plot(seriediferenciadeficit)
seriediferenciamasamonetaria=diff(masamonetariasola.ts)
plot(seriediferenciamasamonetaria)

###Autocorrelación pero con serie estacionaria
acf(seriediferenciadaeconometriatabla)
acf(seriediferenciadainflacion)
acf(seriediferenciadasalario)
acf(seriediferenciadagasto)
acf(seriediferenciatipodecambio)
acf(seriediferenciadeficit)
acf(seriediferenciamasamonetaria)
#esto si es una serie de tiempo estacionaria, media constante
#no es perfecta porque tiene pico pero se puede utilizar

#ver cuantas diferencias faltan
ndiffs(seriediferenciadaeconometriatabla)
ndiffs(seriediferenciadainflacion) ##para saber cuantas diferencias necesito
ndiffs(seriediferenciadasalario)
ndiffs(seriediferenciadagasto)
ndiffs(seriediferenciatipodecambio)
ndiffs(seriediferenciadeficit)
ndiffs(seriediferenciamasamonetaria)

#otra diferencia, falta la otra de masa monetaria y al parecer tambien de la general

seriediferenciadaeconometriatabla2=diff(econometriatabla.ts,differences=2)
plot(seriediferenciadaeconometriatabla2)
acf(seriediferenciadaeconometriatabla2)
ndiffs(seriediferenciadaeconometriatabla2)

seriediferenciamasamonetaria2=diff(masamonetariasola.ts,differences = 2)
plot(seriediferenciamasamonetaria2)
acf(seriediferenciamasamonetaria2)
ndiffs(seriediferenciamasamonetaria2)

####Analisis visual de las graficas autocorrelación
#general
#par(mfrow=c(2,2),mar=c(4,4,4,1)+ .1)
#plot(econometriatabla.ts, ylab="Determinantes inflación")
#acf(econometriatabla.ts,main="Serie no estacionaria")
#plot(seriediferenciadaeconometriatabla2)
#acf(seriediferenciadaeconometriatabla2,main="Serie estacionaria")

#individual
par(mfrow=c(2,2),mar=c(4,4,4,1)+ .1)
plot(inflacionsola.ts, ylab=" inflación")
acf(inflacionsola.ts,main="Serie no estacionaria")
plot(seriediferenciadainflacion)
acf(seriediferenciadainflacion,main="Serie estacionaria")

par(mfrow=c(2,2),mar=c(4,4,4,1)+ .1)
plot(salariosolo.ts, ylab="Salario")
acf(salariosolo.ts,main="Serie no estacionaria")
plot(seriediferenciadasalario)
acf(seriediferenciadasalario,main="Serie estacionaria")

par(mfrow=c(2,2),mar=c(4,4,4,1)+ .1)
plot(gastosolo.ts, ylab="Gasto")
acf(gastosolo.ts,main="Serie no estacionaria")
plot(seriediferenciadagasto)
acf(seriediferenciadagasto,main="Serie estacionaria")

par(mfrow=c(2,2),mar=c(4,4,4,1)+ .1)
plot(tipodecambiosolo.ts, ylab="Tipo de cambio")
acf(tipodecambiosolo.ts,main="Serie no estacionaria")
plot(seriediferenciatipodecambio)
acf(seriediferenciatipodecambio,main="Serie estacionaria")

par(mfrow=c(2,2),mar=c(4,4,4,1)+ .1)
plot(deficitsolo.ts, ylab="Deficit o superavit fiscal")
acf(deficitsolo.ts,main="Serie no estacionaria")
plot(seriediferenciadeficit)
acf(seriediferenciadeficit,main="Serie estacionaria")

par(mfrow=c(2,2),mar=c(4,4,4,1)+ .1)
plot(masamonetariasola.ts, ylab="Deficit o superavit fiscal")
acf(masamonetariasola.ts,main="Serie no estacionaria")
plot(seriediferenciamasamonetaria2)
acf(seriediferenciamasamonetaria2,main="Serie estacionaria")

###test dickey-fuller
###adf.test(seriediferenciadaeconometriatabla2,alternative = "stationary")
#si el p es mayor a 0.5 no es estacionaria
adf.test(seriediferenciadainflacion,alternative = "stationary")
adf.test(seriediferenciadasalario,alternative = "stationary")
adf.test(seriediferenciadagasto,alternative = "stationary")
adf.test(seriediferenciatipodecambio,alternative = "stationary")
adf.test(seriediferenciadeficit,alternative = "stationary")
adf.test(seriediferenciamasamonetaria2,alternative = "stationary")
##h0=no es estacionario >0.05
##h1=es estacionario <0.05
#Rechazamos H0, la serie temporal es estacionaria


###Ahora si se puede hacer el arima
par(mfrow=c(2,1),mar=c(4,4,4,1)+ .1)
acf(seriediferenciadainflacion) #autocorrelacion, numero de medias moviles "en este caso los que se salen de los puntos azules"
pacf(seriediferenciadainflacion) #autocorrelacion parcial = #autoregresivos
acf(ts(seriediferenciadainflacion,frequency = 1)) ##para que el resago coincida con las frecuencias, cambia el eje x
pacf(ts(seriediferenciadainflacion,frequency = 1))
#7 MEDIAS MOVILES,5AUTOREGRESIVOS, 1 DIFERENCIA
#orden es autoregresivo, diferencias y media movil

par(mfrow=c(2,1),mar=c(4,4,4,1)+ .1)
acf(seriediferenciadasalario) #autocorrelacion, numero de medias moviles "en este caso los que se salen de los puntos azules"
pacf(seriediferenciadasalario) #autocorrelacion parcial = #autoregresivos
acf(ts(seriediferenciadasalario,frequency = 1)) ##para que el resago coincida con las frecuencias, cambia el eje x
pacf(ts(seriediferenciadasalario,frequency = 1))

par(mfrow=c(2,1),mar=c(4,4,4,1)+ .1)
acf(seriediferenciadagasto) #autocorrelacion, numero de medias moviles "en este caso los que se salen de los puntos azules"
pacf(seriediferenciadagasto) #autocorrelacion parcial = #autoregresivos
acf(ts(seriediferenciadagasto,frequency = 1)) ##para que el resago coincida con las frecuencias, cambia el eje x
pacf(ts(seriediferenciadagasto,frequency = 1))

par(mfrow=c(2,1),mar=c(4,4,4,1)+ .1)
acf(seriediferenciatipodecambio) #autocorrelacion, numero de medias moviles "en este caso los que se salen de los puntos azules"
pacf(seriediferenciatipodecambio) #autocorrelacion parcial = #autoregresivos
acf(ts(seriediferenciatipodecambio,frequency = 1)) ##para que el resago coincida con las frecuencias, cambia el eje x
pacf(ts(seriediferenciatipodecambio,frequency = 1))

par(mfrow=c(2,1),mar=c(4,4,4,1)+ .1)
acf(seriediferenciadeficit) #autocorrelacion, numero de medias moviles "en este caso los que se salen de los puntos azules"
pacf(seriediferenciadeficit) #autocorrelacion parcial = #autoregresivos
acf(ts(seriediferenciadeficit,frequency = 1)) ##para que el resago coincida con las frecuencias, cambia el eje x
pacf(ts(seriediferenciadeficit,frequency = 1))

par(mfrow=c(2,1),mar=c(4,4,4,1)+ .1)
acf(seriediferenciamasamonetaria2) #autocorrelacion, numero de medias moviles "en este caso los que se salen de los puntos azules"
pacf(seriediferenciamasamonetaria2) #autocorrelacion parcial = #autoregresivos
acf(ts(seriediferenciamasamonetaria2,frequency = 1)) ##para que el resago coincida con las frecuencias, cambia el eje x
pacf(ts(seriediferenciamasamonetaria2,frequency = 1))


#####Hasta acá voy bien
###De acá para abajo dudas
###### DUDA DE COMO CALCULARLO MAS EFICIENTEMENTE




#########################Lo que sigue está sin reajustar nuevamente

#1-forma de hacerlo
###consisite en probar con varios modelos
modelo1=arima(inflacionsola.ts,order = c(2,1,0)) ###modelo de serie de tiempo orginal, no el 2
modelo1
tsdiag(modelo1) #diagnostico



####2- forma de hacerlo
#Ajustamos el modelo.
#La función auto.arima de la librería forecast de R, proporciona una opción rápida para construir pronósticos
#con series temporales, debido a que evalúa entre todos los posibles modelos, al mejor modelo considerando 
#diversos criterios: estacionariedad, estacionalidad, diferencias, entre otras.
modelos=auto.arima(inflacionsola.ts, seasonal=FALSE) ####otra manera de hacerlooooo
modelos
summary(modelos)
modelos2=auto.arima(salariosolo.ts, seasonal=FALSE) ####otra manera de hacerlooooo
modelos2
summary(modelos2)
modelos3=auto.arima(gastosolo.ts, seasonal=FALSE) ####otra manera de hacerlooooo
modelos3
summary(modelos3)
modelos4=auto.arima(tipodecambiosolo.ts, seasonal=FALSE) ####otra manera de hacerlooooo
modelos4
summary(modelos4)
modelos5=auto.arima(deficitsolo.ts, seasonal=FALSE) ####otra manera de hacerlooooo
modelos5
summary(modelos5)
modelos6=auto.arima(deficitsolo.ts, seasonal=FALSE) ####otra manera de hacerlooooo
modelos6
summary(modelos6)
#ar=autoregresivos, ma1=media movil
#abajo los errores
#Usando la notación ARIMA presentada anteriormente, el modelo ajustado se puede escribir como:
#Y^dt=0.5471Yt???1???0.0649Yt???2+E
#donde E es un error y la serie original se diferencia con la orden 1.
tsdiag(modelos) #diagnostico para saber si el modelo es bueno
tsdiag(modelos2) #diagnostico para saber si el modelo es bueno pero no se que tan bueno
tsdiag(modelos3) #diagnostico para saber si el modelo es bueno
tsdiag(modelos4) #diagnostico para saber si el modelo es bueno
tsdiag(modelos5) #diagnostico para saber si el modelo es bueno
tsdiag(modelos6) #diagnostico para saber si el modelo es bueno

#errores estandarizados deben pareserse al ruido blanco
#valores p del estadistico de Ljung-Box, ver si hay o no ruido blanco
#linea azul; p=0.05, son mayores "modelo se ajusta bien
#h0:ruido clanco >0.05, h1:no hay ruido blanco<0.05
#####OJO= Ruido blanco significa que el error: media igual a cero, varianza constante, no estar serialmente correlacionada


#comprobarlo con el estadistico:
Box.test(residuals(modelos),type = "Ljung-Box")
#p mayor a 0.5 existe ruido blanco
error=residuals(modelos)
plot(error)

Box.test(residuals(modelos2),type = "Ljung-Box")
#p mayor a 0.5 existe ruido blanco
error2=residuals(modelos2)
plot(error2)

Box.test(residuals(modelos3),type = "Ljung-Box")
#p mayor a 0.5 existe ruido blanco
error3=residuals(modelos3)
plot(error3)

Box.test(residuals(modelos4),type = "Ljung-Box")
#p mayor a 0.5 existe ruido blanco
error4=residuals(modelos4)
plot(error4)

Box.test(residuals(modelos5),type = "Ljung-Box")
#p mayor a 0.5 existe ruido blanco
error5=residuals(modelos5)
plot(error5)

Box.test(residuals(modelos6),type = "Ljung-Box")
#p mayor a 0.5 existe ruido blanco
error6=residuals(modelos6)
plot(error6)



###ahora lo que se hace es pronosticar, "si el modelo es bueno"
pronostico<-forecast::forecast(modelos,h=10) ##10 meses más
pronostico
plot(pronostico)

pronostico2<-forecast::forecast(modelos2,h=10) ##10 meses más
pronostico2
plot(pronostico2)

pronostico3<-forecast::forecast(modelos3,h=10) ##10 meses más
pronostico3
plot(pronostico3)

pronostico4<-forecast::forecast(modelos4,h=10) ##10 meses más
pronostico4
plot(pronostico4)

pronostico5<-forecast::forecast(modelos5,h=10) ##10 meses más
pronostico5
plot(pronostico5)

pronostico6<-forecast::forecast(modelos6,h=10) ##10 meses más
pronostico6
plot(pronostico6)

pronostico7<-forecast::forecast(modelos,h=10) ##10 meses más
pronostico7
plot(pronostico7)

###modelo salario es malo creo


##################### con sarima nuestro modelo podría mejorar "extensión de modelos arimas"
# volver a comprobar la estacionariedad pero esta vez mediante el test de KPSS (Kwiatkowski-Phillips-Schmidt-Shin).



