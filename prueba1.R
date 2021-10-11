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
install.packages("fpp2")
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
library(fpp2)
library(zoo)
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
#diferenciaciones regulares
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

#resumen datos originales SIN ESTAR EN SERIE DE TIEMPO
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

##GRAFICAS AUTOCORRELACIÓN "lineas"
acf(econometriatabla.ts) #en esta relaciona todas, solo escogeriamos la primera fila, y la primera grafica de la segunda fila
acf(inflacionsola.ts)
acf(salariosolo.ts) #va disminuyendo lentamente, no estacionaria
acf(gastosolo.ts) #altibajos
acf(tipodecambiosolo.ts) #va disminuyendo, no estacionaria
acf(deficitsolo.ts) #altibajos
acf(masamonetariasola.ts) #va disminuyendo, no estacionaria

#adf.test(econometriatabla2,alternative = "stationary")
#si el p es mayor a 0.5 no es estacionaria, dickey fuller antes
adf.test(inflacionsola,alternative = "stationary")#no requeriria
adf.test(salariosolo,alternative = "stationary")#si requeriria
adf.test(gastosolo,alternative = "stationary") #no requeriria
adf.test(tipodecambiosolo,alternative = "stationary") #si requeriria
adf.test(deficitsolo,alternative = "stationary") #no requeriria
adf.test(masamonetariasola,alternative = "stationary") #si requeriria

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

########
###Autocorrelación pero con serie estacionaria "lineas"
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
#serie diferenciada y además relaciona la inflación con cada variable exogena
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
#muestra 4 iconos, graficas y autorrelaciones "lineas", tanto no estacionarias como estacionarias
#par(mfrow=c(2,36),mar=c(4,4,4,1)+ .1)
#plot(econometriatabla.ts, ylab="Determinantes inflación")
#acf(econometriatabla.ts,main="Serie no estacionaria")
#plot(seriediferenciadaeconometriatabla2)
#acf(seriediferenciadaeconometriatabla2,main="Serie estacionaria")

#individual ##### MEDIAS MOVILES COMPONENTE REGULAR Y ESTACIONAL ACF Y PACF#########################################
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

###test dickey-fuller despues de haberlas vuelto estacionarias ##################################
#adf.test(seriediferenciadaeconometriatabla2,alternative = "stationary")
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
####Las volvió todas 0.01 "Incluso menor"

###Ahora si se puede hacer el arima "autocorrelación y autocorrelación parcial"#############
par(mfrow=c(2,1),mar=c(4,4,4,1)+ .1)
acf(seriediferenciadainflacion) #autocorrelacion, numero de medias moviles "en este caso los que se salen de los puntos azules"
pacf(seriediferenciadainflacion) #autocorrelacion parcial = #autoregresivos
acf(ts(seriediferenciadainflacion,frequency = 1)) ##para que el resago coincida con las frecuencias, cambia el eje x
pacf(ts(seriediferenciadainflacion,frequency = 1))
#7 MEDIAS MOVILES,5AUTOREGRESIVOS, 1 DIFERENCIA A OJO CON EL AUTOMATICO DA OTRA VUELTA
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
######################### Lo que sigue está sin reajustar nuevamente ############
############################CREACIÓN DE MODELOS###################
#1-forma de hacerlo "A OJO"
###consisite en probar con varios modelos
#7 MEDIAS MOVILES,5AUTOREGRESIVOS, 1 DIFERENCIA A OJO CON EL AUTOMATICO DA OTRA VUELTA
modelo1=arima(inflacionsola.ts,order = c(2,1,0)) ###modelo de serie de tiempo orginal, no el 2
modelo1   ####bota sigma cuadrado=varianza#######
tsdiag(modelo1) #diagnostico


####2- forma de hacerlo
#Ajustamos el modelo.
#La función auto.arima de la librería forecast de R, proporciona una opción rápida para construir pronósticos
#con series temporales, debido a que evalúa entre todos los posibles modelos, al mejor modelo considerando 
#diversos criterios: estacionariedad, estacionalidad, diferencias, entre otras.
modelos=auto.arima(inflacionsola.ts, seasonal=FALSE) ####otra manera de hacerlooooo, seriediferenciadainflacion; SOLO Cambia  (ARIMA(2,0,0) with zero mean ) el resto igual
#INFLACIONSOLA.TS ó seriediferenciadainflacion cualquiera sirve, lo que cambia está en las "diferencias"
modelos  #################### AIC=70.05,AIC=70.15, BIC=80.63################################
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
modelos6=auto.arima(masamonetariasola.ts, seasonal=FALSE) ####otra manera de hacerlooooo
modelos6
summary(modelos6)
#ar=autoregresivos, ma1=media movil
#abajo los errores
#Usando la notación ARIMA presentada anteriormente, el modelo ajustado se puede escribir como:
#Y^dt=0.5471Yt???1???0.0649Yt???2+E
#donde E es un error y la serie original se diferencia con la orden 1.

#La diagnosis del modelo estimado
tsdiag(modelos) #diagnostico para saber si el modelo es bueno
tsdiag(modelos2) #diagnostico para saber si el modelo es bueno TIENE SOLO UNO POR DEBAJO
tsdiag(modelos3) #diagnostico para saber si el modelo es bueno
tsdiag(modelos4) #diagnostico para saber si el modelo es bueno
tsdiag(modelos5) #diagnostico para saber si el modelo es bueno, REGULAR, INCLUSO MAS PAILAS QUE EL GASTO 5 POR DEBAJO DE 0.05
tsdiag(modelos6) #diagnostico para saber si el modelo es bueno, IGUAL DE IRREGULAR AL LIMITE, 1 SOLO POR DEBAJO

#errores estandarizados deben pareserse al ruido blanco
#valores p del estadistico de Ljung-Box, ver si hay o no ruido blanco
#linea azul; p=0.05, son mayores "modelo se ajusta bien
#h0:ruido clanco >0.05, h1:no hay ruido blanco<0.05
#####OJO= Ruido blanco significa que el error: media igual a cero, varianza constante, no estar serialmente correlacionada
#la creación de objetos de clase ts() requiere que las observaciones estén distribuidas de modo regular a lo largo del tiempo. En caso de que no sea así, podemos utilizar objetos de la clase zoo
#no se que tan necesaria sea



#comprobarlo con el estadistico: BOX.TEST=prueba general de independencia 
#Si el valor p es mayor que el nivel de significancia, usted puede concluir que los residuos son 
#independientes y que el modelo cumple con el supuesto.
##el modelo se ajusta a los datos, se puede concluir bien 
Box.test(residuals(modelos),type = "Ljung-Box")
#p mayor a 0.05 existe ruido blanco
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
pronostico<-forecast::forecast(modelos,h=36) ##10 meses más
pronostico
plot(pronostico)

pronostico2<-forecast::forecast(modelos2,h=36) ##10 meses más
pronostico2
plot(pronostico2)

pronostico3<-forecast::forecast(modelos3,h=36) ##10 meses más
pronostico3
plot(pronostico3)

pronostico4<-forecast::forecast(modelos4,h=36) ##10 meses más
pronostico4
plot(pronostico4)

pronostico5<-forecast::forecast(modelos5,h=36) ##10 meses más
pronostico5
plot(pronostico5)

pronostico6<-forecast::forecast(modelos6,h=36) ##10 meses más
pronostico6
plot(pronostico6)

pronostico7<-forecast::forecast(modelos,h=36) ##10 meses más
pronostico7
plot(pronostico7)
###modelo salario es malo creo




##################### con sarima nuestro modelo podría mejorar "extensión de modelos arimas"
# volver a comprobar la estacionariedad pero esta vez mediante el test de KPSS (Kwiatkowski-Phillips-Schmidt-Shin).
#### (PDQ)m "m=periodo de tiempo mensual", el resto en estacional

#ver cuantas diferencias hay que hacer "LO MISMO QUE ARRIBA"
#diferenciaciones regulares
####sarima va con s
#n por componente estacional
#nsdiffs(econometriatabla.ts) #orden 0 "OJO"
nsdiffs(inflacionsola.ts) #orden 0
nsdiffs(salariosolo.ts) #orden 1
nsdiffs(gastosolo.ts) #orden 1
nsdiffs(tipodecambiosolo.ts) #orden 0
nsdiffs(deficitsolo.ts) #orden 1
nsdiffs(masamonetariasola.ts) #orden 1

#inflacion
acf(inflacionsola) #autocorrelacion
pacf(inflacionsola) #orden de las medias moviles
modelosarima1=auto.arima(inflacionsola.ts,stepwise = FALSE,approximation = FALSE)
summary(modelosarima1) ###propuesta 
residuals=resid(modelosarima1)
adf.test(residuals) #si es ruido blanco, mejorpronostico sugerido
coeftest(modelosarima1)
forecast(modelosarima1,h=10)
autoplot(acf(modelosarima1$residuals, plot = FALSE))
autoplot(pacf(modelosarima1$residuals, plot = FALSE))
ggseasonplot(inflacionsola.ts,main="plot sarima ")
tsdiag(modelosarima1)
#Los p-valores para la prueba Q de Ljung-Box están por encima de 0,05, lo que indica "no significativo".
#Realizamos el contraste de hipótesis:
#H0: Los datos se distribuyen de forma independiente
#H1: Los datos no se distribuyen de forma independiente
independencia <- Box.test(modelosarima1$residuals, type="Ljung-Box") # Test de Ljung-Box
independencia$p.value
#Efectivamente, los datos se distribuyen de forma independiente. P-valor = 0.53>0.05
qqnorm(modelosarima1$residuals)
qqline(modelosarima1$residuals) 
#Gráficamente observamos que los datos siguen una distribución normal aunque en las colas los datos se alejan y se dispersan un poco, para una mayor seguridad comprobamos la normalidad mediante el test Shapiro Wilk:
#H0:  Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente
normalidad <-shapiro.test(modelosarima1$residuals)    # Test de Shapiro-Wilk
normalidad$p.value  
#p valor 0.70>0.05 no rechazamos nuestra hipótesis nula, los residuos siguen una distribución normal.
#Por lo tanto, damos el modelo por VÁLIDO
prediccion123 <- forecast(modelosarima1, h=36) #nivel confianza 95%, h = periodos
autoplot(prediccion123)

#SALARIO
acf(salariosolo) #autocorrelacion
pacf(salariosolo) #orden de las medias moviles
modelosarima2=auto.arima(salariosolo.ts,stepwise = FALSE,approximation = FALSE)
summary(modelosarima2) ###propuesta 
residuals=resid(modelosarima2)
adf.test(residuals) #si es ruido blanco, mejorpronostico sugerido
coeftest(modelosarima2)
forecast(modelosarima2,h=10)
autoplot(acf(modelosarima2$residuals, plot = FALSE))
autoplot(pacf(modelosarima2$residuals, plot = FALSE))
ggseasonplot(salariosolo.ts,main="plot sarima ")
tsdiag(modelosarima2)
#Los p-valores para la prueba Q de Ljung-Box están por encima de 0,05, lo que indica "no significativo".
#Realizamos el contraste de hipótesis:
#H0: Los datos se distribuyen de forma independiente
#H1: Los datos no se distribuyen de forma independiente
independencia2 <- Box.test(modelosarima2$residuals, type="Ljung-Box") # Test de Ljung-Box
independencia2$p.value
#Efectivamente, los datos se distribuyen de forma independiente. P-valor = 0.53>0.05
qqnorm(modelosarima2$residuals)
qqline(modelosarima2$residuals) 
#Gráficamente observamos que los datos siguen una distribución normal aunque en las colas los datos se alejan y se dispersan un poco, para una mayor seguridad comprobamos la normalidad mediante el test Shapiro Wilk:
#H0:  Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente
normalidad2 <-shapiro.test(modelosarima2$residuals)    # Test de Shapiro-Wilk
normalidad2$p.value  
#p valor 0.70>0.05 no rechazamos nuestra hipótesis nula, los residuos siguen una distribución normal.
#Por lo tanto, damos el modelo por VÁLIDO
prediccion1234 <- forecast(modelosarima2, h=36) #nivel confianza 95%, h = periodos
autoplot(prediccion1234)

#Gasto
acf(gastosolo) #autocorrelacion
pacf(gastosolo) #orden de las medias moviles
modelosarima3=auto.arima(gastosolo.ts,stepwise = FALSE,approximation = FALSE)
summary(modelosarima3) ###propuesta 
residuals=resid(modelosarima3)
adf.test(residuals) #si es ruido blanco, mejorpronostico sugerido
coeftest(modelosarima3)
forecast(modelosarima3,h=10)
autoplot(acf(modelosarima3$residuals, plot = FALSE))
autoplot(pacf(modelosarima3$residuals, plot = FALSE))
ggseasonplot(gastosolo.ts,main="plot sarima ")
tsdiag(modelosarima3)
#Los p-valores para la prueba Q de Ljung-Box están por encima de 0,05, lo que indica "no significativo".
#Realizamos el contraste de hipótesis:
#H0: Los datos se distribuyen de forma independiente
#H1: Los datos no se distribuyen de forma independiente
independencia3 <- Box.test(modelosarima3$residuals, type="Ljung-Box") # Test de Ljung-Box
independencia3$p.value
#Efectivamente, los datos se distribuyen de forma independiente. P-valor = 0.53>0.05
qqnorm(modelosarima3$residuals)
qqline(modelosarima3$residuals) 
#Gráficamente observamos que los datos siguen una distribución normal aunque en las colas los datos se alejan y se dispersan un poco, para una mayor seguridad comprobamos la normalidad mediante el test Shapiro Wilk:
#H0:  Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente
normalidad3 <-shapiro.test(modelosarima3$residuals)    # Test de Shapiro-Wilk
normalidad3$p.value  
#p valor 0.70>0.05 no rechazamos nuestra hipótesis nula, los residuos siguen una distribución normal.
#Por lo tanto, damos el modelo por VÁLIDO
prediccion12345 <- forecast(modelosarima3, h=36) #nivel confianza 95%, h = periodos
autoplot(prediccion12345)

#tipo de cambio
acf(tipodecambiosolo) #autocorrelacion
pacf(tipodecambiosolo) #orden de las medias moviles
modelosarima4=auto.arima(tipodecambiosolo.ts,stepwise = FALSE,approximation = FALSE)
summary(modelosarima4) ###propuesta 
residuals=resid(modelosarima4)
adf.test(residuals) #si es ruido blanco, mejorpronostico sugerido
coeftest(modelosarima4)
forecast(modelosarima4,h=10)
autoplot(acf(modelosarima4$residuals, plot = FALSE))
autoplot(pacf(modelosarima4$residuals, plot = FALSE))
ggseasonplot(tipodecambiosolo.ts,main="plot sarima ")
tsdiag(modelosarima4)
#Los p-valores para la prueba Q de Ljung-Box están por encima de 0,05, lo que indica "no significativo".
#Realizamos el contraste de hipótesis:
#H0: Los datos se distribuyen de forma independiente
#H1: Los datos no se distribuyen de forma independiente
independencia4 <- Box.test(modelosarima4$residuals, type="Ljung-Box") # Test de Ljung-Box
independencia4$p.value
#Efectivamente, los datos se distribuyen de forma independiente. P-valor = 0.53>0.05
qqnorm(modelosarima4$residuals)
qqline(modelosarima4$residuals) 
#Gráficamente observamos que los datos siguen una distribución normal aunque en las colas los datos se alejan y se dispersan un poco, para una mayor seguridad comprobamos la normalidad mediante el test Shapiro Wilk:
#H0:  Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente
normalidad4 <-shapiro.test(modelosarima4$residuals)    # Test de Shapiro-Wilk
normalidad4$p.value  
#p valor 0.70>0.05 no rechazamos nuestra hipótesis nula, los residuos siguen una distribución normal.
#Por lo tanto, damos el modelo por VÁLIDO
prediccion12345 <- forecast(modelosarima4, h=36) #nivel confianza 95%, h = periodos
autoplot(prediccion12345)
###########################DUDA PREGUNTAR A RANGEL##############

#DEFICIT
acf(deficitsolo) #autocorrelacion
pacf(deficitsolo) #orden de las medias moviles
modelosarima5=auto.arima(deficitsolo.ts,stepwise = FALSE,approximation = FALSE)
summary(modelosarima5) ###propuesta 
residuals=resid(modelosarima5)
adf.test(residuals) #si es ruido blanco, mejorpronostico sugerido
coeftest(modelosarima5)
forecast(modelosarima5,h=10)
autoplot(acf(modelosarima5$residuals, plot = FALSE))
autoplot(pacf(modelosarima5$residuals, plot = FALSE))
ggseasonplot(deficitsolo.ts,main="plot sarima ")
tsdiag(modelosarima5)
#Los p-valores para la prueba Q de Ljung-Box están por encima de 0,05, lo que indica "no significativo".
#Realizamos el contraste de hipótesis:
#H0: Los datos se distribuyen de forma independiente
#H1: Los datos no se distribuyen de forma independiente
independencia5 <- Box.test(modelosarima5$residuals, type="Ljung-Box") # Test de Ljung-Box
independencia5$p.value
#Efectivamente, los datos se distribuyen de forma independiente. P-valor = 0.53>0.05
qqnorm(modelosarima5$residuals)
qqline(modelosarima5$residuals) 
#Gráficamente observamos que los datos siguen una distribución normal aunque en las colas los datos se alejan y se dispersan un poco, para una mayor seguridad comprobamos la normalidad mediante el test Shapiro Wilk:
#H0:  Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente
normalidad5 <-shapiro.test(modelosarima5$residuals)    # Test de Shapiro-Wilk
normalidad5$p.value  
#p valor 0.70>0.05 no rechazamos nuestra hipótesis nula, los residuos siguen una distribución normal.
#Por lo tanto, damos el modelo por VÁLIDO
prediccion123456 <- forecast(modelosarima3, h=36) #nivel confianza 95%, h = periodos
autoplot(prediccion123456)

##masa monetaria
acf(masamonetariasola) #autocorrelacion
pacf(masamonetariasola) #orden de las medias moviles
modelosarima6=auto.arima(masamonetariasola.ts,stepwise = FALSE,approximation = FALSE)
summary(modelosarima6) ###propuesta 
residuals=resid(modelosarima6)
adf.test(residuals) #si es ruido blanco, mejorpronostico sugerido
coeftest(modelosarima6)
forecast(modelosarima6,h=10)
autoplot(acf(modelosarima6$residuals, plot = FALSE))
autoplot(pacf(modelosarima6$residuals, plot = FALSE))
ggseasonplot(masamonetariasola.ts,main="plot sarima ")
tsdiag(modelosarima6)
#Los p-valores para la prueba Q de Ljung-Box están por encima de 0,05, lo que indica "no significativo".
#Realizamos el contraste de hipótesis:
#H0: Los datos se distribuyen de forma independiente
#H1: Los datos no se distribuyen de forma independiente
independencia6 <- Box.test(modelosarima6$residuals, type="Ljung-Box") # Test de Ljung-Box
independencia6$p.value
#Efectivamente, los datos se distribuyen de forma independiente. P-valor = 0.53>0.05
qqnorm(modelosarima6$residuals)
qqline(modelosarima6$residuals) 
#Gráficamente observamos que los datos siguen una distribución normal aunque en las colas los datos se alejan y se dispersan un poco, para una mayor seguridad comprobamos la normalidad mediante el test Shapiro Wilk:
#H0:  Los datos se distribuyen normalmente
#H1: Los datos no se distribuyen normalmente
normalidad6 <-shapiro.test(modelosarima6$residuals)    # Test de Shapiro-Wilk
normalidad6$p.value  
#p valor 0.70>0.05 no rechazamos nuestra hipótesis nula, los residuos siguen una distribución normal.
#Por lo tanto, damos el modelo por VÁLIDO
prediccion1234567 <- forecast(modelosarima6, h=36) #nivel confianza 95%, h = periodos
autoplot(prediccion1234567)

##########Intento de razon de promedio movil######## suavizado
#estacionaria con respecto a la media
library(TTR)
####diferenciada?

?SMA

par(mfrow=c(1,2),ylab("media movil de inflacion a corto y largo plazo"))
mediamovilsimpleinfla=EMA(seriediferenciadainflacion,n=30)#media movil, n=periodo?
plot(mediamovilsimpleinfla)
mediamovilsimpleinfla2=EMA(seriediferenciadainflacion,100)
plot(mediamovilsimpleinfla2)

par(mfrow=c(1,2),ylab("media movil de salario a corto y largo plazo"))
mediamovilsimplesalario=EMA(salariosolo.ts,n=30)#media movil, n=periodo?
plot(mediamovilsimplesalario)
mediamovilsimplesalario2=EMA(salariosolo.ts,n=100)#media movil, n=periodo?
plot(mediamovilsimplesalario2)

par(mfrow=c(1,2),ylab("media movil de gasto a corto y largo plazo"))
mediamovilsimplegasto=EMA(gastosolo.ts,n=30)#media movil, n=periodo?
plot(mediamovilsimplegasto)
mediamovilsimplegasto2=EMA(gastosolo.ts,n=100)#media movil, n=periodo?
plot(mediamovilsimplegasto2)


par(mfrow=c(1,2),ylab("media movil de salario a corto y largo plazo"))
mediamovilsimpletipodecambio=EMA(tipodecambiosolo.ts,n=30)#media movil, n=periodo?
plot(mediamovilsimpletipodecambio)
mediamovilsimpletipodecambio2=EMA(tipodecambiosolo.ts,n=100)#media movil, n=periodo?
plot(mediamovilsimplesalario2)

par(mfrow=c(1,2),title("media movil de deficit a corto y largo plazo"))
mediamovilsimpledeficit=EMA(deficitsolo.ts,n=30)#media movil, n=periodo?
plot(mediamovilsimpledeficit)
mediamovilsimpledeficit2=EMA(deficitsolo.ts,n=100)#media movil, n=periodo?
plot(mediamovilsimpledeficit2)


par(mfrow=c(1,2),title("media movil de masa monetaria a corto y largo plazo"))
mediamovilsimplemasa=EMA(masamonetariasola.ts,n=30)#media movil, n=periodo?
plot(mediamovilsimplemasa)
mediamovilsimplemasa2=EMA(masamonetariasola.ts,n=100)#media movil, n=periodo?
plot(mediamovilsimplemasa2)

####Cuando  se  usa  el  método  de  promedios  móviles  se  está  suponiendo  que  todas
##las observaciones de la serie de tiempo son igualmente importantes para la estimación  
##del  parámetro  a  pronosticar  (en  este  caso  los  ingresos).  De  esta  manera, se 
##utiliza como pronóstico para el siguiente periodo el promedio de los n valores de los 
#atos más recientes de la serie de tiempo (López, 2016).

###varianza constante con respecto al tiempo homocedasticidad "falta"
bartlett.test(inflacionsola.ts,salariosolo.ts,gastosolo.ts,tipodecambiosolo.ts,deficitsolo.ts,masamonetariasola.ts,data=econometriatabla.ts)
#shi cuadrado, grados de libertad y p-value, 0.00000000000000022
bartlett.test() #da lo mismo
####con logaritmo en la transformacion se logra que la varianza sea más o menos constantes#########
###diferencia regular para eliminar la tendencia "DIFERENCIACION"
#####diferencia estacional para eliminar componente estacional




##############DUDAS: 
#1-QUE ES RESAGO
#2-ES NECESARIO VOLVER ESTACIONARIA CADA VARIABLE, INCLUYENDO LA DEPENDIENTE?
#3-CON PRUEBA DICKEY FULLER EL P VALOR ME INDICA ALGO Y CON NDIFFS EL CUAL ME INDICA CUANTAS
#4-CUANTAS DIFERENCIAS HAY QUE HACER ME INDICA OTRA VUELTA, SIN EMBARGO AL HACER LAS VECES QUE ME
#INDICA NDIFF ME VUELVE TODOS LOS PVALOR 0.01 INCLUSO MENOS
##SEASON PLOT"GRAFICO DE COLORES"NO ENCUENTRO DE QUE FORMA ME INDICA CUAL ES ESTACIONARIA Y CUAL NO
#CON LA TABLA NORMAL EN SERIES DE TIEMPO ME PUEDE DAR UNA IDEA DE ESTACIONARIEDAD SIN EMBARGO NO ES EFICIENTE
#EN ARIMA NO ME QUEDA CLARO DE LA DIFERENCIA DE AUTORELACIÓN=MEDIAS MOVILES Y AUTORRELACIÓN PARCIAL=AUTOREGRESIVOS
#LO UNICO QUE SE ES QUE DETERMINA EL (AUTOREGRESIVO,DIFERENCIAS Y MEDIAS MOVILES) DE LOS MODELOS
#ts diag, zoo, necesario?
#medias moviles simples va con variable diferenciada o en serie?



