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
econometriatabla.ts=ts(econometriatabla2,start=c(2000,1) , freq=12 )
inflacionsola.ts=ts(inflacionsola,start=c(2000,1) , freq=12 )
salariosolo.ts=ts(salariosolo,start=c(2000,1) , freq=12 )
gastosolo.ts=ts(gastosolo,start=c(2000,1) , freq=12 )
tipodecambiosolo.ts=ts(tipodecambiosolo,start=c(2000,1) , freq=12 )
deficitsolo.ts=ts(deficitsolo,start=c(2000,1) , freq=12 )
masamonetariasola.ts=ts(masamonetariasola,start=c(2000,1) , freq=12 )
#cuantas diferencias necesito
ndiffs(econometriatabla.ts)
ndiffs(inflacionsola.ts)
ndiffs(salariosolo.ts)
ndiffs(gastosolo.ts)
ndiffs(tipodecambiosolo.ts)
ndiffs(deficitsolo.ts)
ndiffs(masamonetariasola.ts)

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


plot(econometriatabla.ts,main="Determinantes de la inflación Enero 2000-Diciembre 2020")
plot(inflacionsola.ts,main="Inflacion Enero 2000-Diciembre 2020")
plot(salariosolo.ts,main="Salario nominal Enero 2000-Diciembre 2020")
plot(gastosolo.ts,main="Gasto publico Enero 2000-Diciembre 2020")
plot(tipodecambiosolo.ts,main="tipo de cambio Enero 2000-Diciembre 2020")
plot(deficitsolo.ts,main="Deficit o superavit fiscal Enero 2000-Diciembre 2020")
plot(masamonetariasola.ts,main="Masa monetaria Enero 2000-Diciembre 2020")

summary(econometriatabla2) #podria servir solo esta
summary(inflacion)
summary(salariosolo)
summary(gastosolo)
summary(tipodecambiosolo)
summary(deficitsolo)
summary(masamonetariasola)

#revisar si tiene estacionariedad "se requiere que sea así"
#seasonplot(econometriatabla.ts,col=rainbow(12),year.labels = TRUE) ##inquietud, en general no se puede porque estan medidas en diferentes unidades
seasonplot(inflacionsola.ts,col=rainbow(12),year.labels = TRUE)
seasonplot(salariosolo.ts,col=rainbow(12),year.labels = TRUE)
seasonplot(gastosolo.ts,col=rainbow(12),year.labels = TRUE)
seasonplot(tipodecambiosolo.ts,col=rainbow(12),year.labels = TRUE)
seasonplot(deficitsolo.ts,col=rainbow(12),year.labels = TRUE)
seasonplot(masamonetariasola.ts,col=rainbow(12),year.labels = TRUE)

#Autocorrelación

#para que una serie de tiempo sea estacionaria se requiere que la media
#y la varianza sean constantes a lo largo del tiempo

acf(econometriatabla.ts) #en esta relaciona todas, solo escogeriamos la primera fila, y la primera grafica de la segunda fila
acf(inflacionsola.ts)
acf(salariosolo.ts) #va disminuyendo lentamente, no estacionaria
acf(gastosolo.ts) #altibajos
acf(tipodecambiosolo.ts) #va disminuyendo, no estacionaria
acf(deficitsolo.ts) #altibajos
acf(masamonetariasola.ts) #va disminuyendo, no estacionaria

#convertir a estacionarias
#diferencias hasta convertirla

seriediferenciadaeconometriatabla=diff(econometriatabla.ts)
plot(seriediferenciadaeconometriatabla)
seriediferenciadainflacion=diff(inflacionsola.ts)
plot(seriediferenciadainflacion)
seriediferenciadasalario=diff(salariosolo.ts)
plot(seriediferenciadasalario)
seriediferenciadainflacion=diff(inflacionsola.ts)
plot(seriediferenciadainflacion)
#esto si es una serie de tiemp estacionaria, media constante
#no es perfecta porque tiene pico pero se puede utilizar
ndiffs(seriediferenciadainflacion) ##para saber cuantas diferencias necesito
ndiffs(seriediferenciadasalario)
