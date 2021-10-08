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
inflacion.ts=ts(econometriatabla2,start=c(2000) , freq=12 )
inflacion.ts
plot(inflacion.ts)
summary(econometriatabla2)