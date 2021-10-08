library(readxl)
datos <- read_excel("C:/Users/Usuario/Downloads/economic.xlsx")
View(datos)
attach(datos)
names(datos)
inflacion<-datos[,-1]
colnames(datos)=c("Año","Inflacion","Salario nominal","PIB municipal 2018")#cambiar nombre de las variables porque son muy largos
INFLACION2=data.frame(INFLACION,`Gastos)