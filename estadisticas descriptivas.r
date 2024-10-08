install.packages("modeest")
library(modeest)

data(iris)
names(iris)

#Media
mean(iris$Sepal.Length)
mean(iris$Sepal.Width)
mean(iris$Petal.Length)
mean(iris$Petal.Width)

#Mediana
median(iris$Sepal.Length)
median(iris$Sepal.Width)
median(iris$Petal.Length)
median(iris$Petal.Width)

#Moda
mfv(iris$Sepal.Length)
mfv(iris$Sepal.Width)
mfv(iris$Petal.Length)
mfv(iris$Petal.Width)

#Desviación Estandar
sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Width)

#Varianza
var(iris$Sepal.Length)
var(iris$Sepal.Width)
var(iris$Petal.Length)
var(iris$Petal.Width)

#Rango
range(iris$Sepal.Length)
range(iris$Sepal.Width)
range(iris$Petal.Length)
range(iris$Petal.Width)

max(iris$Sepal.Length)-min(iris$Sepal.Length)
max(iris$Sepal.Width)-min(iris$Sepal.Width)
max(iris$Petal.Length)-min(iris$Petal.Length)
max(iris$Petal.Width)-min(iris$Petal.Width)

#Percentiles
quantile(iris$Sepal.Length, seq(0,1 , 0.1))
quantile(iris$Sepal.Length, seq(0,1 , 0.25))

#Medidas Asociación:
#  -Covarianza

cov(iris$Sepal.Length, iris$Petal.Width)
cov(iris$Sepal.Width, iris$Petal.Length)

#  -Coeficiente de correlación Pearson

# Si el coef = 1    .: Relación Directa si una aumenta, aumenta la otra
# Si el coef = 0    .: Independientes
# Si el coef = -1   .: Relación inversa Si una aumenta, la otra disminuye

cor(iris$Petal.Length, iris$Petal.Width)
cor(iris$Sepal.Width, iris$Petal.Length)

summary(iris)
View(iris)

#######--------- Gráficas

#----- Gráfica 1 Pastel
pie(table(iris$Species), main="Cantidad de flores por especie")

#----- Gráfica 2 Barras
barplot(table(iris$Species), 
        xlab="Especie",
        ylab = "Frecuencia",
        ylim = c(0,60), 
        main = "Cantidad por especie")

#----- Gráfica 3 Histograma
hist(iris$Sepal.Length, 
     main = "Histograma de largo del Sépalo",
     xlab = "Largo del Sépalo",
     ylab = "Frecuencia")

#----  Gráfica Diagrama de caja
boxplot(iris$Sepal.Length ~ iris$Species,
        main = "Largo del sépalo por especie", 
        xlab = "Variedad de flor",
        ylab = "Largo del sépalo")

#----  Gráfica Diagrama de caja
boxplot(iris$Petal.Length ~ iris$Species,
        main = "Largo del Petalo por especie", 
        xlab = "Variedad de flor",
        ylab = "Largo del Petalo")

#----  Gráfica de dispersión y pares
plot(iris$Petal.Length, iris$Petal.Width,
     col = iris$Species,
     main = "Largo del Pétalo vs Ancho del Petalo",
     xlab = "Largo del Pétalo",
     ylab = "Ancho del Pétalo")

pairs(iris[, 1:4], col = iris$Species,
      labels = names(iris)[1:4])

#Coordenadas Paralelas
library(MASS)

parcoord(iris[1:4], col = iris$Species, var.label = TRUE)

isSetosa <- ifelse(iris$Species == "setosa", "red", "gray")
parcoord(iris[1:4], col = isSetosa)






