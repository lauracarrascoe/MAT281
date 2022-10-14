library(ggplot2)
library(naivebayes)
library(class)
require(carData)
library(car)


setwd("C:/Users/Laura/Desktop/U/2022-2/APLICA")

iris.dat <- read.csv("iris.data.csv")
colnames(iris.dat) <- c("longitud.s", "ancho.s", "longitud.p", "ancho.p", "iris.type")
#a)
cant.setosa <- sum(with(iris.dat, iris.type == "Iris-setosa"))
cant.versi <- sum(with(iris.dat, iris.type == "Iris-versicolor"))
cant.virg <- sum(with(iris.dat, iris.type == "Iris-virginica"))
cant.tot <- sum(cant.setosa,cant.versi,cant.virg)
#149 obs en total
#49 iris-setosa, 50 iris-versicolor y 50 iris-virginica.
attach(iris.dat)
as.vector(iris.type)

##SCATTERPLOTS
#long.s vs ancho.s
plot(ancho.s , longitud.s , main = "Longitud Sépalo vs Ancho Sépalo", xlab= "Ancho Sépalo", ylab = "Longitud Sépalo" , col = as.factor(iris.type), pch=16)
legend("topright", legend = levels(as.factor(iris.type)), pch=16, col = as.factor(levels(as.factor(iris.type))), cex = 0.5)

#long.s vs long.p
plot(longitud.p , longitud.s , main = "Longitud Sépalo vs Longitud Pétalo", xlab= "Longitud Pétalo", ylab =  "Longitud Sépalo", col = as.factor(iris.type), pch=16)
legend("topright", legend = levels(as.factor(iris.type)), pch=16, col = as.factor(levels(as.factor(iris.type))), cex = 0.5)

#long.s vs ancho.p
plot(ancho.p , longitud.s , main = "Longitud Sépalo vs Ancho Pétalo", xlab= "Ancho Pétalo", ylab =  "Longitud Sépalo", col = as.factor(iris.type), pch=16)
legend("topright", legend = levels(as.factor(iris.type)), pch=16, col = as.factor(levels(as.factor(iris.type))), cex = 0.5)

##BAYES INGENUO
model <- naive_bayes(iris.type ~ longitud.s+longitud.p+ancho.s+ancho.p, data = iris.dat, usekernel=T)
plot(model)
cor(iris.dat[,c(1,2,3,4)])

#predicción bayes
predict(model, newdata = data.frame(longitud.s = 3, ancho.s = 2, longitud.p = 3, ancho.p = 2), type = "prob")

##KNN
knn(train = cbind(longitud.s, ancho.s, longitud.p, ancho.p), test = cbind(3,2,3,2),
    cl = iris.type, k = 10, prob = T)
knn(train = cbind(longitud.s, ancho.s, longitud.p, ancho.p), test = cbind(3,2,3,2),
    cl = iris.type, k = 15, prob = T)
knn(train = cbind(longitud.s, ancho.s, longitud.p, ancho.p), test = cbind(3,2,3,2),
    cl = iris.type, k = 25, prob = T)
