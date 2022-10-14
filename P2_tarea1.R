library(rpart)
library(rpart.plot)

setwd("C:/Users/Laura/Desktop/U/2022-2/APLICA")

tae.dat <- read.csv("tae.data.csv")
colnames(tae.dat) <- c("niv.ingles", "instruct", "curso", "semestre", "cant.est", "desempeño")
attach(tae.dat)

#hay 150 obs
cant.1 <- sum(with(tae.dat, desempeño == "1"))
cant.2 <- sum(with(tae.dat, desempeño == "2"))
cant.3 <- sum(with(tae.dat, desempeño == "3"))

tae.bin <- tae.dat
bin6 <- tae.bin[,6]

for (j in 1:150) {
  if (bin6[j] == "3") {
    tae.bin[j,6] = 0
  }
  if (bin6[j] == "2") {
    tae.bin[j,6] = 0
  }
}

tree = rpart(desempeño ~ niv.ingles + instruct + curso + semestre + cant.est, tae.bin, method="class", minbucket = 10)
rpart.plot(tree)
