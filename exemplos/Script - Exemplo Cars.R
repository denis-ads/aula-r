cars <- cars

head(cars) #Mostras as 6 primeiras observa��es

scatter.smooth(x=cars$speed, 
               y=cars$dist, 
               main="Dist ~ Vel")  # Gr�fico de Dispers�o

par(mfrow=c(1, 2))  # Dividir a �rea de gr�fico em 2
boxplot(cars$speed, main="Velocidade", sub=paste("Outliers: ", boxplot.stats(cars$speed)$out))  # box plot para a vari�vel 'speed'
boxplot(cars$dist, main="Distancia", sub=paste("Outliers: ", boxplot.stats(cars$dist)$out))  # box plot para 'dist�ncia'


library(e1071)
par(mfrow=c(1, 2))  # Dividir a �rea de gr�fico em 2
plot(density(cars$speed), 
     main="Gr�fico de densidade: Velocidade", 
     ylab="Frequ�ncia", 
     sub=paste("Skewness:", 
               round(e1071::skewness(cars$speed), 2)))  # gr�fico de densidade para 'speed'
polygon(density(cars$speed), col="blue")
plot(density(cars$dist), main="Gr�fico de densindade: Dist�ncia", ylab="Frequ�ncia", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")


cor(cars$speed, cars$dist)  # Calcular a correla��o entre velocidade e dist�ncia


linearMod <- lm(dist ~ speed, data=cars)  # modelo de regress�o linear
print(linearMod)


summary(linearMod) #resumo do modelo
