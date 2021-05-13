cars <- cars

head(cars) #Mostras as 6 primeiras observações

scatter.smooth(x=cars$speed, 
               y=cars$dist, 
               main="Dist ~ Vel")  # Gráfico de Dispersão

par(mfrow=c(1, 2))  # Dividir a área de gráfico em 2
boxplot(cars$speed, main="Velocidade", sub=paste("Outliers: ", boxplot.stats(cars$speed)$out))  # box plot para a variável 'speed'
boxplot(cars$dist, main="Distancia", sub=paste("Outliers: ", boxplot.stats(cars$dist)$out))  # box plot para 'distância'


library(e1071)
par(mfrow=c(1, 2))  # Dividir a área de gráfico em 2
plot(density(cars$speed), 
     main="Gráfico de densidade: Velocidade", 
     ylab="Frequência", 
     sub=paste("Skewness:", 
               round(e1071::skewness(cars$speed), 2)))  # gráfico de densidade para 'speed'
polygon(density(cars$speed), col="blue")
plot(density(cars$dist), main="Gráfico de densindade: Distância", ylab="Frequência", sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")


cor(cars$speed, cars$dist)  # Calcular a correlação entre velocidade e distância


linearMod <- lm(dist ~ speed, data=cars)  # modelo de regressão linear
print(linearMod)


summary(linearMod) #resumo do modelo
