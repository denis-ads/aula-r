
#isso é um comentario
1 + 1 #isso é a soma de 1 + 1


# Ajuda -------------------------------------------------------------------

#ajuda no R
help(median)
?median

# Definindo Diret?rio -----------------------------------------------------

getwd() #retornar o meu diretorio
setwd("~/ambienteDesenv/workspace_fia/aula-r")


# leitura de arquivos ----
base_dados = read.csv("bases/Boston.csv", sep= ";")

summary(base_dados)


# instalar pacotes ----
install.packages("dplyr")
#install.packages("ggplot2")

# import lib
library(dplyr)


# Primeiros codigos -------------------------------------------------------

#install.packages("dplyr")
library(dplyr)


# Programação -------------------------------------------------------------

#A != a Case sensitivity

#operações
1 + 1
2 - 1
2 * 2
2 / 2
2 ^ 2

2 / 0 #inf
0 / 0 #NaN

#operadores Logicos
1 < 2 #menor
1 > 2 #maior
1 <= 2 #menor igual
1 >= 2 #maior igual
1 == 2 #igualdade
1 != 2 #diferente
!TRUE #Negativa
TRUE&TRUE #E
TRUE|FALSE #OU
xor(x,y) 

#definir vari?veis
minha_variavel <- 1
minha_variavel2 = 2

#remover vari?vel
#rm(minha.variavel)

minha_variavel * minha_variavel2

minha_variavel = 5

minha_variavel * minha_variavel2


# Estrutura de Dados ------------------------------------------------------

#vetores
vetor = 2:5
vetor2 = c(5,4,8,7,9,6,3,2,8,5,10,78)
vetor3 = seq(1,5, by=0.5) #gera uma sequencia de 1 a 5 quebrando em 0.5
vetr4 = runif(6,0,60) #gerar 6 aleatorios que podem ser fracionados

set.seed(42)#fixar o aleat?rio
vetr5 = sample(1:60,6, replace = F) #amostra aleatoria de 1 a 60

vetor * 2

vetor * vetor2

sort(vetor2)
table(vetor2)
rev(vetor2)
unique(vetor2)



#subsetting
vetor2[4]
vetor2[-4]
vetor2[length(vetor2)]
vetor2[2:6]
vetor2[c(2,4,6,8)] # c é uma função de concatenacao
vetor2[-c(2,4,6,8)]
vetor2 = c(vetor2,10) #adicionar um valor novo

vetor2[vetor2 == 10] #retornar os valores iguais a 10
vetor2[vetor2 < 10] #retorna menor que 10

vetor2[vetor2 > 5] = 0 
vetor2


# matriz ------------------------------------------------------------------

matriz = matrix(1:12,nrow = 3,ncol = 4)#criar uma matriz
matriz

matriz2 = matrix(1:16,nrow = 4, ncol = 4,byrow = T) #criar matriz por linha
matriz2

matriz[,2]#subseting de coluna
matriz[2,]#subseting de linha
matriz[2,2]#Posiçao

t(matriz)


# Data Frame --------------------------------------------------------------

teste.cartao = read.table("bases/base_gastos_cartao.csv", 
                          sep= ",", 
                          header = T, 
                          dec = ",") #carregar csv usando paramentros de controle
cartao = read.csv("bases/base_gastos_cartao.csv") #carregar csv

head(cartao) #6 primeiras linhas
tail(cartao) #6 ultimas linhas 

#subsetting de posicao
cartao[,2]
cartao[2,]

#subsetting por nome de variavel
cartao$Impostos[2]

nrow(cartao)
ncol(cartao)

cartao$Gastos_Cartao[cartao$Gastos_Cartao >= 500] #selecior coluna maior que 500
cartao$Gastos_Cartao[cartao$Gastos_Cartao >= 500] = 0 #maior que 500 vira 0


cartao$Renda[cartao$Renda > 1500]
cartao[cartao$Segmento == 'C',]

cartao_500 =cartao[cartao$Gastos_Cartao >= 500,]

summary(cartao)#metricas descritivas

cartao$Impostos[cartao$Gastos_Cartao>=500]
cartao[cartao$Gastos_Cartao>=500,]
cartao[cartao$Gastos_Cartao>=500,1]

#analise exploratoria
par(mfrow = c(1,3))
boxplot(cartao$Gastos_Cartao)
boxplot(cartao$Idade)
plot(cartao$Idade,cartao$Gastos_Cartao)
plot(cartao$Idade,cartao$Gastos_Cartao)
cor(cartao$Idade,cartao$Gastos_Cartao)

#matriz de correlacao sem coluna de segmento
cor(cartao[,1:4])

cartao$nova_coluna = 1:150 #atribuir nova coluna


cartao = cartao[,-cartao$Gastos_Cartao] #Excluir determinada coluna
unique(cartao$Segmento)

# modelo = lm("Gastos_Cartao ~ Idade + Renda + Impostos", cartao)
# summary(modelo)
# plot(modelo)



# Lista -------------------------------------------------------------------

lista = list(numeros = c(1,5,7),frutas = c("banana","ma??"))#criar lista

lista[[1]][2]#selecionar posicao 2 da lista numeros
lista$numeros[,2] #selecionar posicao 2 da lista numeros

sum(lista[[1]]) #soma
sum(lista$numeros) #soma


# #factor -----------------------------------------------------------------

cartao$Segmento = as.factor(cartao$Segmento)

summary(cartao)

#funcoes ----
  calculadora <- function(horas, pph=40){
    round(horas* pph)
  }

calculadora(176, 900)


desconto <- function(horas, pph, percent_desc){
  liquido <- horas * pph
  if(horas > 100){
    liquido = liquido * percent_desc
  }
  round(liquido)
}

desconto(176,150, 0.8)
desconto(176,150, 0.9)


# REMOVER OBJETO DA MEMORIA ----
rm(desconto)


# FUNCOES VETORIZADAS ----
x <- 1:1000000 # sequência de inteiros de 1 a 1.000.000
# função para calcular a raiz quadrada de cada elemento de um vetor de numerico
minha_raiz <- function(numeros) {
  resp <- numeric(length(numeros))
  for(i in seq_along(numeros)) {
    resp[i] <- sqrt(numeros[i])
  }
  return(resp)
}


system.time(loop <- minha_raiz(x))
# MELHOR PERFORMANCE UTILIZAR VETORES NA FUNCAO DIRETAMENTE.
system.time(vetor <- sqrt(x))



# ESTRUTURA DE DADOS ----

#VETOR
vetor1 = 10:20
sum(vetor1)
mean(vetor1)

which(vetor1 == 15)# posicao do vetor
vetor[3]

#LISTA

#ARRAY
array = array()



# EXERCICIOS  ----


#1 realizar o summary do dataset completo
#2 realizar o summary do dataset cartao por segmento
#3 realizar um boxplot para as vari?veis Idade, Renda, Impostos, por categoria
#4 realizar um grafico de dispers?o utilizando as vari?veis Impostos por gasto no cart?o para categoria C
#5 Renomear as categorias para Classe A, Classe B e Classe C
