y= c(5.4, 5.6, 5.3, 4.7, 3.9, 4.0, 3.7)
x= c(18, 19, 20, 23, 24, 25, 26)

# A - Informe os valores das somas a seguir:
#sumatorio de X 
sum(x)

#Somatorio de XY
sum(x, y)

#somatorio de X^2
sum(x^2)
 
#mediana de X
median(x)

#Somatorio de Y
sum(y)

#Somatorio de Y^2
sum(y^2)

#mediana de Y
median(y)

# B -  Ajuste a equacao de regressco linear simples:
dados <- data.frame(y, x)
print(dados)
RLS <- lm(y ~ x, data = dados)
print(RLS)

#b1 = -0,24
#b0 = 10,05

# Equacao ajustada 
# y = 10,05 - 0,24x

# C - Interprete b1:
#b1 = -0,24
#Para cada aumento de 1 ano na idade estima-se uma dininuicao 
#media de 0,24 nas notas 

# D - Interprete b0:
#b0 = 10,05
#Nao ha interpretacao uma vez que a variavel X nao passa 
#pela origem 

#  E -  Qual o valor estimado das notas quando a idade 
# e de 21 anos?

b1 <- -0.24
b0 <- 10.05

Y_est <- b0 +b1*21
print(Y_est)

# F - Qual o valor estimado e o desvio da regressao 
#quando a idade for 26 anos?

Y_est2 <- b0 +b1*26
print(Y_est2)

y_x26 <- 3.7

e_desv <- y_x26 - Y_est2
print(e_desv)

# G - Qual o valor estimado das notas quando a 
#idade e de 15 anos? Justifique sua resposta

Y_est3 <- b0 +b1*15
print(Y_est3)
 
# Trata-se de uma extrapolacao pois estamos utilizando a equacao 
#ajustada para prever valores fora do intervalo coberto pela amostra 
