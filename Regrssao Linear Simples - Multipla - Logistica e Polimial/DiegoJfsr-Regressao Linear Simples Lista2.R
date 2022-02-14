library(ggplot2)

y= c(5.4, 5.6, 5.3, 4.7, 3.9, 4.0, 3.7)
x= c(18, 19, 20, 23, 24, 25, 26)

df <- data.frame(y, x)

# Exercicio 1
# A
#Faca o grafico de dispersco

ggplot(df, aes(x=df$x, y=df$y))+
  geom_point() +
  stat_smooth (method = lm)

# B 
#Ajuste a equacao de regressao linear simples

RLS <- lm(y ~ x, data = df)
print(RLS)


#b1 = -0,24
#b0 = 10,05

# Equacao ajustada 
# y = 10,05 - 0,24x

# C 
#Calcule e interprete o coeficiente de correlacao

print(cor(x, y))

#A varivel X e Y possuem uma correlacao linear negativa
#logo com o aumento da idade, a uma tendencia que as notas diminuem

# D e E
#Calcule e interprete o coeficiente de determinacao
#Calcule o coeficiente de correlacao corrigido

summary(RLS)

#R2= 0,927 ou 92,7%
#92,7% e o percentual da variabilidade observada nas notas sendo
#sendo explicado pela regrssao linear simples nos valores da idade 
# ou 92,7% da variacao em y (notas) e esplicada pela RLS nos valores
# de x (idade)
