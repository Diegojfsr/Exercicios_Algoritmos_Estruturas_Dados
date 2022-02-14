x= c(2, 3, 4, 5, 6)
y= c(3, 5, 6, 8, 9)

df <- data.frame(y, x)


# Exercicio 1
# A - Ajuste a equacao de regressao linear simples.

RLS <- lm(y~x, data =df)
print(RLS)
summary(RLS)

# b1 = 1,5
# b0 = 0,2
# Equacao Ajustada
# y = 0,2 + 1,5x

# B - Utilize a ANOVA para verificar se o tempo de estudo tem 
# influencia significativa obre as notas utilizando alpha = 5%.

alpha <- 0.05

ANOVA <- aov(y ~x, data = df)
summary(ANOVA)

p_value <- summary(ANOVA) [[1]] [["Pr(>F)"]] [1]
print(p_value)

if (p_value < alpha) {
    print("rejeita-se H0")
}

# C -  Utilize o teste t para verificar se o tempo de estudo tem 
# influencia significativa sobre as notas utilizando alpha = 5%.

n = length(x)
b1 <- summary(RLS)$coefficients [2]
print(b1)

Mean_Sq_Residuals <- summary(ANOVA) [[1]] [["Mean Sq"]] [2]
print(Mean_Sq_Residuals)

t_cal <- b1/Mean_Sq_Residuals
print(t_cal)

t_tab = qt(alpha/2, df=n-2)

if (abs(t_cal) > abs(t_tab)){
    print("Rejeita-se H0")
} else {
    print("Aceita-se H0")
}


# Exercicio 2
# Modelo 1
alpha <- 0.05
GL_Reg <- 1
GL_Total <- 4

GL_Res <- sum(GL_Total - GL_Reg)
print(GL_Res)

SQ_Reg <- 50
SQ_Res <- 15

SQ_Total <- sum(SQ_Reg + SQ_Res)
print(SQ_Total)

QM_Reg <- (SQ_Reg/GL_Reg)
print(QM_Reg)

QM_Res <- (SQ_Res/GL_Res)
print(QM_Res)

F_Cal <- QM_Reg / QM_Res
print(F_Cal)

F_tab = qf(1-alpha, df1=GL_Reg, df2=GL_Res)
print(F_tab)

R2 <- (SQ_Reg/SQ_Total)
print(R2)

# Exercicio 2
# Modelo 2

alpha <- 0.05
GL_Reg <- 1
GL_Total <- 4

GL_Res <- sum(GL_Total - GL_Reg)
print(GL_Res)

SQ_Reg <- 76
SQ_Res <- 18

SQ_Total <- sum(SQ_Reg + SQ_Res)
print(SQ_Total)

QM_Reg <- (SQ_Reg/GL_Reg)
print(QM_Reg)

QM_Res <- (SQ_Res/GL_Res)
print(QM_Res)

F_Cal <- QM_Reg / QM_Res
print(F_Cal)

F_tab = qf(1-alpha, df1=GL_Reg, df2=GL_Res)
print(F_tab)

R2 <- (SQ_Reg/SQ_Total)
print(R2)