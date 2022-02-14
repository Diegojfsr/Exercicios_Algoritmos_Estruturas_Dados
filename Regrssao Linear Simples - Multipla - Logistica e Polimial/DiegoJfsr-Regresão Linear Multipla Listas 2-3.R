#AULA 2 

y= c(1.5, 6.5, 10.0,11.0, 11.5, 16.5)
x1= c(0, 1, 1, 2, 2, 3)
x2= c(0, 2, 4, 2, 4, 6)

Y = matrix(c(1.5, 6.5, 10.0,11.0, 11.5, 16.5), nrow = 6, ncol= 1) 
X = matrix(c(1, 1, 1, 1, 1, 1, 0, 1, 1, 2, 2, 3, 0, 2, 4, 2, 4, 6),nrow = 6, ncol = 3 )

Xt_X <- t(X) %*% X

inverse_Xt_X <- solve(Xt_X)

Xt_Y <- t(X) %*% Y

b <- inverse_Xt_X %*% Xt_Y

#Multiplique Linear Regression in R

df = data.frame(x1, x2, y)

RLS <- lm(y ~ x1 + x2, data = df)


#Os valores dos vetores de parametros sao: 

#^b0 = 2 ^b1 = 3 ^b2= 1

#Equacao de Regressao Estimada 

#^y = 2 + 3x1 + 2x2 

##################################################################

#AULA 3

y  = c(1.5, 6.5, 10, 11, 11.5, 16.5)
x1 = c(0, 1, 1, 2, 2, 3)
x2 = c(0, 2, 4, 2, 4, 6)

Y = matrix(c(1.5, 6.5, 10, 11, 11.5, 16.5), nrow = 6, ncol = 1) 
X = matrix(c(1, 1, 1, 1, 1, 1, 0, 1, 1, 2, 2, 3, 0, 2, 4, 2, 4, 6),  nrow = 6, ncol = 3) 

Xt_X <- t(X) %*% X

inverse_Xt_X <- solve(Xt_X)

Xt_Y <- t(X) %*% Y 

b <- inverse_Xt_X %*% Xt_Y

df = data.frame(x1, x2, y)

RLS <- lm(y ~ x1 + x2, data = df)

print(RLS)


###############################################################################
#Letra A)
#ANOVA 

alpha <- 0.05

n = length(Y)

p <- 2  

GL_Regressao <- p  

GL_Total  <- n-1

GL_Residuo <- GL_Total - GL_Regressao

bt <- t(b)

Xt <- t(X)

C <- (sum(Y)^2)/n

Yt_Y = t(Y) %*% Y

Xt_Y <- Xt %*% Y

SQResiduo <-  Yt_Y - (bt %*% Xt_Y)

SQRegressao <- bt %*% Xt %*% Y - C

SQTotal <- Yt_Y - C
SQTotal <- SQResiduo + SQRegressao


QM_Regressao <- SQRegressao / GL_Regressao

QM_Residuo <- SQResiduo / GL_Residuo

F_cal <- QM_Regressao / QM_Residuo


F_tab = qf(1-alpha, df1=GL_Regressao, df2=GL_Residuo) 
print(F_tab)

#ANOVA
################################################################################
#  FV          GL          SQ         QM          F_cal    F_tab(GL_Reg, GL_Res)

#Regressao     2          125,5      62,75        62,75     F_tab(2,3) -> 9.55  

#Residuo       3            3          1        

#Total         5           128,5
################################################################################
#teste F para a Anova 

# H0 = b0 =  b1 = b2 = 0
# Ha = nao H0 (teste bilateral) -> pelo menos um b <> 0 

if (abs(F_cal) > abs(F_tab)) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}

##############################################################
#Letra B
#coeficiente de determinacao

R2 = SQRegressao / SQTotal

R2_Ajustado <- (R2*(n-1) - p) / (n - p - 1)


##############################################################
#Letra C
#test-t para ANOVA 

# H0 = b0 =  10
# Ha = b0 <  10  (teste unilateral a esquerda)

b0 <- 10
alpha <- 0.01 

b0_est <- b[1]

Var_est_b0 <- inverse_Xt_X[1,1]

t_cal <- (b0_est - b0) / (sqrt(Var_est_b0*QM_Residuo))
#t_cal <- summary(RLS)$coefficients[1,3]

t_tab = qt(alpha, df=GL_Residuo)   # df = degrees of freedom   -> unilateral test 
#to do bilateral test use alpha/2 

if (abs(t_cal) > abs(t_tab)) {
  print("Rejeita-se H0")
  
} else {
  print("Aceita-se H0")
}