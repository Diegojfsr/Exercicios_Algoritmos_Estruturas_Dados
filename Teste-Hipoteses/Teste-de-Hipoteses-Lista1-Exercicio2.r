# EXERCICIO 2 LISTA 1
# Determinada firma desejava comprar cabos tendo recebido do fabricante a 
# informação de que a tensão média é de 8000 kgf. Efetuou-se um ensaio em 6 
# cabos e obteve a tensão média de ruptura 8250 kgf, com um desvio padrão de 
# 145 kgf. Verifique a 5% de probabilidade se a tensão média é maior do que a 
# anunciada.


#media populacional
mu0 = 8000

#media amostral
xbar = 8250

#desvio padrao
sd = 145

#numero de elementos na amostras
n = 6

#nivel de significancia
alpha = 0.05


#a) Hipotese do teste
# H0 = mu0 = 50 //Se tempo medio igual a 50
# Ha = mu0 > 50 //Se tempo medio menor que 50
# Teste unilateral a direita

vlt <- "Direita"

paste("Teste Unilateral a", vlt)

paste("Tempo medio:", mu0, "Media amostral:", xbar, "Desvio padrão:", sd, "Tamanho amostra:", n, "Nivel significancia:", alpha)

#b) Estatistica do teste

# Tcalculado = (media amostral - medai populacional) / (desvio padrao/raizquadrada do numero da amostra) )
tcal = (xbar-mu0)/(sd/sqrt(n))
print("O T calculado é:")
print(tcal)


# c) Regiao Critica
#Ttabelado = tabelaT(nivel de significancia, grau de liberdade = tamanho da amostra - 1)
ttab = qt(alpha, df=n-1)
print("O T tabelado é:")
print(ttab)


#Conclusão
#Se (valor absoluto do(Tcalculado) é superior ao valor absoluto do(Ttabelado))
#entao print(rejeita-se H0) Senao print (Aceita-se H0)

if (abs(tcal)>abs(ttab)) { print("rejeita-se H0")} else{ print("Aceita-se H0")}