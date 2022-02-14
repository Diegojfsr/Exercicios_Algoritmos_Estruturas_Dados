# EXERCICIO 1 LISTA 1
# O tempo médio que um analgésico leva para “cair” na corrente sanguínea é de 
# 50 minutos. Admitindo um α = 1% e com base em uma amostra de tamanho 
# 10, cuja média foi de 48,20 minutos e desvio padrão de 7,81 minutos, verifique 
# se houve aceleração no processo após a inclusão de um novo componente na 
# fórmula.


#media populacional
mu0 = 50

#media amostral
xbar = 48.20

#desvio padrao
sd = 7.81

#numero de elementos na amostras
n = 10

#nivel de significancia
alpha = 0.01

print("a) Hipotese do teste")
#a) Hipotese do teste
# H0 = mu0 = 50 //Se tempo medio igual a 50
# Ha = mu0 < 50 //Se tempo medio menor que 50
# Teste unilateral a esquerda
vlt <- "Esquerda"

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