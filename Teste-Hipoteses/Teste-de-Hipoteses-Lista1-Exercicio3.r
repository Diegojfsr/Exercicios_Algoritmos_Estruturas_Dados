# EXERCICIO 3 LISTA 1
# A cervejaria BebeBier vende cervejas em embalagens cujos rótulos indicam 
# um conteúdo médio de 600 ml. O Instituto Nacional de Pesos e Medidas 
# (INPM) seleciona aleatoriamente 50 garrafas de cerveja produzidas pela 
# companhia, mede seu conteúdo e obtém uma média amostral igual a 596,25 
# ml e desvio padrão de 14,06 ml. Com um nível de significância de 1%, teste a 
# hipótese de que a cervejaria esta enganando seus consumidores vendendo 
# produtos com menor conteúdo.



#media populacional
mu0 = 600

#media amostral
xbar = 596.25

#desvio padrao
sd = 14.06

#numero de elementos na amostras
n = 50

#nivel de significancia
alpha = 0.01


#a) Hipotese do teste
# H0 = mu0 = 600 //Se a media e igual a 600ml
# Ha = mu0 < 600 //Se a media e menor que 600ml
# Teste unilateral a esquerda

vlt <- "Esquerda"

paste("Teste", vlt)

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