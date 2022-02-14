# EXERCICIO 4 LISTA 1
# Um processo deveria produzir bancadas com, em média, 0.85 m de altura. O 
# engenheiro desconfia que as bancadas que estão sendo produzidas são 
# diferentes que o especificado. Uma amostra de oito valores foi coletada e 
# indicou média de 0.87 m. Sabendo que o desvio-padrão é 0.01, teste a hipótese 
# o engenheiro usando um nível de significância de 5%.


#media populacional
mu0 = 0.85

#media amostral
xbar = 0.87

#desvio padrao
sd = 0.01

#numero de elementos na amostras
n = 8

#nivel de significancia
alpha = 0.05


#a) Hipotese do teste
# H0 = mu0 = 0.85 //Se a media de altura das bancadas e igual a 0.85m
# Ha = mu0 != 0.85 //Se a media de altura das bancadas e diferente de 0.85m
# Teste bilateral

vlt <- "Bilateral"

paste("Teste", vlt)

paste("Tempo medio:", mu0, "Media amostral:", xbar, "Desvio padrão:", sd, "Tamanho amostra:", n, "Nivel significancia:", alpha)

#b) Estatistica do teste

# Tcalculado = (media amostral - medai populacional) / (desvio padrao/raizquadrada do numero da amostra) )
tcal = (xbar-mu0)/(sd/sqrt(n))
print("O T calculado é:")
print(tcal)


# c) Regiao Critica
#Ttabelado = tabelaT(nivel de significancia, grau de liberdade = tamanho da amostra - 1)
ttab = qt(alpha/2, df=n-2)
print("O T tabelado é:")
print(ttab)


#Conclusão
#Se (valor absoluto do(Tcalculado) é superior ao valor absoluto do(Ttabelado))
#entao print(rejeita-se H0) Senao print (Aceita-se H0)

if (abs(tcal)>abs(ttab)) { print("rejeita-se H0")} else{ print("Aceita-se H0")}