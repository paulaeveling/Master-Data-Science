library(readxl)
library(BSDA)
library(TeachingDemos)

#QUESTAO 01
#De uma amostra aleatória de 500 adultos residentes em uma vila, 385 foram favoráveis 
#ao aumento do limite de velocidade em uma autoestrada para 75 mph, enquanto em outra 
#amostra de 400 de adultos residentes em uma outra vila, 267 foram favoráveis a esse 
#aumento do limite de velocidade.
#
#O valor p que indica se existe ou não diferença sobre a o aumento da velocidade entre a
#s duas vilas é:
#RESPOSTA:
#
#Utilizando a função prop.test, ficaria da seguinte forma:
#
#favoraveis <- c(385,267)
#pop <- c(500, 400)
#prop.test(favoraveis, pop, correct = F)
#
#O resultado será:
#
#2-sample test for equality of proportions without continuity correction
#
#data: favoraveis out of pop
#X-squared = 11.696, df = 1, p-value = 0.0006265
#alternative hypothesis: two.sided
#95 percent confidence interval:
#0.04340589 0.16159411
#sample estimates:
#prop 1 prop 2 
#0.7700 0.6675
#
#Logo o valor p será de 0,001 aproximadamente.
favoraveis <- c(385,267)
pop <- c(500, 400)
prop.test(favoraveis, pop, correct = F)



#QUESTAO 02
#Sabe-se que a vida, em horas, de uma bateria é aproximadamente normalmente distribuída, 
#com desvio-padrão σ=1,25 hora. Uma amostra aleatória de 10 baterias 
#tem uma vida média de x¯=40,5 horas.
#
#O valor p que avalie que a vida da bateria exceda 40 horas é igual a:
#RESPOSTA
#
#Para essa questão, basta usar a seguinte função:
#
#Teste_Media_Conhecida_sumarizado <- function(xbarra, n, sigma, mi_zero, H_1 = c(diferente, menor, maior)){
#z <- (xbarra - mi_zero)/(sigma/sqrt(n))
#if (H_1 == "menor"){
#valor_p <- pnorm(z, 0, 1, lower.tail = T)
#}else if (H_1 == "maior"){
#valor_p <- pnorm(z, 0, 1, lower.tail = F)
#}else {valor_p <- 2*pnorm(z, 0, 1)}
#saida <- cbind(xbarra, mi_zero, z, valor_p)
#saida
#}
#
#Teste_Media_Conhecida_sumarizado(40.5, 10, 1.25, 40, "maior")
#
#O resultado será:
#
#xbarra      mi_zero   z                 valor_p
#40.5          40          1.264911     0.1029516
Teste_Media_Conhecida_sumarizado <- function(xbarra, n, sigma, mi_zero, H_1 = c(diferente, menor, maior)){
z <- (xbarra - mi_zero)/(sigma/sqrt(n))
if (H_1 == "menor"){
valor_p <- pnorm(z, 0, 1, lower.tail = T)
}else if (H_1 == "maior"){
valor_p <- pnorm(z, 0, 1, lower.tail = F)
}else {valor_p <- 2*pnorm(z, 0, 1)}
saida <- cbind(xbarra, mi_zero, z, valor_p)
saida
}

Teste_Media_Conhecida_sumarizado(40.5, 10, 1.25, 40, "maior")



#QUESTAO 03
#Um artigo na revista ASCE Jornaul of Energy Engineering (1999, Vol. 125, pp. 59-75) descreve 
#um estudo das propriedades da inércia térmica de concreto aerado na autoclave, usado como um 
#material de construção. Cinco amostras do material foram testadas em uma estrutura e a 
#temperatura (°C) média no interior foi: 23,01; 22,22; 22,04; 22,62 e 22,59.
#
#Supondo que as temperaturas são normalmente distribuídas, o valor p para o seguinte teste de 
#hipótese, H0 : μ = 22,5 H0 : μ ≠ 22,5, é igual a:
#RESPOSTA
#
#Nesse caso, não precisamos construir uma função para avaliar o valor p desse teste de hipótese. 
#Podemos usar a função t.test. Veja:
#
#temperaturas <- c(23.01, 22.22, 22.04, 22.62, 22.59)
#t.test(temperaturas, mu = 22.5)
#
#Note que meu teste é bilateral, avalia na hipótese alternativa a diferença da média no valor de 
#22,5 e assim os demais parâmetros da função ficarão como Default.
#
#Veja a resposta:
#
#One Sample t-test
#
#data: temperaturas
#t = -0.023642, df = 4, p-value = 0.9823
#alternative hypothesis: true mean is not equal to 22.5
#95 percent confidence interval:
#22.02625 22.96575
#sample estimates:
#mean of x 
#22.496
#
#Assim o valor p será de 0,9823, sendo a hipótese nula não rejeitada, garantindo que os dados 
#possuem uma média de 22,5 graus Celsius.
temperaturas <- c(23.01, 22.22, 22.04, 22.62, 22.59)
t.test(temperaturas, mu = 22.5)



#QUESTAO 04
#Um fabricante de lentes intraoculares está qualificando uma nova máquina de moagem. Ele 
#qualificará a máquina se a porcentagem de lentes que contenham defeitos na superfície não exceder 
#2%. Uma amostra aleatória de 250 lentes contém seis lentes defeituosas.
#
#O valor p para esse teste foi igual a:
#RESPOSTA
#
#Para essa questão, utilizaremos a função prop.test.
#
#prop.test(6, 250, p = .02, alternative = "greater", correct = F)
#
#O detalhe maior é não usar a correção de continuidade para realizar o teste de hipótese 
#(correct =F). Assim o resultado será:
#
#1-sample proportions test without continuity correction
#
#data: 6 out of 250, null probability 0.02
#X-squared = 0.20408, df = 1, p-value = 0.3257
#alternative hypothesis: true p is greater than 0.02
#95 percent confidence interval:
#0.01246022 1.00000000
#sample estimates:
#p 
#0.024
#
#O valor p foi de 0,3257.
prop.test(6, 250, p = .02, alternative = "greater", correct = F)



#QUESTAO 05
#Estão sendo estudadas as taxas de queima de dois diferentes propelentes sólidos usados no sistema 
#de escapamento de aeronaves. Sabe-se que ambos os propelentes têm aproximadamente o mesmo 
#desvio-padrão, ou seja, σ1 = σ2 e que essas taxas seguem distribuição normal. Duas amostras 
#aleatórias de n1 = 20 e n2 = 18 espécimes foram testadas resultando em taxas médias de 18 cm/s e 
#desvio de 2,8 cm/s para a primeira amostra e de média de 21 cm/s e desvio de 3,0 cm/s na segunda.
#
#O valor p para avaliar se a taxa média dos dois propelentes vem da mesma população ou não é igual a:
#RESPOSTA
#
#Nessa questão os dados estão sumarizados. Não podemos utilizar a função t.test para resolver esse 
#teste de hipóteses. Para isso usaremos o pacote BSDA que possui a função tsum.test da qual fará o teste 
#de hipótese para a média considerando duas amostras. Veja como fica o processo desde a instalação do 
#pacote.
#
#install.packages("BSDA")
#library(BSDA)
#tsum.test(18, 2.8, 20, 21, 3, 18, var.equal = T)
#
#O resultado será:
#
#Standard Two-Sample t-Test
#
#data: Summarized x and y
#t = -3.1883, df = 36, p-value = 0.002959
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#-4.908324 -1.091676
#sample estimates:
#mean of x mean of y 
#18                    21
#
#Logo o valor p é igual a 0,003
install.packages("BSDA")
library(BSDA)
tsum.test(18, 2.8, 20, 21, 3, 18, var.equal = T)
