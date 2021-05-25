
#------------------- UNIDADE I -------------------#

library(readxl)
attach(Nota_de_Alunos_Parte_1)
View(Nota_de_Alunos_Parte_1)


# Tabela de Frequência para Gênero

# descobrir relação de gênero
freq_genero <- table(Nota_de_Alunos_Parte_1$Genero)
freq_genero

# função proporção | calculo da medida relativa
prop_genero <- prop.table(freq_genero)
prop_genero
# arrendondar pra 2 casas decimais
perc_genero <- round(prop_genero*100,digits = 2)
perc_genero
# colocar info em 2 colunas separadas, frequencia e percentual
coluna_freq <- c(freq_genero,sum(freq_genero))
coluna_freq
# soma percentual total - tem ser igual a 100%
coluna_perc <- c(perc_genero,sum(perc_genero))
# nova linha de total
names(coluna_freq)[length(coluna_freq)] <- "Total"
# constroi tabela de frequencia - cbind juntar duas colunas
tabela_freq <- cbind(coluna_freq,coluna_perc)
tabela_freq


# Tabela de Frequência para Conceito Final

# resumo da informacao
freq_conceito <- table(Nota_de_Alunos_Parte_1$Conceito)
freq_conceito

# calcula proporcao
prop_conceito <- prop.table(freq_conceito)
perc_conceito <- round(prop_conceito*100,digits = 2)

# combinacao das colunas
coluna_freq <- c(freq_conceito,sum(freq_conceito))
coluna_perc <- c(perc_conceito,sum(perc_conceito))
names(coluna_freq)[length(coluna_freq)] <- "Total"
tabela_freq <- cbind(coluna_freq,coluna_perc)
tabela_freq


# Tabela de Frequ?ncia para Nota Final

# dados qualitativos
# criar faixas de valores: funcao cut
# right = false; valores a direta nao entram na faixa

intervalos <- cut(Nota_de_Alunos_Parte_1$Nota_Final,breaks=0:10,right = F)
freq_notas <- table(intervalos)
freq_notas

# calcular percentual
prop_notas <- prop.table(freq_notas)
perc_notas <- round(prop_notas*100, digits = 2)
coluna_freq <- c(freq_notas,sum(freq_notas))
coluna_freq
coluna_perc <- c(perc_notas,sum(perc_notas))
names(coluna_freq)[length(coluna_freq)] <- "Total"
tabela_freq <- cbind(coluna_freq,coluna_perc)
tabela_freq


# Gráfico de Pizza

rotulos <- paste(perc_genero,"%",sep="")
pie(freq_genero,main="Gráfico de Pizza: Gênero dos Alunos",
    labels = rotulos,col = rainbow(7))
# pch simbolo quadrado
legend(1,1,names(freq_genero),col = rainbow(7),pch = 11)

# Gráfico de Barras ou Colunas
barplot(freq_conceito)

# horiz = T exibe dados na horizontal / grafico de barras regular
barplot(freq_conceito,horiz = T)

freq_cruzada <- table(Nota_de_Alunos_Parte_1$Genero,Nota_de_Alunos_Parte_1$Conceito)
freq_cruzada

# se nao colocar beside = T, R vai empilhar colunas
barplot(freq_cruzada, beside = T, main = "Conceito vs Gênero",
        ylab = "Número de Aluno",col = c("darkblue","red"))
legend(1,30,rownames(freq_cruzada),col = c("darkblue","red"),
       pch = 15)


# Histograma para Nota Final
hist(Nota_de_Alunos_Parte_1$Nota_Final, breaks=0:10, right = F, col = "green",
     xlab = "Notas", ylab = "Frequência", main = "Distribuição de Notas" )


# Gráfico de Séries
plot(Nota_de_Alunos_Parte_1$Prova_1, type = 'l',xlab = "ID Aluno", ylab = "Nota")
lines(Nota_de_Alunos_Parte_1$Prova_2,col = "blue")
lines(Nota_de_Alunos_Parte_1$Prova_3,col = "red")


# Gráfico de Caixa
boxplot(Nota_de_Alunos_Parte_1$Nota_Final ~ Nota_de_Alunos_Parte_1$Disciplina,
        main = "Nota Final por Disciplina",
        xlab = "Disciplina", col = c("orange","green"))




# Medidas Descritivas
# forma de sumarizar dados quantitativos
# medidas de tendencia central, medidas de dispersão e medidas de separatrizes

# medidas de tendencia central - media aritmetica
mean(Nota_de_Alunos_Parte_1$Prova_1)

# medidas de tendencia central - mediana
# não é afetado por dados extremos
median(Nota_de_Alunos_Parte_1$Prova_1)

# medidas de tendencia central - moda
# valor mais frequente, neste exemplo, a nota 10 repetiu 27 vezes
tabela_freq <- table(Nota_de_Alunos_Parte_1$Prova_1)
subset(tabela_freq,
       tabela_freq == max(tabela_freq))

# medidas de tendencia central - média ponderada
wt <- c(5,  5,  4,  1)
x <- c(3.7,3.3,3.5,2.8)
xm <- weighted.mean(x, wt)
xm

m <- mean(x)
m



# medidas de separatriz - amplitude
#---Percentil 35---#
quantile(Nota_de_Alunos_Parte_1$Prova_1,.35)

#---Decil 20---#
quantile(Nota_de_Alunos_Parte_1$Prova_1,.20)

#---Quartil 3---#
quantile(Nota_de_Alunos_Parte_1$Prova_1,.75)

# medidas de dispersão - amplitude
diff(range(Nota_de_Alunos_Parte_1$Prova_1))


# medidas de dispersão - variância e desvio-
# distancia dos dados em relaçao a media artimetica
# variancia amostral
var(Nota_de_Alunos_Parte_1$Prova_1)
# desvio padrao amostral
sd(Nota_de_Alunos_Parte_1$Prova_1)

# variancia populacional
n <- length(Nota_de_Alunos_Parte_1$Prova_1)
((n-1)/n)*var(Nota_de_Alunos_Parte_1$Prova_1)
# desvio padrao populacional (afastamento padrao com relacao a media)
# quanto menor a diferença, mais consistente sao os dados
sqrt(((n-1)/n)*var(Nota_de_Alunos_Parte_1$Prova_1))


# coeficiente de varição - medida relativa de dispersao
x_barra <- mean(Nota_de_Alunos_Parte_1$Prova_1)
s <- sd(Nota_de_Alunos_Parte_1$Prova_1)
CV <- s*100/x_barra
# variacao media dos dados eh de 35.6% da media (algumas notas estao se afastando mais em relacao a media)
# espera-se que coeficiente seja menor que 25% (bem consistente) 25-50% (consistencia media)
CV


# Sumarizacao 

# sumarizacao de uma coluna especifica da tabela
summary(Nota_de_Alunos_Parte_1$Prova_1)
# sumarizacao de todos os dados da tabela
summary(Nota_de_Alunos_Parte_1)


# Questionario 1
# questao 1
freq_cruzada_2 <- table(Nota_Alunos$Conceito,Nota_Alunos$Genero)
freq_cruzada_2
prop_cruzada_2 <- round(100*prop.table(freq_cruzada_2,2),2)
prop_cruzada_2
barplot(prop_cruzada_2, ylab = "% de Alunos", col = rainbow(8), ylim = c(0,120))
legend(1, 120, rownames(prop_cruzada_2), col = rainbow(8), pch = 15, horiz = T)
title("Conceito vs Gênero")

# questao 2
Rotulos_Desc <- c("Média", "Mediana", "Amplitude", "Desvio-Padrão", "CV")
xbarra <- mean(Nota_Alunos$Nota_Final)
xtil <- median(Nota_Alunos$Nota_Final)
Amp <- diff(range(Nota_Alunos$Nota_Final))
Desvio <- sd(Nota_Alunos$Nota_Final)
CV <- 100*Desvio/xbarra
Descritivas <- c(xbarra, xtil, Amp, Desvio, CV)
Descritivas <- round(cbind(Descritivas),digits = 2)
Valores_Desc <- as.data.frame(Descritivas, row.names = Rotulos_Desc,
col.names = names(Descritivas))
Valores_Desc



#------------------- UNIDADE 2 -------------------#


# Modelos de Probabilidade Discreta

# xemplo Modelo Binomial
# sempre calcula probabilidade igual a x, se for > ou <, revisar o valor de x
# Probabilidade de sucesso > 8
pbinom(8, size = 10, prob = 0.2, lower.tail = F)
# Probabilidade de X < 3
pbinom(2, 10, .2)
# Probabilidade de X ser maior e igual a 2 e menor e igual a 5
pbinom(5, 10, .2)-pbinom(1, 10, .2)

# DATA SCIENCE ACADEMY
# Help com detalhes da funcao
?dbinom

# dbinom(x, size, prob)
# pbinom(x, size, prob)
# qbinom(p, size, prob)
# rbinom(n, size, prob)

# x e um vetor de numeros
# p e um vetor de probabilidades
# n e o numero de observacoes
# size e o numero de tentativas
# prob probabilidade de sucesso de cada tentativa

# PROBLEMA 1
# 12 perguntas de multipla escolha
# 5 opcoes, apenas 1 resposta possivel == prob = 20%
# qual a probabilidade de ter 3 ou mais respostas corretas?

# calcular Bernoulli primeiro - probabilidade de sucesso por tentativa unica
dbinom(3, size = 12, prob = 0.2)

# diferenca entre p e dbinom
# dbinom: probabilidade de observar valor exatamente igual a x
# pbinom: probabilidade de observar valor menor ou igual a x

# PROBLEMA 2
# 10 adutoras avaliadas
# probabilidade de ter problema 0.2
# probabilidade de que pelo menos 1 funcione
1 - dbinom(0, size = 10, prob = 0.2)

# ---------------------------------------------------------------


# Exemplo Modelo Poisson
# Probabilidade de X = 0
dpois(0, .02)
# Probabilidade de X < 3
ppois(2, .02)
# Probabilidade de X ser maior e igual a 2 e menor e igual a 5
ppois(5, .02)-ppois(1, .02)


# DATA SCIENCE ACADEMY
# Help com detalhes da funcao
?dpois
?ppois

# ocorrencia de eventos indepentendes em um intervalo

# PROBLEMA
# 12 carros cruzando uma ponte por minuto
# probabilidade de ter 17 ou mais carros em um minuto especifico

# tudo que estiver na parte superior, por isso lower = false
ppois(16, lambda = 12, lower = FALSE)


# EXERCICIOS
# lambda = 2 petroleiros por dia | media para 2 dias = 4
# X = ate 3 petroleiros em dois dias
ppois(3, lambda = 4)
# valor complementar
ppois(3, lambda = 4, lower = FALSE)

# lambda = 3 passageiros por segundo
# X = menor ou igual 2
ppois(2, lambda = 3)

# lambda = 3 falhas por mes | media para 15 dias = 1.5
# X = 2 falhas em 15 dias
dpois(2, lambda = 1.5)

# lambda = 30 a cada 30 minutos | 5 por 5 minutos
# X = menos de 3 eleitores em 5 minutos
ppois(2, lambda = 5)


# Modelos de Probabilidade Contínua
# Exemplo de Modelo Exponencial - nao importa igualdade

# Probabilidade de X > 1500
# tempo de vida superior a 1,500 horas
pexp(1500, 1/2000, lower.tail = F)

# Probabilidade de X < 2000
pexp(2000, 1/2000)

# Probabilidade de X ser maior e igual a 2000 e menor e igual a 3000
pexp(3000, 1/2000)-pexp(2000, 1/2000)

# media: 1/400
# q = 100
pexp(100, 1/400)


#DS ACADEMY
# utilizada para descrever dados quando valores mais baixos tender a dominar a distribuicao.
# Valores mais altos nao ocorrem com frequencia

#-----------------------------------------------------------------------

# Exemplo de Modelo Normal
# Probabilidade de X > 8
pnorm(8, 10, 2, lower.tail = F)
# Probabilidade de X < 3
pnorm(3, 10, 2)
# Probabilidade de X ser maior e igual a 2 e menor e igual a 5
pnorm(5, 10, 2)-pnorm(2, 10, 2)

pnorm(11, 10, 2)


# DS ACADEMY
# Modelo de distribuicao normal
# representa uma dentre as muitas probabilidade que uma variavel pode possuir
# distribuicao normal media e desvio padrao

# formato de sino e simetrico
# media e mediana possuem mesmo valor
# parte mais alta da curva representa maior probabilidade do evento ocorrer
# propriedades: nunca e <= 0 e >= 100

# o cientista de dados deve garantir que os dados estao normalizados
# media do teste = 72
# desvio padrao = 15.2
# qual a porcentagem de alunos com mais de 84 pontos
# ou seja, estamos interessados na cauda superior da distribuicao

?pnorm

# pnorm(q, mean=0, sd=1)
pnorm(84, mean = 72, sd = 15.2, lower.tail = FALSE)

#-----------------------------------------------------------------------

# EXERCICIOS

# media = 145
# desvio padrao = 6,53
# q = 1.5%

pnorm(1.5, 145, 6.53, lower.tail = FALSE)


# Questionario 2
# lambda = 30 a cada 30 minutos | 5 por 5 minutos
# X = menos de 3 eleitores em 5 minutos
ppois(2, lambda = 5)

# vida media = 1,500 dias
# desvio padrao = 70 dias
# distribuicao normal
# pnorm(q, mean=0, sd=1)
qnorm(.05, 1500, 70)
# durar entre 1500 e 1570 dias
pnorm(1570, 1500, 70) - pnorm(1500, 1500, 70)

# distribuicao exp
# media de 400/hora
# probabilidade de falha 100
pexp(100, 1/400)

# media 145 pes
# desvio padrao = 6.53
# distribuicao normal
# 1.5%
qnorm(0.015, 145, 6.53, lower.tail = F)

# probabilidade = 0.2
# 10 adutoras observadas
# probabilidade de 1 ou mais funcionarem perfeitamente
1 - dbinom(0, size = 10, prob = 0.2)



#------------------- UNIDADE 3 -------------------#

# Estimação Pontual e Intervalar: Estimação para a Média
# conjunto de técnicas que tem como objetivo estudar uma população através de evidências fornecida por uma amostra representativa
# pontual: forma que mais se aproxima de um parametro
# intervalar: media aritmetica

# Comandos sobre Intervalos de Confiança
# Intervalo de Confiança para a Média - Variância Conhecida
# x = dados, sigma = desvio padrao populacional, confianca = em proporcao
# n = tamanho do vetor x
# xbarra = media aritmetica
# estimar intervado inferior e superior
IC_Media_Conhecida <- function(x, sigma, confianca){
  n <- length(x)
  x_barra <- mean(x)
  IC_inf = x_barra - qnorm((1-confianca)/2,0,1, lower = FALSE)*(sigma/sqrt(n))
  IC_sup = x_barra + qnorm((1-confianca)/2,0,1, lower = FALSE)*(sigma/sqrt(n))
  saida <- cbind(IC_inf,IC_sup)
  saida
}

IC_Media_Conhecida (60139.7, 16, 0.95)


# Intervalo de Confian?a para a M?dia - Vari?ncia Desconhecida
# x = dados, confianca = em proporcao
# n = tamanho do vetor x
# xbarra = media aritmetica
# estimar intervado inferior e superior
IC_Media_Desconhecida <- function(x, confianca){
  n <- length(x)
  x_barra <- mean(x)
  s <- sd(x)
  IC_inf = x_barra - qt((1-confianca)/2,n-1, lower = FALSE)*(s/sqrt(n))
  IC_sup = x_barra + qt((1-confianca)/2,n-1, lower = FALSE)*(s/sqrt(n))
  saida <- cbind(IC_inf,IC_sup)
  saida
}

IC_Media_Desconhecida (60139.7, 0.95)


# Estimação para a Proporção
# quantificar quantidade de indices favoraveis / numero de ocorrencias
# exemplo: jogar um dados 10 vezes; x = numero de vezes que 6 ocorreu
IC_Proporcao <- function(x, confianca, evento){
  n <- length(x)
  x <- x[x == evento]
  prop <- length(x)/n
  IC_inf = prop - qnorm((1-confianca)/2,0,1)*(sqrt((prop*(1-prop))/n))
  IC_sup = prop + qnorm((1-confianca)/2,0,1)*(sqrt((prop*(1-prop))/n))
  saida <- cbind(IC_inf,IC_sup)
  saida
}
IC_Proporcao(Nota_de_Alunos_Parte_1$Conceito, .95, "B")
# resultado: probabilidade de um aluno tirar B = 15% ate 58%


# Questionario 3
# 5 vigas delaminadas
# distribuicao normal
# intervalo de confianca = 0.99
# frequencia media = ?
# 230,33       233,05      232,58      229,48      232,58
IC_Media_Desconhecida <- function(x, confianca){
  n <- length(x)
  x_barra <- mean(x)
  s <- sd(x)
  IC_inf = x_barra - qt((1-confianca)/2,n-1, lower = FALSE)*(s/sqrt(n))
  IC_sup = x_barra + qt((1-confianca)/2,n-1, lower = FALSE)*(s/sqrt(n))
  saida <- cbind(IC_inf,IC_sup)
  saida
}
dados_q3 <- c(230.33, 233.05, 232.58, 229.48, 232.58)
IC_Media_Desconhecida (dados_q3, 0.99)

# 13 defeitos (x)
# 300 circuitos investigados (n)
# intervalo de 90% (confianca)
IC_Proporcao <- function(x, n, confianca){
  prop <- x/n
  IC_inf <- prop - qnorm((1-confianca)/2, lower.tail = F)*(sqrt((prop*(1-prop))/n))
  IC_sup <- prop + qnorm((1-confianca)/2, lower.tail = F)*(sqrt((prop*(1-prop))/n))
  saida <- cbind(IC_inf, IC_sup)
  saida
}
IC_Proporcao(13, 300, 0.9)

# 1000 pacientes
# 823 faleceram de cancer de pulmao 
# 10 anos
# confianca 0,799344 a 0,846656.
Encontre_confiaca_prop <- function(x, n, IC_inf, IC_sup){
  prop <- x/n
  erro <- IC_inf - prop
  alfa <- 2*pnorm(erro/(sqrt(prop*(1-prop)/n)))
  confianca <- round(100*(1 - alfa), digits = 1)
  saida <- cbind(prop, IC_inf, IC_sup, confianca)
  saida
}

Encontre_confiaca_prop(823, 1000, 0.799344, 0.846656)

# 16 pneus fabricados
# media = 60.139
# desvio padrao = 3.645,94 km
# confianca = 0.95
teste_media_var_desconhecida <- function(xbarra, desvio, n, confianca){
  IC_inf <- xbarra - qt((1-confianca)/2, n-1, lower.tail = F)*(desvio/sqrt(n))
  IC_sup <- xbarra + qt((1-confianca)/2, n-1, lower.tail = F)*(desvio/sqrt(n))
  saida <- cbind(IC_inf, IC_sup)
  saida
}

teste_media_var_desconhecida(60139.7, 3645.94, 16, .95)


# sigma = 25 horas
# amostra = 20
# vida media = 1014
# intervalo confianca = 0.95
teste_media_var_conhecida <- function(xbarra, n, sigma, confianca){
  IC_inf <- xbarra - qnorm((1-confianca)/2, lower.tail = F)*(sigma/sqrt(n))
  IC_sup <- xbarra + qnorm((1-confianca)/2, lower.tail = F)*(sigma/sqrt(n))
  saida <- cbind(IC_inf, IC_sup)
  saida
}

teste_media_var_conhecida(1014, 20, 25, .95)



#------------------- UNIDADE 4 -------------------#

# Comandos para Teste de Hip?teses para a M?dia
# Teste de Hip?teses para a M?dia com Vari?ncia Conhecida

# hipoteses podem ser nula, menor ou maior

# teste de hipotese: media das notas maior igual a 8
# x = 
# sigma = 3 (desvio padrao conhecido)
# mi_zero = hipotese nula -- media igual ou superior a 8
# H_1 = opcoes - igual, menor ou maior
# valor_p: probabilidade nivel de significancia

Teste_Media_Conhecida <- function(x, sigma, mi_zero, H_1 = c(igual, menor, maior)){
  # n = tamanho da amostra
  n <- length(x)
  # media aritmetica de x
  media <- mean(x)
  # z = estatistica de teste
  z <- (media-mi_zero)/(sigma/sqrt(n))
  # analise da cauda inferior
  if (H_1 == "menor"){
    # avaliar se media populacional (mi) é inferior a 0
    valor_p <- pnorm(z, 0, 1, lower.tail = T)
  }else if (H_1 == "maior"){
    # avaliar se media populacional (mi) é superior a 0
    valor_p <- pnorm(z, 0, 1, lower.tail = F)
  }else {valor_p <- 2*pnorm(z, 0, 1)}
  saida <- cbind(media, mi_zero, z, valor_p)
  saida
}
Teste_Media_Conhecida(Nota_de_Alunos_Parte_1$Prova_1,3,8,"maior")
# resultado: media aritmetica e menor do que 8
# z da valor negativo 
# probabilidade nula nao foi rejeitada


#Teste de Hip?teses para a M?dia com Vari?ncia Desconhecida
Teste_Media_Desconhecida <- function(x, mi_zero, H_1 = c(igual, menor, maior)){
  n <- length(x)
  media <- mean(x)
  # formula é a mesma, mas desvio padrao amostral deve ser calculado
  desvio <- sd(x)
  # calcular estatistica de teste
  t <- (media-mi_zero)/(desvio/sqrt(n))
  if (H_1 == "menor"){
    valor_p <- pt(t, n-1, lower.tail = T)
  }else if (H_1 == "maior"){
    valor_p <- pt(t, n-1, lower.tail = F)
  }else {valor_p <- 2*pt(t, n-1)}
  saida <- cbind(media, desvio, mi_zero, t, valor_p)
  saida
}
Teste_Media_Desconhecida(Nota_de_Alunos_Parte_1$Prova_1,8,"igual")
# analise:
# nivel de significancia de 5% de erro
# probabilidade maior que 5%
# nao rejeito hipotese nula

#Teste de Hip?teses para a Propor??o
# proportcao de alunos com conceito A = 10%
# x = informações do conceito da nota do aluno
# p_zero = valor da hipotese nula - neste caso, 10%
# evento = evento de interesse - neste caso, conceito A
Teste_Proporcao <- function(x, p_zero, evento, H_1 = c(igual, menor, maior)){
  # tamanho da amostra
  n <- length(x)
  # valores que sao iguais ao evento (conceito A)
  x <- x[x == evento]
  # probabilidade de conceitos A / tamanho da amostra
  prop <- length(x)/n
  # estatistica de teste
  z <- (prop-p_zero)/(sqrt((p_zero*(1-p_zero))/n))
  if (H_1 == "menor"){
    valor_p <- pnorm(z, 0, 1, lower.tail = T)
  }else if (H_1 == "maior"){
    valor_p <- pnorm(z, 0, 1, lower.tail = F)
  }else {valor_p <- 2*pnorm(z, 0, 1)}
  saida <- cbind(prop, p_zero, z, valor_p)
  saida
}
Teste_Proporcao(Nota_de_Alunos_Parte_1$Conceito, 0.10, "A", "igual")
# analise: 
# se proporcao de alunos com conceito A = 10%
# 4,87% dos alunos tiveram conceito A (coluna prop)
# estatistica de teste muito alto (coluna z)
# valor p: 3.33% -- abaixo do valor minimo de significante de 5%
# hipotese é nula - proporcao nao é igual a 10%

# novo teste para analisar se hipotese é nula
Teste_Proporcao(Nota_de_Alunos_Parte_1$Conceito, 0.10, "A", "menor")

# teste de hipotese
t.test(Nota_de_Alunos_Parte_1$Prova_1, mu = 8)

# teste de proporcao
# fornecer numero de sucesso (conceito A), experimento (alunos), e hipotese alternativa
# proporcao de alunos com conceito A eh menor do que 8?
prop.test(8, 161, 0.10, alternative = "less", correct = F)

# Teste para duas amostras independentes
# nivel de confianca = 5%
# valor de P indica que a media das duas provas é totalmente diferente - valor bem abaixo de 5%
# intervalo de confianca indica que medias sao diferentes, se fossem iguais, o valor 0 seria informado
# hipotese nula - nao existe igualdade entre media de prova 1 e 2
t.test(Nota_de_Alunos_Parte_1$Prova_1, Nota_de_Alunos_Parte_1$Prova_2, var.equal = F, conf.level = 0.95)

# hipotese alternativa - avaliar se nota da prova 1 é menor que 2
# valor p = media prova 1 é menor que prova 2, por isso valor de P = 1
t.test(Nota_de_Alunos_Parte_1$Prova_1, Nota_de_Alunos_Parte_1$Prova_2, alternative = "less", var.equal = F, conf.level = 0.95)

# Questionario 4
# vila 1
# amostra 500 adultos
# 385 favoraveis 

# vila 2
# amostra 400 adultos
# 267 favoraveis
favoraveis <- c(385, 267)
populacao <- c(500, 400)
prop.test(favoraveis, populacao, correct = F)


# amostras = 23,01; 22,22; 22,04; 22,62 e 22,59.
# media = 22.5
amostra <- c(23.01, 22.22, 22.04, 22.62, 22.59)
t.test(amostra, mu=22.5)


# 2% nao ocorrer defeitos
# amostra = 250
prop.test(6, 250, p = .02, alternative = "greater", correct = F)


# amostra a =  20
# media = 18cm
# devio a = 2.8cm

# amostra b = 18
# media = 21cm
# desvio b = 3cm

# dados estão sumarizados
# 
install.packages("BSDA")
library(BSDA)
tsum.test(18, 2.8, 20, 21, 3, 18, var.equal = T)


# desvio padrao = 1.25
# amostra = 10
# media = 40,5
# valor p que avalie que a vida da bateria exceda 40 horas
t.test(10, mu = 40.5, )

# Exercícios Complementares

# pontuação varia de 0 a 700 pontos
# score minimo: 600 pontos
# probabilidade de tirar 600 ou mais: 0,10
# amostra: 20 alunos
# A: probabilidade de no maximo 3 atenderem requisito
pbinom(3, size = 20, prob = 0.10)
# B: probabilidade de metade deles terem sido aprovados
dbinom(10, size = 20, prob = 0.10)


# probabilidade de cura: 80%
# amostra: 15 pacientes
# probabilidade binomial
# A: todos serem curados
dbinom(15, size = 15, prob = 0.80)
# B: pelo menos 2 nao serem curados
pbinom(13, size = 15, prob = 0.80)
# C: ao menos 10 ficarem livres da doença
pbinom(9, 15, .8, lower.tail = F)


# parametro = 1 por m2
# A: 1 chapa apresentar defeito
ppois(0, 1, lower.tail = F)
# B: no maximo 2 defeitos
ppois(2, 1)
# C: entre 2 e 4 defeitos
ppois(4, 1) - ppois(1,1)
# D: nao mais de 1 defeito ser encontrado
ppois(1, 1)


# parametro: 5
# n: 1
# A: probabilidade de que nao haja emissao
dpois(0, 5)
# B: pelo menos 1 particula seja emitida em 1 minuto
1 - dpois(0, 5)
# C: entre 2 e 5 em 1 minuto
ppois(5,5) - ppois(3,5)


# parametro: 2 anos
# probabilidade: 1 ano
1 - pexp(1, 2, lower.tail = F)


# probabilidade exponencial 3
# A: tempo inferior a 1 minuto
pexp(1, 3, lower.tail = T)
# B: tempo ser superior a 2 minutos
pexp(2, 3, lower.tail = F)


# distribuicao media: 130kg
# desvio padrao: 20kg
# magros: 25%
# 25% obesos
# valores que delimitam cada classificacao
# obeso:
qnorm(0.25, 130, 20)
# magro:
qnorm(0.75, 130, 20)


# media: 90 minutos
# desvio padrao: 20 minutos
# A: amostra: 65 candidatos - 80 minutos
# qnorm quando tem a porcentagem
# pnorm quando nao tem porcentagem
65*pnorm(80, mean = 90, sd = 20)
# B: 5% melhores, quao rapido devem ser
qnorm(0.05, mean = 90, sd = 20, lower.tail = T)


# variancia: 0.01 m2
# amostra: 10 animais
# media: 1.69m
# intervalo: 95%
library(BSDA)
zsum.test(1.69, sqrt(0.01), 10)


# amostra: 500 conexões
# media: 25 minutos
# desvio padrao: 4.25 minutos - como é desvio padrao, nao precisa calcular sqrt
# faixa de 92% de confiança
library(BSDA)
zsum.test(25, 4.25, 500, conf.level = 0.92)


# amostra: 200 pacientes
# 160 foram curados
# intervalo de confianca: 95%
prop.test(160, 200, correct = F)


# intervalo de confianca: 95%
# amostra: 100 observacoes
# desvio padrao = 2
x_barra = 35.21 + qnorm((1-.95)/2, lower.tail = F)*2/sqrt(100)
x_barra
# intervalo de confianca: 90%
zsum.test(35.60, 2, 100, conf.level = 0.9)


# amostra: 10 observacoes
# media: 5.5
# desvio padrao: 2
# 5% de significancia se media e menor ou igual a 6
tsum.test(5.5, 2, 10, mu = 6, alternative = "less")


# distribuicao normal
# 5mm
# desvio padrao: 0.2
# probabilidade de ser maior que 5.5mm
# pnorm(q, mean=0, sd=1)
pnorm(5.5, mean = 5, sd = 0.2, lower.tail = FALSE)
?pnorm


# media: 180 meses
# desvio padrao: 15 meses
# superior a 150 meses
pnorm(150, mean = 180, sd = 15)


# amostra: 50 ovos
# 8 apresentaram anomalia
# proporcao inferior a 26%?
prop.test(8, 50)


# amostra: 10
# calcular media e desvio padrao
mean(c(21,25,23,20,18,21,18,19,22,24))
sd(c(21,25,23,20,18,21,18,19,22,24))


# media: 2,000 dias
# desvio padrao: 80 dias
# distribuicao normal

# A: maximo de dias para repor no maximo 5%
qnorm(0.95, mean = 2000, sd = 80)
# B: maximo de dias para repor no maximo 95%
qnorm(0.95, mean = 2000, sd = 80)
# C: probabilidade durar entre 2100 e 2200
pnorm(2200, mean = 2000, sd = 80) - pnorm(2100, 2000, sd = 80)


# media salarios
salarios <- c(1600, 1600, 1600, 1600, 1800, 1800, 2750, 5000)
salarios.dif <- diff(salarios)
mean(salarios)


# amostra: 16 pneus
# media: 60,139.7 km
# desvio padrao: 3,645.94km
# vida media excede 60,000
pnorm(60000, mean = 60139.7, sd = 3645.94, lower.tail = F)







