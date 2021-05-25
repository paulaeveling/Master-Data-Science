# PROBLEMA 1
# 20 pessoas avaliadas
# probabilidade de alcancar a nota 0.10

# A = probabilidade de no maximo 3 pessoas atenderem requisito
pbinom(3, size = 20, prob = 0.1)

# B = 50% ser aprovado
dbinom(10, 20, 0.1)

# --------------------------------------------------------------

# PROBLEMA 2
# 15 pacientes
# 80% pode ser curada

# A = todos serem criados
dbinom(15, 15, 0.8)

# B = 13 serem curados
dbinom(13,15,0.8)

# C = ao menos 10 serem curados
# aqui eu coloquei 10, mas deve ser 9
pbinom(9,15,0.8, lower = FALSE)

# --------------------------------------------------------------

# PROBLEMA 3
# lambda = 1 por m2

# A = pelo menos 1 defeito
# cometi o mesmo erro que o anterior - coloquei 1 mas deveria ser 0
ppois(0, lambda = 1, lower.tail = FALSE)

# B = no maximo 2 defeitos
ppois(2, lambda = 1)

# C = entre 2 e 4 defeitos
# mesmo erro de antes
ppois(4, 1) - ppois(1, 1)

# D = nao mais de 1 defeito
ppois(1, lambda = 1)

# --------------------------------------------------------------

# PROBLEMA 4
# lambda = 5

# A = 0 ocorrencias em 1 minuto
dpois(0, lambda = 5)

# B = pelo menos 1 ocorrencia
# aqui fiz errao, nao entendi
1 - dpois(0, 5)

# C = numero de particulas emitidas entre 2 e 5
# errei de novo
ppois(5,5) - ppois(2,5)

# --------------------------------------------------------------

# PROBLEMA 5
# 2 anos
# Nao entendi o exercicio, tentar de novo
# soube que deveria usar a formula pexp

# --------------------------------------------------------------

# PROBLEMA 6
# probabilidade exponencial = 3

# A = inferior a 1 minuto
pexp(1,3)

# B = superior a 2 minutos
pexp(2,3, lower = FALSE)

# --------------------------------------------------------------

# PROBLEMA 7
# media = 130kg
# desvio padrao = 20kg
# 0.25 magros | 0.25 obesos

# A = magros
qnorm(0.25, mean = 130, sd = 20)

# B = obesos
qnorm(0.25, mean = 130, sd = 20, lower = FALSE)

# --------------------------------------------------------------

# PROBLEMA 8
# media = 90 minutos
# devio padrao = 20 minutos
# errei esse exercicio - ficou faltando a segunda linha de A e era pra usar qnorm em B

# A = 65 candidatos | minimo 80 minutos
pnorm(80, mean = 90, sd = 20)
65*pnorm(80, mean = 90, sd = 20)

# B = 5% melhores
qnorm(0.05, mean = 90, sd = 20)

# EXERCICIO 9
# media desconhecida
# variancia = 0.01
# amostra de 10 animais
# media 1.69
# obter intervalo de 95% de confianca

# zsum.test(media, raiz da variancia, amostra, nivel de confianca)

library(BSDA)
zsum.test(1.69, sqrt(0.01), 10, conf.level = 0.95)

# EXERCICIO 10
# amostra 500 conexoes
# valor medio 25 minutos
# desvio padrao = 4.25 minutos
# intervalo de confianca 92%

# tsum.test(media, desvio padrao, amostra, nivel de confianca)

library(BSDA)
tsum.test(25, 4.25, 500, conf.level = 0.92)


# EXERCICIO 11
# amostra 200 pacientes
# 160 curados (resultado do teste)
# intervalo 95%
prop.test(160, 200, correct = F)

# EXERCICIO 12
# intervalo 95%
# IC = [35. 21; 35.99]
# amostra = 100 observacoes
# media u desconhecida
# desvio padrao 2

x_barra = 35.21 + qnorm((1-.95)/2, lower.tail = F)*2/sqrt(100)
x_barra

# EXERCICIO 13
# media = 15km/l
# amostra = 25
# media = 14.3km/l
# variancia = 9km/l
# significancia = 6%
# alternativa inferior

# zsum.test(media, raiz da variancia, amostra, nivel de confianca)

zsum.test(14.3, sqrt(9), 25, mu = 15, alternative = "less")

# EXERCICIO 14
# amostra 10
# media 5.5
# desvio padrao = 2
# significancia = ??
# media na populacao <= 6

# zsum.test(media, desvio padrao, amostra, nivel de confianca)
# mu = valor da media

tsum.test(5.5, 2, 10, mu = 6, alternative = "less")

# --------------------------------------------------------------

# TESTE 1
# Poisson
# media = 30 a cada 30 minutos | 5 a cada 5 minutos
# menos de 3 em 5 minutos
ppois(2,5)

# TESTE 2
# media 145
# desvio padrao 6,53
# 0.15 lower false
qnorm(0.015, mean = 145, sd = 6.53, lower = FALSE)

# TESTE 3
# exponencial
# media 1/400
# probabilidade de falha em menos de 100 horas
pexp(100, 1/400)

# TESTE 4
# media = 1500
# desvio padrao = 70
# A = no maximo 5% FALSE
qnorm(0.05, mean = 1500, sd = 70)

# B = mesma calculo que questao anterior, mas TRUE

# C = durar entre 1500 e 1570 TRUE
pnorm(1570, mean = 1500, sd = 70) - pnorm(1500, mean = 1500, sd = 70)

# TESTE 5
# probabilidade 0.2
# 10 adutoras observadas
# pelo menos 1 funcionando
1 - dbinom(0, 10, 0.2)

# --------------------------------------------------------------

# PROVAS ANTERIORES

# QUESTAO 1: Durabilidade de um tipo de pneu da marca Rodabem
# media 60.000 km
# desvio padrao 8.300km
# 2% substituidos
qnorm(0.02, mean = 60000, sd = 8300)

# QUESTAO 2: Um sensor tem vida media de 
# media 2.000 dias
# desvio padrao 80 dias
# distribuicao normal
# ERRADA
qnorm(.05, 2000, 80)
# CORRETA
qnorm(.95, 2000, 80)
# CORRETA
pnorm(2200, 2000, 80) - pnorm(2100, 2000, 80)

# QUESTAO 3: Uma pesquisa envolvendo 1,000 pacientes
# amostra = 1,000
# 823 morreram em 10 anos
# intervalo de confianca = 0.799344 e 0.846656

# Resposta: 95%


# QUESTAO 4: Em uma determinada empresa trabalham quatro analistas de mercado.
salarios <- c(1600, 1600, 1600, 1600, 1800, 1800, 2750, 5000)
salarios.dif <- diff(salarios)
mean(salarios)