#Exemplo Modelo Binomial
#Probabilidade de X > 8
pbinom(8, 10, .2, lower.tail = F)
#Probabilidade de X < 3
pbinom(2, 10, .2)
#Probabilidade de X ser maior e igual a 2 e menor e igual a 5
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
# 5 , apenas 1 corretarespostas possiveis
# qual a probabilidade de ter 3 ou mais respostas corretas?

# calcular Bernoulli primeiro - probabilidade de sucesso por tentativa unica
dbinom(3, size = 12, prob = 0.2)
pbinom(3, size = 12, prob = 0.2)

# diferenca entre p e dbinom
# dbinom: probabilidade de observar valor exatamente igual a x
# pbinom: probabilidade de observar valor menor ou igual a x

# PROBLEMA 2
# 10 adutoras avaliadas
# probabilidade de ter problema 0.2
# probabilidade de que pelo menos 1 funcione
1 - dbinom(0, size = 10, prob = 0.2)

# ---------------------------------------------------------------


#Exemplo Modelo Poisson
#Probabilidade de X = 0
dpois(0, .02)
#Probabilidade de X < 3
ppois(2, .02)
#Probabilidade de X ser maior e igual a 2 e menor e igual a 5
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

