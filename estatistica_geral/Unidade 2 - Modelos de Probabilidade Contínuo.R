#Exemplo de Modelo Exponencial

#Probabilidade de X > 1500
# tempo de vida superior a 1,500 horas
pexp(1500, 1/2000, lower.tail = F)

#Probabilidade de X < 2000
pexp(2000, 1/2000)

#Probabilidade de X ser maior e igual a 2000 e menor e igual a 3000
pexp(3000, 1/2000)-pexp(2000, 1/2000)

# media: 1/400
# q = 100
pexp(100, 1/400)


#DS ACADEMY
# utilizada para descrever dados quando valores mais baixos tender a dominar a distribuicao.
# Valores mais altos nao ocorrem com frequencia

#-----------------------------------------------------------------------

#Exemplo de Modelo Normal
#Probabilidade de X > 8
pnorm(8, 10, 2, lower.tail = F)
#Probabilidade de X < 3
pnorm(3, 10, 2)
#Probabilidade de X ser maior e igual a 2 e menor e igual a 5
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





