#Resolu??o da Lista de Exerc?cio Complementar

#----Exec?cio 1----
#Veja que a escolha aleat?ria dos 20 estudantes e de tal forma que cada um pode ou n?o ultrapassar os 600 pontos,
#temos aqui um modelo de probabilidade Binomial, com n = 20 e p = 0,10.

#Na primeira pergunta, deseja-se obter a probabilidade P(X <= 3)
pbinom(3, 20, .10)

#Na segunda pergunta, deseja-se a probabilidade P(X = 10)
dbinom(10, 20, .10)

#----Exerc?cio 2----
#Letra a
dbinom(15, 15, .80)

#Letra b - Veja que pelo menos 2 n?o sejam curados ? a mesma coisa que no m?ximo 13 sejam curados.
#Ou seja, tenham 13 ou menos curados.
pbinom(13, 15, .8)

#Letra c - Nesse queremos 10 ou mais curados. Lembrando que a fun??o pbinom com lower.tail = F devemos usar um passo
#antes do valor desejado, ou seja, P(X >= 10) = P(X > 9) = pbinom(9, 15, .8, lower.tail = F)
pbinom(9, 15, .8, lower.tail = F)

#----Exerc?cio 3----
#Letra a - Encontrar pelo menos 1 defeito ? o mesmo que 1 ou mais defeitos - P(X >= 1) = P(X > 0)
ppois(0, 1, lower.tail = F)

#Letra b - Encontrar no m?ximo 2 defeitos - P(X <= 2)
ppois(2, 1)

#Letra c - Encontra de 2 a 4 defeitos - P(2 <= X <= 4) = P(X <= 4) - P(X <= 1)
ppois(4, 1) - ppois(1, 1)

#Letra d - N?o mais de um defeito ? a mesma coisa de n?o encontrar no m?ximo um defeito - P(X <= 1)
ppois(1, 1)

#----Exerc?cio 4----
#Letra a - N?o haja part?culas - P(X = 0)
dpois(0, 5)

#Letra b - Probabilidade que pelo menos uma part?cula seja emitida ? igual
#a 1 menos a probabilidade de n?o emitir uma part?cula - P(X >= 1) = 1 - P(X = 0)
1 - dpois(0, 5)

#Letra c - P(2 < X <= 5) = P(3 <= X <= 5)
ppois(5, 5) - ppois(2, 5)

#----Exerc?cio 5----
#Letra a - P(X >= 1) dar? a probabilidade de o tempo de eliminar a contamina??o em mais de 1 ano. Logo 1 - P(X >= 1)
#dar? a probabilidade de da fruta n?o estar mais contaminda ap?s um ano.
1 - pexp(1, 2, lower.tail = F)

#Letra b - Se a fruta for consumida com 2 anos ou mais, a chance de estar contaminada - P(X >= 2)
pexp(2, 2, lower.tail = F)
#A seguran?a ser? um menos a probabilidade de encontrarmos uma fruta contamida com 2 ou mais anos
1 - pexp(2, 2, lower.tail = F)
#Seguran?a de 98,17%

#----Exerc?cio 6----
#Letra a - P(X < 1)
pexp(1, 3)

#Letra b - P(X > 2)
pexp(2, 3, lower.tail = F)

#----Exerc?cio 7----
#Para os 25% menores pesos
qnorm(.25, 130, 20)

#Para os 25% maiores pesos ou 75% menores pesos
qnorm(.75, 130, 20)

#----Exerc?cio 8----
#Letra a - Primeiro devemos obter a probabilidade de um piloto terminar antes dos 80 min - P(X < 80)
pnorm(80, 90, 20)
#Basta multiplicar a quantidade total de pilotos pela probabilidade de terminar em menos de 80 min para chegarmos
#no total de pilotos aprovados
65*pnorm(80, 90, 20)
#Logo se espera que 20 pilotos passem no teste

#Letra b - Os 5% melhores pilotos s?o os 5% que concluem o teste com os menores tempos
#Ou seja, queremos achar o x que atendam a seguinte f?rmula: P(X < x) = 0,05
qnorm(.05, 90, 20)
#Logo, pilotos que terminaram o teste em menos de 57,1 minutos ser?o os 5% melhores pilotos.

#----Exerc?cio 9----
#Para esse exerc?cio, voc? pode utilizar a fun??o que criei nos slides ou a fun??o zsum.test da biblioteca BSDA.
#Basta instalar a bibioteca e carreg?-la.Veja que foi dada a vari?ncia da popul??o, por isso usaremos a estat?stica
# "Z". Na fun??o, devemos inserir o desvio-padr?o populacional, por isso usei a fun??o sqrt (raiz quadrada).
#Por padr?o, o teste de hip?tese ir? fornecer um intervalo de confian?a quando fazemos um teste bilateral.
#Tamb?m como default da fun??o criar intervalo de 95% de confian?a.
install.packages("BSDA")
library(BSDA)
zsum.test(1.69, sqrt(0.01), 10)
#O intervalo de 95% de confian?a ser? IC = {1,628; 1,752}

#----Exerc?cio 10----
#Utilizando novamente uma fun??o da biblioteca BSDA por?m para vari?ncia desconhecida.
#Nesse caso, devemos especificar o n?vel de confian?a j? que o default ? de 95%.
library(BSDA)
tsum.test(25, 4.25, 500, conf.level = .92)
#O intervalo de 92% de confian?a ser? IC = {24,67; 25,33}

#----Exerc?cio 11----
#Basta utilizarmos a fun??o prop.test para encontrarmo um intervalo de confian?a, assim como para a m?dia.
#N?o ? necess?rio propor uma propor??o para a hip?tese nula. Como default ser? colocado o valor 0,5.
#Lembrando que o n?vel de confian?a como default ? de 95%.
#Para um teste exato, sem corre??o de continuedade, colocamos correct = F
prop.test(160, 200, correct = F)
#Intervalo final para a propor??o IC = {0,7391; 0,8495}

#----Exerc?cio 12----
#Letra a
#Para obter o valor da m?dia, devemos recordar sobre a f?rmula de como se obt?m os limites do intervalo.
#Primeiro devemos notar que aqui temos um caso de vari?ncia conhecida, visto que ele diz que os par?metros
#Populacionais s?o a m?dia "mi" e o desvio padr?o igual a 2.
#O limite inferior do intervalo ? igual a x_barra - z(alfa/2)*sigma/raiz(n)
#Note que esse limite inferior ? igual a 35,21. Se substitu?rmos todos os valores dados na f?rmula acima, ficaremos
#somente como x_barra de inc?gnita. Ficaria: x_barra = 35,21 + z(alfa/2)*sigma/raiz(n)
#Esse z(alfa/2) ? o escore da Normal padr?o com alfa/2 de densidade na calda superior.
#Logo usaremos a distribui??o Normal para encontrar esse valor. Veja como fica tudo
x_barra = 35.21 + qnorm((1-.95)/2, lower.tail = F)*2/sqrt(100)
x_barra
#Logo a m?dia ? igual a 35,60

#Letra b
#Basta usarmos a fun??o zsum.test para obter um intervalo de 90% de confian?a
zsum.test(35.60, 2, 100, conf.level = .9)
#O intervalo ser? IC = {35,27; 35,93}

#----Exerc?cio 13----
#Nesse exerc?cio usaremos a fun??o zsum.test pois a vari?ncia populacional foi dada (vari?ncia da distribui??o)
#A hip?tese alternativa foi avaliada em ser menor que 15 km/l ("less")
zsum.test(14.3, 3, 25, mu = 15, alternative = "less")
#Como o valor p foi superior ao valor de signific?ncia (6%) n?o devemos rejeitar a hip?tese e dizer que o consumo
#m?dio dos carros ? igual a 15 km/l.

#----Exerc?cio 14----
#Nesse exerc?cio usaremos a fun??o tsum.test pois n?o possu?mos a vari?ncia populacional e sim a amostral.
#Aqui a hip?tese alternativa ? ser menor do que 6 ("less")
tsum.test(5.5, 2, 10, mu = 6, alternative = "less")
#Nesse teste, n?o devemos rejeitar a hip?tese nula pois o valor p ? superior ao n?vel 5% de signific?ncia.

#----Exerc?cio 15----
#Nesse exemplo iremos avaliar um teste de hip?tese para a propor??o de duas amostras (A e B)
#A fun??o prop.test tamb?m funciona para este caso.
#Devemos fornecer os dados na forma de vetor para as ocorr?ncias e para o tamnaho das amostras.
#Como adicionamos as informa??es da ra?a A como a primeira propor??o e da ra?a B como a dois, a hip?tese alternativa
#ficou como "less", ra?a A menor que ra?a B.
prop.test(c(6, 10), c(58, 85), alternative = "less", correct = F)
#Assim, ao n?vel de 6% de signific?ncia n?o rejeitamos a hip?tese nula, pois o valor p foi superior (39,57%).
#Lembrando que n?o colocamos a corre??o de continuidade.

#----Exerc?cio 16----
#Lendo os dados
Usual <- c(80, 90, 93, 92, 75, 92, 72, 87, 90, 86, 78, 97)
Novo <- c(100, 85, 90, 102, 90, 99, 97, 95, 100, 94, 89, 98)
#Nesse caso, devemos usar a fun??o t.test pois os dados n?o est?o sumarizados.
t.test(Usual, Novo)
#Veja que deixamos como default as demais informa??es, pois o teste ? bilateral e as demais informa??es s?o
#calculadas automaticamente
#Nesse caso, devemos rejeitar a hip?tese nula pois o valor p foi inferior a 5% e dizemos que existe diferen?a
#significativa entre o Usual e o Novo sistema de programa??o.