library(readxl)
Casas <- read_excel("/Users/paulaevelinga.rodrigues/Documents/02 - Study/01 - Master/05. Técnicas Estatísticas de Predição- Teoria e Aplicações/DataSets - Exemplos/Casas.xlsx")

# PROCEDIMENTO PARA USO DE UM MODELO DE REGRESSAO
# 1 - Definir problema
# 2 - Selecionar variaveis (preditoras e predita)
# 3 - Diagrama de dispersao
# 4 - Gerar modelo de regressao
# 5 - Verificar existencia de outliers
# 6 - Verificao do ajuste
# 7 - Uso do modelo


#-----Exemplo 1 de Correla??o------

r1 <- cor(Casas$Preco_Anunciado, Casas$Idade)
r1
plot(Casas$Preco_Anunciado, Casas$Idade)

# ANALISE:
# relacao entre media e fraca - quanto mais proxima de 1, mais forte a relacao


#-----Exemplo 2 de Correla??o-----
r2<- cor(Casas$Preco_Anunciado, Casas$Area_Util)
r2
plot(Casas$Preco_Anunciado, Casas$Area_Util)

#----Teste de Correla??o Linear----
teste1 <- cor.test(Casas$Preco_Anunciado, Casas$Idade)
teste1
teste2 <- cor.test(Casas$Preco_Anunciado, Casas$Area_Util)
teste2

#----Exemplo 1 de Regress?o----
r1 <- cor(Casas$Idade, Casas$Preco_Anunciado)
r1
plot(Casas$Idade, Casas$Preco_Anunciado)
ajuste <- lm(Preco_Anunciado ~ Idade, data = Casas)
summary(ajuste)
lines(Casas$Idade, ajuste$fitted.values, col = 2)

#----Exemplo 2 de Regress?o----
r2<- cor(Casas$Area_Util, Casas$Preco_Anunciado)
r2
plot(Casas$Area_Util, Casas$Preco_Anunciado)
ajuste <- lm(Preco_Anunciado ~ Area_Util, data = Casas)
summary(ajuste)
# col = color; not column
lines(Casas$Area_Util, ajuste$fitted.values, col = 2)


#----Exemplo Geral Regress?o Linear Simples----

# PROCEDIMENTO PARA USO DE UM MODELO DE REGRESSAO
# 1 - Definir problema
# 2 - Selecionar variaveis (preditoras e predita)
# 3 - Diagrama de dispersao
# 4 - Gerar modelo de regressao
# 5 - Verificar existencia de outliers
# 6 - Verificao do ajuste
# 7 - Uso do modelo

attach(RLS_BEZERRO)
plot(`Medida(CM)`,`Peso (KG)`, pch = 19, col = "Dark Blue", main = "Medida x Peso",
     cex.axis = 1.5, cex.main = 1.5, cex.lab = 1.5, cex = 1.5)

# calcular correlacao entre medida e peso
# resultado = 0.881, entao correlacao e muito alta
cor.test(`Medida(CM)`,`Peso (KG)`)

# armazena modelo de regressao em ajuste
# analizar coeficientes: a cada 1cm, peso e acrescido em 2.70kg
ajuste <- lm(`Peso (KG)` ~ `Medida(CM)`)
summary(ajuste)

# checar outliers atraves de boxplot
# nao ha asteriscos fora das caixas, significa que nao ha outliers
boxplot(`Medida(CM)`, col = "Light Blue", main = "Box Plot Medida (CM)",
        cex.axis = 1.5, cex.main = 1.5, cex.lab = 1.5)
boxplot(`Peso (KG)`, col = "Light Green", main = "Box Plot Peso (KG)",
        cex.axis = 1.5, cex.main = 1.5, cex.lab = 1.5)


# checar outliers atraves da padronizacao com base na media
# nenhum passou da faixa de -3 a 3
RLS_BEZERRO$Z_Medida <- scale(`Medida(CM)`)
RLS_BEZERRO$Z_Peso <- scale(`Peso (KG)`)
range(RLS_BEZERRO$Z_Medida)
range(RLS_BEZERRO$Z_Peso)

# verificar ajuste e suposicoes (residuos)
# residuos 1Q e 3Q devem estar entre -3 a 3
# media deve estar proxima de 0
# se alguma dessas condicoes nao for verdadeira, ha residuos
# Multiple R-squared: quanto mais alto, melhor. Qualidade de predicao muito alta. Consegue prever 77% dos dados.
ajuste <- lm(`Peso (KG)` ~ `Medida(CM)`)
summary(ajuste)

# verificao do ajuste e suposicoes
par(mfrow=c(2,2))
plot(ajuste)

# modelo de predicao
predicao <- data.frame(Peso_previsto = c(20, 28))
coef_ajuste <- coefficients(ajuste)
predicao$Peso_Previsto <- coef_ajuste[1]+coef_ajuste[2]*predicao
colnames(predicao) <- c("Medida", "Peso_previsto")
predicao



#----Exemplo de Regress?o M?ltipla----
# Etapas de Validacao do Modelo
# 1- Verificacao dos coeficientes das variaveis preditoras
# 2- Verificacao da ANOVA do modelo
# 3- R ao quadrado
# 4- Multicolinearidade (VIF)
# 5- Metodo de selecao de variaveis (stepwise)
# 6- Analise de residuos

attach(Casas)
ajuste_multi <- lm(Preco_de_Venda ~ Area_Util + Idade + Comodos)
summary(ajuste_multi)

install.packages("car")
library(car)
vif(ajuste_multi)

install.packages("MASS")
library(MASS)
stepAIC(ajuste_multi, direction = "both")




#----Exemplo Geral Regress?o Linear Composta----

# ETAPAS DE VALIDACAO DO MODELO
# 1 - Verificacao dos coeficientes das variaveis preditoras
# 2 - Verificacao da ANOVA
# 3 - R ao quadrado ajustado
# 4 - Multicolinearidade 
# 5 - Metodo de Selecao de Variaveis (stepwise)
# 6 - Analise de Residuos

attach(HOUSES_EUA)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

# Remover colunas 2 e 5 pois nao tem correlacao
HOUSES_Numeric <- HOUSES_EUA[,-2]
HOUSES_Numeric <- HOUSES_Numeric[,-4]

# exibir dados correlacionados
chart.Correlation(HOUSES_Numeric)

# ajuste do modelo
# incluir factor() para tratar valores numericos como str
# variavel dummies = transforma variavel em Yes/No (dicotomica) -- feita_tijolos
ajuste <- lm(Preco ~ Ofertas + `Tamanho (pes)` + Quartos + Banheiros + factor(Bairro) + Feita_tijolos)
summary(ajuste)


install.packages("car")

# Multicolinearidade
# nenhum vif foi maior do que 5, entao nao ha problemas de multicolinearidade
library(car)
vif(ajuste)

install.packages("MASS")

# Stepwise - Selecionar melhor modelo
# quanto menor, melhor
# se uma variavel aumenta o AIC, ela deve ser mantida no modelo
library(MASS)
stepAIC(ajuste, direction = "both")

# Analise de rediduos
par(mfrow=c(2,2))
plot(ajuste)

# Problema de Escala
# variaveis quantitativas sao discretas e tem valores extremamente pequenos
# valor e muito alto (coeficiente) e quartos e banheiros sao muito baixos
# padronizar variaveis muito grandes
ajuste <- lm(scale(Preco) ~ Ofertas + scale(`Tamanho (pes)`) + Quartos + Banheiros + factor(Bairro) + Feita_tijolos)
summary(ajuste)

vif(ajuste)
stepAIC(ajuste, direction = "both")
par(mfrow=c(2,2))
plot(ajuste)




#----Exemplo 1 Regress?o Log?stica Simples----

# Estima probabilidade de evento ocorrer
# Binaria = duas categorias
# Multinomial = 3 ou mais
# Odds Ratio

# PROCEDIMENTOS
# 1 - Definir corretamente variavel resposta do tipo binaria (0 ou 1)
# 2 - Definir variavel preditora quantitativa continua
# 3 - Ajuste do modelo
# 4 - Coeficiente significativo do modelo
# 5 - Avaliacao das suposicoes e residuo

# EXEMPLOS
# 1 - Sera que os anos de experience aumentam as chances de contratcao? (sim/nao)
# 2 - Sera que o excesso de peso aumenta as chances de infarto? (sim/nao)
# 3 - Sera que ingerir menos acucar diminui chances de ter diabetes? (sim/nao)

# Cuidado com extrapolacao - se x >= 5 e <= 30 entao essa eh a area da amostra

install.packages('pscl')

attach(Experiencia_Tarefa)

# glm: general linear model
# informar variavel dicotomica (binaria)
# y = tarefa concluida
# x = experiencia
# como analisar informacoes do summary:
# importante olhar valores dos coeficientes
# intercept = beta0
# coefficients: ambas variaveis tiveram valores significativos inferiores a 0.05 (ultima coluna)
# residuals: nenhum residuo extremamente alto (acima de 3 ou abaixo de -3)
ajuste <- glm(Tarefa_concluidda ~ Experiencia, family = binomial)
summary(ajuste)

# anova: analise de deviancia - teste quadrado (avaliar se modelo esta adequado ou nao)
# valor Pr(>Chi) e menor que 5%. Significa que variavel e significativa
anova(ajuste, test = 'Chisq')

# razao de chance
# invervalo de confianca exponencial de 95%: confint.default
# exponencial = da as odds
# experiencia: odds de experiencia da a razao de chance de ter concluido o teste a media que aumenta os anos de experiencia
# neste caso, a cada ano de expencia, a chance de concluir o teste cresce em 1.175 == 17.53%
# confianca do teste: 97.5% - extremamente alto (otimo)
# odds ratio eh diferente de 1
# quanto mais experiencia, mais chances de concluir o teste
require(MASS)
exp(cbind(coef(ajuste), confint.default(ajuste)))

# analisar curva dos valores preditos
# nao esta funcionando
library(pscl)
pR2(ajuste)
plot(Experiencia_Tarefa)
y_chapeu <- data.frame(X = Experiencia_Tarefa$Experiencia, y_chapeu = ajuste$fitted.values)
y_chapeu <- y_chapeu[order(y_chapeu$X),]
lines(y_chapeu, col = 2)
# y_chapeu
plot(y_chapeu)

# Verificacao de residuos e valores ajustados
# nao ha pontos fora das margins Residuals vs Leverage
par(mfrow=c(2,2))
plot(ajuste)

# testar se valores estimados condizem com observacoes
# se probabilidade superior a 0.5 = concluiu a tarefa
# se probabilidade menor igual do que 0.5 = nao concuiu a tarefa
# availiar nivel de assertividade
y_chapeu <- data.frame(X = Experiencia_Tarefa$Experiencia, y_chapeu = ajuste$fitted.values)
# cria coluna nova chamada conclusao de acordo com probabilidade
y_chapeu$conclusao <- ifelse(y_chapeu$y_chapeu > 0.5, 1, 0)
# adiciona y observado (estimado + verdadeiro)
y_chapeu$y <- Tarefa_concluidda
# cria tabela com y verdadeiro e estimado - colunas 3 e 4
table(y_chapeu[,3:4])
# media de acerto = 76% de assertividade (otimo)
mean(y_chapeu$conclusao == y_chapeu$y)



#----Exemplo 2 Regress?o Log?stica Composta----
# variaveis podem ser quantitativas continuas ou discretas ou ainda qualitativas
# variaveis qualitativas devem ser transformadas em variaveis dummies
# por exemplo, se tiver 3 categorias > 2 dummies serao criadas
# bom = dummy 1; regular = dummy 2; ruim = null (ausente)

# PROCEDIMENTOS
# 1 - Definir corretamente variavel tipo binaria
# 2 - Definir variaveis preditoras
# 3 - Ajuste do Modelo
# 4 - Coeficiente significos e do modelo
# 5 - Verificacao da Multicolinearidade (dependencia entre preditoras - nao pode ocorrer)
# 6 - Stepwise
# 7 - Avaliacao das Suposicoes e Residuo

#library(readxl)
#Lasagna_Triers <- read_excel("D:/Google Drive/PUC P?s/PUC Virtual/An?lise Preditiva/Lasagna Triers.xlsx")
#View(Lasagna_Triers)

# Variavel resposta deve ser 1 ou 0, por isso so ajustamos variavel Have Tried
# cria uma nova coluna Compra para armazenar variavel resposta
# usa o glm para armazenar primeiro ajuste com todas as variaveis
# familia: binomial (sucesso ou fracasso | compra ou nao compra)
attach(Lasagna_Triers)
Lasagna_Triers$Compra <- ifelse(Lasagna_Triers$`Have Tried`=="Yes", 1, 0)
ajuste <- glm(Compra ~ Age + Weight + Income + `Pay Type` +
                `Car Value` + `CC Debt` + Gender + `Live Alone` +
                `Dwell Type` + `Mall Trips` + Nbhd, family = binomial)

# neste modelo, peso nao eh significativo
# variaveis significativas sao marcadas com * (ate 5%)
# variaveis com baixa significancia sao marcadas com . (ate 10%)
# nesta etapa nao excluimos nenhuma variavel do modelo - stepwise vai verificar o melhor modelo a ser utilizado
summary(ajuste)

# stepwise
# adiciona e remove variaveis para encontrar modelo com menor AIC
# neste caso, somente 6 variaveis sao selecionadas para o modelo
# demais variaveis nao sao significativas para o modelo
require(MASS)
stepAIC(ajuste, direction = "both")

# novo ajuste considerando somente variaveis relevantes
# neste novo ajuste, o sexo masculino se mostrou menos relevante que demais variaveis
# verificar deviance residuals (entre -3 e 3)
ajuste2 <- glm(Compra ~ Age + `Pay Type` + Gender + `Live Alone` + 
                 `Mall Trips` + Nbhd, family = binomial)
summary(ajuste2)

# verificar os coeficientes
# encontrar invervalo de confianca
# coef = extrai todos os coeficientes estimados de ajuste 2
# confint.default = intervalo de confianca de ajuste 2
# exp = exponencial do coeficiente

# analise:
# aumento de idade diminui probabilidade de compra (odd < 1 | coluna 1)
# salariado aumenta probabilidade de compra
# sexo masculino: esta em 1 - mas aumenta chance de comprar lasanha em 47%
# viver sozinho aumenta probabilidade de compra em 240%
# visitas ao shopping aumenta probabilidade de compra em 102%
# vive no sul aumenta prob de compra em 137%
# vive no oeste aumenta prob de compra em 745%

# se numero for maior que 1, diminui 1 e encontra percentual
# se for menor que 1, 1 - valor para encontrar chances de compra
require(MASS)
exp(cbind(coef(ajuste2), confint.default(ajuste2)))

# testar qualidade do ajuste
# valor esperado eh ser menor do que 5%
# indica que nao ha problemas com a variacao do modelo
pchisq(ajuste2$deviance, ajuste2$df.residual, lower.tail = F) #Teste Chi-quadrado do Deviance
# valor extremamente baixo, rejeitando hipotese nula
# betas sao realmente diferentes de 0
pchisq(ajuste2$null.deviance - ajuste2$deviance,
       ajuste2$df.null - ajuste$df.residual, lower.tail = F) #Teste Chi-quadrado da Regress?o


# verificar multicolinearidade
# todos os VIFs da ultima coluna sao menores que 5
# indicando que nao ha problemas de multicolinearidade
library(car)
vif(ajuste2)

# McFadden: estimado pelo pR2, melhor estimador
# analisar duas ultimas colunas
# modelo consegue explicar a variacao do y em 47.7%
library(pscl)
pR2(ajuste2)

# verificar posicao dos residuos
# normalidade seguidas
# outliers nao encontrados - pontos distantes da linha de cookie
par(mfrow=c(2,2))
plot(ajuste2)

# estimando probabilidade
# individuo de 20 anos
# salariado
# sexo masculino
# vive sozinho
# vai 10 vezes ao shopping 
# mora na regiao oeste
coef2 <- data.frame(coef(ajuste2))

# coef2[1]: intercepto
# coef2[2]: idade * 20
# coef2[3]: salariado
# coef2[4]: sexo -- se fosse sexo femino, era so remover coef da equacao
# coef2[5]: se mora sozinho -- se nao morasse sozinho era so remover coef da equacao
# coef2[6]: quantas vezes vai ao shopping * 10
# coef2[8]: bairro que mora
# como ele nao mora no sul, coef2[7] e removido da equacao

# depois que calcular o exp de cada coef, tem que calcular 1 + coef
# 1 + exatamente formula anterior
p_hat <- (exp(coef2[1,] + coef2[2,]*20 + coef2[3,] + coef2[4,] +
              coef2[5,] + coef2[6,]*10 + coef2[8,]))/
          (1+exp(coef2[1,] + coef2[2,]*20 + coef2[3,] + coef2[4,] +
             coef2[5,] + coef2[6,]*10 + coef2[8,]))
p_hat

# chance dessa pessoa comprar a lasanha e de 99%



# Análise Multivariada de Dados

# conjunto de tecnicas que ajudam a entender dados utilizando uma enorme quantidade de variaveis
# foco em encontrar relacionamento entre inumeras variaveis que nao se correlacionam
# mais de uma variavel resposta
# objetivo nao e predicao, mas entender estrutura de dados

#----Exemplo Analise Fatorial----

# objetivo: analisar inter-relacoes entre grande volume de variaveis
# encontrar modo de condensar informacoes contidas em diversas variaveis
# variaveis metricas ou quantitativas
# estrutura minima de cinco variaveis por fator
# entender relacoes entre variaveis de clientes atraves de questionario
# analise fatorial exploratoria ou confirmatoria
# variaveis sao separadas por fatores e nao podem estar em 2 fatores ao mesmo tempo
# condensar dados relevantes sem perder informacoes

install.packages('psych')
install.packages('GPArotation')

attach(EFA)

library(psych)
library(GPArotation)

# fa.parallel | fm = componentes principais para escolha de fatores | fa = analise fatorial
# retorna grafico e sugestao de numero de fatores
# recomendado 4 fatores
parallel <- fa.parallel(EFA, fm = 'pa', fa = 'fa') #escolha do n?mero de fatores

# determinar numero de fatores, neste caso 3
# rotacao: varimax (correlacao entre os fatores) ou oblimin (nao existe correlacao entre fatores)
# retorna matriz de relacionamento entre variaveis
# PA1 PA2 PA3 sao of fatores
# logo abaixo estao os pesos
# se variavel tem peso entre -3 e 3, significa que peso eh significativo
tresfatores <- fa(EFA, nfactors = 3, rotate = "oblimin", fm="pa")
tresfatores

# entender melhor como fatores estao divididos em relacao as variaveis
# omite valores acima de 0.3
# algumas variaveis ficaram de fora, o que influencia na analise de resultados
print(tresfatores$loadings, cutoff = 0.3)

# e porque variaveis ficaram de fora, rodamos de novo a linha mas com 4 fatores
quatrofatores <- fa(EFA, nfactors = 4, rotate = "oblimin", fm="pa")
quatrofatores

# corta a 0.3 para analisar estrutura de dados
# uma variavel nao pode estar em 2 componentes (PA1 e PA2 por exemplo)
# olhar o item seguranca que esta em PA1 e PA2
# tem que escolher um unico componente
# escolher o qual tiver o maio valor em absoluto, neste caso PA2
# essa escolha e feita automaticamente
print(quatrofatores$loadings, cutoff = .3)

# roda as linhas abaixo novamente para analise do modelo
# RMSR: raiz quadrado dos residuos medios - quanto mais proximo de 0 melhor, aqui eh 0.05
# RMSEA: index da aproximacao do erro medio ao quadrado - deve ser inferior a 0.05 (0.036)
# Tucker-Lewis Index (TLI) - acima de 0.900 eh aceitavel
# Cumulative Var - representatividade 37% dos dados (muito baixo - esperado valores acima de 60%)
# talvez a baixa quantidade de dados tenha influenciado os resultados
# se espera se que tenham 5 respostas para cada variavel, neste modelo cada variavel teve 5 possiveis respostas
# quanto mais respostas, mais robusto o modelo
quatrofatores <- fa(EFA, nfactors = 4, rotate = "oblimin", fm="pa")
quatrofatores

fa.diagram(quatrofatores)


#----Exemplo An?lise de Cluster----

# objetivo: definir estrutura de dados 
# agrupa variaveis mais parecidas em clusters de observacao
# analise de cluster de variavel nao eh tao boa quanto analise fatorial
# melhor custer de observacoes
# variaveis metricas ou quantitativas
# variavel que defini os cluster eh nao metrica
# analise visual de cluster 
# utilizacoes: pesquisa de marketing que queira determinar segmentos

install.packages("cluster")

# nao utiliza a primeira coluna, deve ser ocultada
# nao indica a quantidade de proteina, mas distancias (distancia euclidiana)
# distancia e calculada atraves da media 
# identificar quantos clusters temos
attach(Protein)
library(cluster)
hc <- hclust(dist(Protein[,-1]), method = 'average') 
plot(hc)

# como criar cluster
# neste caso estamos criando 3 clusters
clusterCut <- cutree(hc, 3)
# coloca linha no grafico para identificar clusters
rect.hclust(hc, k=3, border="red")

# visualizacao de cluster em grafico de dispersao
clusplot(Protein[,-1], clusterCut,
         main='Represetacao Grafica 2D - Solucao com 3 Clusters',
         color=TRUE, shade=TRUE, labels=2, lines=0)

# mostrar como paises ficaram organizados
# ordenados de acordo com cluster que pertencem
Protein$Grupos <- clusterCut
Grupo_ordenado <- Protein[order(Protein$Grupos),]
somente_grupo <- subset(Grupo_ordenado,select = c(Country, Grupos))
View(somente_grupo)


#----Exemplo An?lise Discriminante----

# discrima o que tem de mais diferente entre grupos
# classifica objetos em grupos com base em escores em conjunto de variaveis independentes
# variaveis: uma variavel dependente categorica excludente (mesmo objeto em 2 categorias)
# variaveis independentes tanto categoricas quanto metricas
# pode ser utilizado para identificar melhores caracteristicas que distinguem um comprador de nao comprador
# semelhante a regressao logistica

# Diferenca entre LDA (linear discriminant analysis) e QDA (quadratic discriminant analysis)
# LDA: quando correlacao entre variaveis independentes sao iguais
# QDA: quando se assume que nao ha covariancia entre variaveis independentes

install.packages("klaR")
attach(Candidatos)

# abre a planilha sem a primeira coluna pois nao e relevante
# variaveis precisa ser categorica, precisa fazer conversao
ds_candidatos <- Candidatos[,-1]
ds_candidatos$Grupo <- factor(ds_candidatos$Grupo, levels = c(1, 2, 3),
                              labels = c("Aprovado", "Espera", "Reprovado"))
View(ds_candidatos)

# analise discriminante
# lda - linear discrimination analysis
# prior probability = chances de cada canditado entrar em cada grupo
# 32% aprovado, 24% espera, 43% reprovado
# Coefficients of linear discriminants: cria 2 retas para diferenciar grupos
attach(ds_candidatos)
require(MASS)
library(klaR)
ajuste <- lda(Grupo ~ Nota_tecnica + Historico)
ajuste

# grafico de dispersao do modelo discriminante
plot(ajuste, col = as.integer(Grupo), cex = 1.5)

# verificar acuracia do modelo
# linha diagonal mostra acertos
# tudo que tiver fora da linha diagonal, foram classificados errados
# acuracia do modelo: 91%
table(ds_candidatos$Grupo, Predito = predict(ajuste, Candidatos[,3:4])$class)
mean(Grupo == predict(ajuste, Candidatos[,3:4])$class)

# itens em vermelho foram classificados errado
partimat(Grupo ~ Nota_tecnica + Historico, method="lda")

plot(ajuste, dimen = 1, type = "b")

# analise discriminante quadratica
# nao entrega equacao, neste caso
# modelo ficou muito mais acurado
# nao houve canditados em grupos errados
# analisar diagonal novamente
ajuste2 <- qda(Grupo ~ Nota_tecnica + Historico)
ajuste2
table(Grupo, Predito = predict(ajuste2, Candidatos[,3:4])$class)
mean(Grupo == predict(ajuste2, Candidatos[,3:4])$class)

partimat(Grupo ~ Nota_tecnica + Historico, method="qda")

#----Exemplo de Analise de Series Temporais----

# conjunto de observacoes ordernadas no tempo
# analisar e modelar observacoes vizinhas
# tem que haver dependencia dos dados no tempo
# diversas aplicacoes: economia, medicina, metereologia, mercado
# precos diarios de acoes, casos semanais de uma doenca, temperatura diaria etc

# OBEJTIVOS
# prever o futuro
# compreender mecanismo gerador da serie
# predizer comportamento futuro da serie
# descrever comportamento da serie
# procurar periodicidades relevantes nos dados

# tendencia, sazonalidade e ciclos
# modelos devem ser estacionaveis
# modelos nao estacionarios devem ser convertidos para estacionarios

# tendencia: oscilacao em linha crescente ou decrescente
# sazonalidade e variacao: comportamento eh parecido (sazonalidade)
# aleatoriedade: nenhuma analise sera 100% -- erro pode acontecer -- ruido branco

# modelo ARIMA
# modelo autoregressivo integrado de medias moveis
# notacao estatistica: ARIMA(p, d, q)
# parametros sao estimados pelos dados amostrais
# p: autoregressao -- qunatas regressoes estamos fazendo em relacao a serie
# d: integracao (diferenciacao) -- numero de diferenciacoes
# q: nivel de medias moveis -- controla sazonalidade

# etapas ARIMA
# grafico da serie para indicar tendencia, ciclo e sazonalidade
# tornar serie estacionaria por diferenciacao
# controlar variacao por transformacao
# determinar se termos autoregressivos (AR) ou de medias moveis serao necessarios
# testar modelo que funciona melhor - menor AIC
# analisar residuos -- media zero e variancia constante -- modelo correto

install.packages("tseries")

# dataset esta armazenado no RStudio
data("AirPassengers")
View(AirPassengers)

# serie com tendencia, variacao e sazonalidade
# comportamento parecido em todos os ciclos
plot(AirPassengers)

# boxplot dentro de cada ciclo
# utilizado para confirmar sazonalidade
# destaque da serie:
# tendendia: a cada ano, numero medio de passageiros aumenta
# sazonalidade: o comportamento da serie e parecida a cada 12 meses
# variacao: aumenta a cada ciclo de 12 meses ao longo da serie
boxplot(AirPassengers~cycle(AirPassengers))

# autocorrelation factor indica estacionaridade
# se fosse estacionaria, haveria so 1 lag e as demais estariam proximas a linha azul
# curva indica que serie nao e estacionaria
acf(AirPassengers)

# partial autocorrelation factor - calculo de medias moveis
# lag 1 sempre tera pico mais alto, as demais devem estar proximas de 0 (dentro da linha azul)
pacf(AirPassengers)

#plot(decompose(AirPassengers))

# variacao homocedástica - a mesma ao longo do tempo
# log ajuda na variacao, mas nao na acf e pacf
plot(log(AirPassengers))
acf(log(AirPassengers))
pacf(log(AirPassengers))

# estacionar serie
# diferenciacao entre ontem e hoje
# se comporta aleatoriamente proxima de 0, tornando-a estacionada
# pacf: picos indicam quantas medias moveis serao necessarias - neste caso 1 ou 2 medias moveis
plot(diff(log(AirPassengers)))
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))

# verificar se serie eh estacionaria
# valor p inferior a 5% indica que serie e estacionaria
# neste caso, p = 1%, serie estacionaria
# se serie nao for estacionaria, tem que fazer a diferenciacao da diferenciacao
library(tseries)
adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)

# estacionaridade ok, medias moveis ok
# este modelo nao tem autoregressao
# c(0,1,1) -- 0, sem autoregressao, 1 diferenciacao, 1 media movel
# seasonal = acontece de 12 em 12 meses, 
# estudar comportamento da serie para encontrar periodicidade
ajuste <- arima(log(AirPassengers), c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
# calculo da predicao dos proximos 10 anos
# 120 predicoes
pred <- predict(ajuste, n.ahead = 10*12)
# predicao da serie nao transformada
# como utilizamos log anteriormente, tem que usar exp para reverter o log -- para serie original
# c(1,3) == preenchida e serrilhada; formato da linha para diferenciar dados de predicao
ts.plot(AirPassengers,exp(pred$pred), log = "y", lty = c(1,3))
# predicao da serie transformada
# tem que colocar dataset original, ja com exponencial, e log = y
ts.plot(AirPassengers,exp(pred$pred), lty = c(1,3))

