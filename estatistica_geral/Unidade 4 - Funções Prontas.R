#Comandos prontos para intervalos de confian?a e testes de hip?teses

#Leitura dos Dados
library(readxl)
Nota_Alunos <- read_excel("D:/Nota de Alunos - Parte 1.xlsx")
View(Nota_Alunos)

#Intervalo de Confian?a e Teste de Hip?tese para a M?dia com Vari?ncia Desconhecida
t.test(Nota_Alunos$Prova_1, mu = 8)

#Intervalo de Confian?a para a Propor??o (basta realizar o teste de hip?tese bilateral)
prop.test(8, 161, correct = F)

#Teste de Hip?tese para a Propor??o
# correct = F somente se o numero de amostras e grande
prop.test(8, 161, alternative = "less", correct = F)

#Teste de hip?teses para duas M?dias - Teste Bilateral
#Considerando que as vari?ncias da Prova 1 e 2 s?o diferentes!
#Ser? que a nota m?dia da Prova 1 ? igual ou diferente da Prova 2?
t.test(Nota_Alunos$Prova_1, Nota_Alunos$Prova_2, var.equal = F, conf.level = .95)

#Ser? que a nota m?dia da Prova 1 ? igual ou ? menor do que a Prova 2?
t.test(Nota_Alunos$Prova_1, Nota_Alunos$Prova_2, alternative = "less", var.equal = F, conf.level = .95)

#Considerando que as vari?ncias da Prova 1 e 3 s?o diferentes!
#Ser? que a nota m?dia da Prova 1 ? igual ou ? menor do que a Prova 3?
t.test(Nota_Alunos$Prova_1, Nota_Alunos$Prova_3, alternative = "less", var.equal = F, conf.level = .95)

#Teste de Hip?teses para duas propor??es
#Ser? que a propor??o 1 (8 em 161) ? menor que a propor??o 2 (18 em 241)?
prop.test(c(8, 18), c(161, 241), alternative = "less", correct = F)
