library(readxl)
Nota_Alunos <- read_excel("/Users/paulaevelinga.rodrigues/Documents/02 - Study/01 - Master/04. Estatística Geral - Teoria e aplicações/Nota de Alunos - Parte 1.xlsx")
View(Nota_Alunos)

#-----Tabela de Frequ?ncia para G?nero-----#

# descobrir relacao h/m na planilha
freq_genero <- table(Nota_Alunos$Genero)
freq_genero

# funcao proporcao
prop_genero <- prop.table(freq_genero)

# arrendondar pra 2 casas decimais
perc_genero <- round(prop_genero*100,digits = 2)

# colocar info em 2 colunas separadas
coluna_freq <- c(freq_genero,sum(freq_genero))

# soma percentual total - tem ser igual a 100%
coluna_perc <- c(perc_genero,sum(perc_genero))

# nova linha de total
names(coluna_freq)[length(coluna_freq)] <- "Total"

# constroi tabela de frequencia - cbind juntar duas colunas
tabela_freq <- cbind(coluna_freq,coluna_perc)
tabela_freq


#-----Tabela de Frequ?ncia para Conceito----#

# resumo da informacao
freq_conceito <- table(Nota_Alunos$Conceito)
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

#-----Tabela de Frequ?ncia para Nota Final-----#

# dados qualitativos
# criar faixas de valores: funcao cut
# right = false; valores a direta nao entram na faixa
intervalos <- cut(Nota_Alunos$Nota_Final,breaks=0:10,right = F)
freq_notas <- table(intervalos)
freq_notas

# calcular percentual
prop_notas <- prop.table(freq_notas)
perc_notas <- round(prop_notas*100, digits = 2)
coluna_freq <- c(freq_notas,sum(freq_notas))
coluna_perc <- c(perc_notas,sum(perc_notas))
names(coluna_freq)[length(coluna_freq)] <- "Total"
tabela_freq <- cbind(coluna_freq,coluna_perc)
tabela_freq

#-----Gr?fico de Pizza-------#
rotulos <- paste(perc_genero,"%",sep="")
pie(freq_genero,main="Gráfico de Pizza: Gênero dos Alunos",
    labels = rotulos,col = rainbow(7))
# pch simbolo quadrado
legend(1,1,names(freq_genero),col = rainbow(7),pch = 11)



#-----Gr?fico de Barras ou Colunas-----#
barplot(freq_conceito)

# horiz = T exibe dados na horizontal / grafico de barras regular
barplot(freq_conceito,horiz = T)
freq_cruzada <- table(Nota_Alunos$Genero,Nota_Alunos$Conceito)
#freq_cruzada

# se nao colocar beside = T, R vai empilhar colunas
barplot(freq_cruzada, beside = T, main = "Conceito vs Gênero",
        ylab = "Número de Aluno",col = c("darkblue","red"))
legend(1,30,rownames(freq_cruzada),col = c("darkblue","red"),
       pch = 15)


#-----Histograma para Nota Final----#
hist(Nota_Alunos$Nota_Final, breaks=0:10, right = F, col = "green",
     xlab = "Notas", ylab = "Frequência", main = "Distribuição de Notas" )

#----Gr?fico de Séries------#
plot(Nota_Alunos$Prova_1, type = 'l',xlab = "ID Aluno", ylab = "Nota")
lines(Nota_Alunos$Prova_2,col = "blue")
lines(Nota_Alunos$Prova_3,col = "red")

#----Gr?fico de Caixa------#
boxplot(Nota_Alunos$Nota_Final ~ Nota_Alunos$Disciplina,
        main = "Nota Final por Disciplina",
        xlab = "Disciplina", col = c("orange","green"))