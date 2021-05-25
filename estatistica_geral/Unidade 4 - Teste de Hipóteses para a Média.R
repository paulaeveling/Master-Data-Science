#Comandos para Teste de Hip?teses para a M?dia
#Teste de Hip?teses para a M?dia com Vari?ncia Conhecida

# hipoteses podem ser nula, menor ou maior

Teste_Media_Conhecida <- function(x, sigma, mi_zero, H_1 = c(igual, menor, maior)){
  n <- length(x)
  media <- mean(x)
  z <- (media-mi_zero)/(sigma/sqrt(n))
  if (H_1 == "menor"){
    valor_p <- pnorm(z, 0, 1, lower.tail = T)
  }else if (H_1 == "maior"){
    valor_p <- pnorm(z, 0, 1, lower.tail = F)
  }else {valor_p <- 2*pnorm(z, 0, 1)}
  saida <- cbind(media, mi_zero, z, valor_p)
  saida
}


#Teste de Hip?teses para a M?dia com Vari?ncia Desconhecida
Teste_Media_Desconhecida <- function(x, mi_zero, H_1 = c(igual, menor, maior)){
  n <- length(x)
  media <- mean(x)
  desvio <- sd(x)
  t <- (media-mi_zero)/(desvio/sqrt(n))
  if (H_1 == "menor"){
    valor_p <- pt(t, n-1, lower.tail = T)
  }else if (H_1 == "maior"){
    valor_p <- pt(t, n-1, lower.tail = F)
  }else {valor_p <- 2*pt(t, n-1)}
  saida <- cbind(media, desvio, mi_zero, t, valor_p)
  saida
}


# EXERCICIO 1
# taxa media = 18cm/s
# desvio = 2.8 amostra 1 e 3.0 amostra 2
# valor p para availar se a taxa media vem da mesma populacao = 0

# EXERCICIO 2
# amostras = 5
# 