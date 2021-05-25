#Teste de Hipóteses para a Proporção
Teste_Proporcao <- function(x, p_zero, evento, H_1 = c(igual, menor, maior)){
  n <- length(x)
  x <- x[x == evento]
  prop <- length(x)/n
  z <- (prop-p_zero)/(sqrt((p_zero*(1-p_zero))/n))
  if (H_1 == "menor"){
    valor_p <- pnorm(z, 0, 1, lower.tail = T)
  }else if (H_1 == "maior"){
    valor_p <- pnorm(z, 0, 1, lower.tail = F)
  }else {valor_p <- 2*pnorm(z, 0, 1)}
  saida <- cbind(prop, p_zero, z, valor_p)
  saida
}
