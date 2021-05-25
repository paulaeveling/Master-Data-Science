#Comandos sobre Intervalos de Confian?a
#Intervalo de Confian?a para a M?dia - Vari?ncia Conhecida
IC_Media_Conhecida <- function(x, sigma, confianca){
  n <- length(x)
  x_barra <- mean(x)
  IC_inf = x_barra - qnorm((1-confianca)/2,0,1, lower = FALSE)*(sigma/sqrt(n))
  IC_sup = x_barra + qnorm((1-confianca)/2,0,1, lower = FALSE)*(sigma/sqrt(n))
  saida <- cbind(IC_inf,IC_sup)
  saida
}

IC_Media_Conhecida (60139.7, 16, 0.95)


#Intervalo de Confian?a para a M?dia - Vari?ncia Desconhecida
IC_Media_Desconhecida <- function(x, confianca){
  n <- length(x)
  x_barra <- mean(x)
  s <- sd(x)
  IC_inf = x_barra - qt((1-confianca)/2,n-1, lower = FALSE)*(s/sqrt(n))
  IC_sup = x_barra + qt((1-confianca)/2,n-1, lower = FALSE)*(s/sqrt(n))
  saida <- cbind(IC_inf,IC_sup)
  saida
}

