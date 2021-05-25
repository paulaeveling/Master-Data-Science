#Intervalo de Confian?a para a Propor??o
IC_Proporcao <- function(x, confianca, evento){
  n <- length(x)
  x <- x[x == evento]
  prop <- length(x)/n
  IC_inf = prop - qnorm((1-confianca)/2,0,1, lower = FALSE)*(sqrt((prop*(1-prop))/n))
  IC_sup = prop + qnorm((1-confianca)/2,0,1, lower = FALSE)*(sqrt((prop*(1-prop))/n))
  saida <- cbind(IC_inf,IC_sup)
  saida
}