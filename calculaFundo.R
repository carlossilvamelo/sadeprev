#' calculaFundo function
#' @param value
#' @return value
#' @export

calculaFundo = function(SalarioContribuicao,Beneficio,FundoInicial,Juros,Aliquota,Tempo,Rodadas){
#Reserva sem ajuste para meio do per?odo

Tempo = dim(SalarioContribuicao)[1]
Rodadas=dim(SalarioContribuicao)[2]

Fundo= matrix(0,Tempo,Rodadas)
#Reserva Matem?tica em t=0 ? 0.
Fundo[1,]=SalarioContribuicao[1,]*Aliquota-Beneficio[1,] + FundoInicial
for (t in 2:Tempo) {
  Fundo[t,]=Fundo[t-1,]*(1+Juros)+SalarioContribuicao[t,]*Aliquota-Beneficio[t,]
}
return(Fundo)

}
