#' sugereFundoAtual function
#' @param value
#' @return value
#' @export

#Calcula o Fundo ideal do tempo zero necess?rio para manter o equil?brio atuarial em m?dia
sugereFundoAtual=function(SalarioContribuicao,Beneficio,Aliquota,Juros,FundoAtualReal){

  Tempo=dim(Beneficio)[[1]]
  Rodadas=dim(Beneficio)[2]
  ####Valor presente das contribui??es e dos benef?cios
  FatorAtualizacaoMonetaria=(1/(1+Juros))^seq(0,Tempo-1)
  ValorPresenteSalarioContribuicao=ValorPresenteBeneficio=vector(length=Rodadas)
  for (k in 1:Rodadas){
    ValorPresenteBeneficio[k]=sum(Beneficio[,k]*FatorAtualizacaoMonetaria)
    ValorPresenteSalarioContribuicao[k]=sum(SalarioContribuicao[,k]*FatorAtualizacaoMonetaria)
  }
  FundoAtual = (mean(ValorPresenteBeneficio/ValorPresenteSalarioContribuicao) - Aliquota)/mean(1/ValorPresenteSalarioContribuicao)

#  print("Fundo Atual Ideal")
#  print(FundoAtual)
#  print(paste("Diferenca:",round(FundoAtual-FundoAtualReal)))

  #print("desvio padr?o")
  #print(sd(ValorPresenteBeneficio/ValorPresenteSalarioContribuicao))
  #print(paste("m?nima", min((ValorPresenteBeneficio/ValorPresenteSalarioContribuicao))))
  #print(paste("m?xima", max((ValorPresenteBeneficio/ValorPresenteSalarioContribuicao))))

  return(FundoAtual)
}
