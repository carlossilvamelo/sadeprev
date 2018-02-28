#' sugereAliquota function
#' @param value
#' @return value
#' @export

sugereAliquota=function(SalarioContribuicao,Beneficio,FundoAtual,Juros){

Tempo=dim(Beneficio)[[1]]
Rodadas=dim(Beneficio)[2]
####Valor presente das contribui??es e dos benef?cios
FatorAtualizacaoMonetaria=(1/(1+Juros))^seq(0,Tempo-1)
ValorPresenteContribuicao=ValorPresenteBeneficio=vector(length=Rodadas)
for (k in 1:Rodadas){
    ValorPresenteBeneficio[k]=sum(Beneficio[,k]*FatorAtualizacaoMonetaria)
    ValorPresenteContribuicao[k]=sum(SalarioContribuicao[,k]*FatorAtualizacaoMonetaria)
}
Aliquota=mean((ValorPresenteBeneficio-FundoAtual)/ValorPresenteContribuicao)

###Al?quota m?dia = al?quota ideal=al?quota m?dia em k simula??es
#Pressuposto: al?quota m?dia mant?m equil?brio atuarial do plano considerando a compensa??o previdenci?ria, pois assume que contribui??es futuras s?o capazes de arcar com benef?cios futuros j? descontada a compensa??o previdenci?ria.
#png(paste("Distribui??o das Al?quotas - ",N[i],"servidores.png"))
#boxplot(VPBF/VPCF, ylab="Valor da Al?quota",cex.main=1.5, cex.lab=1.5, cex.axis=1.5)
#dev.off()
#png(paste("Histograma Distribui??o das Al?quotas - ",N[i],"servidores.png"))
#hist(VPBF/VPCF, xlab="Valor da Al?quota", ylab="Frequ?ncia", main=paste(N[i],"servidores"),cex.main=1.5, cex.lab=1.5, cex.axis=1.5,col='orange', xlim=c(0.20,0.35), ylim=c(0,300))
#dev.off()

#print("Aliquota de contribuicao")
#print(Aliquota)

#print("desvio padrao")
#print(sd(ValorPresenteBeneficio/ValorPresenteContribuicao))
#print(paste("minima", min((ValorPresenteBeneficio/ValorPresenteContribuicao))))
#print(paste("maxima", max((ValorPresenteBeneficio/ValorPresenteContribuicao))))

return(Aliquota)
}