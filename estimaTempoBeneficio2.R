#' estimaTempoBeneficio2 function
#' @param value
#' @return value
#' @export

estimaTempoBeneficio2=function(IdadeBeneficiario2, SexoBeneficiario2,MotivoFimBeneficio,TabuasMorte, Rodadas){
##Estima dura??o do benef?cio do segundo benefici?rio (caso houver)

#TB2=TempoAteSaida(IB2, SB2,TabuaF, TabuaM, rodadas)
TempoBeneficio2=estimaTempoAteMorte(IdadeBeneficiario2, SexoBeneficiario2,TabuasMorte$lxMorte.M,TabuasMorte$lxMorte.F)
####Se benefici?rio era filho menor, benef?cio acaba quando completa 21 anos ou quando morre, o que acontecer primeiro.
TamanhoPopulacao=length(IdadeBeneficiario2)/Rodadas
for (k in 1:Rodadas){
  for (j in 1:TamanhoPopulacao){
    if (MotivoFimBeneficio[j,k]==4 & (21-IdadeBeneficiario2[j,k]< TempoBeneficio2[j,k])) TempoBeneficio2[j,k]=21-IdadeBeneficiario2[j,k]
  }
}

#png(paste("IB2 x TB2 - ", pop, "Servidores.png"))
#plot(IB2[,1][SB2==1 & MSB1!=6],TB2[,1][SB2==1 & MSB1!=6], xlab="Idade inicial do benefici?rio 2", ylab="Dura??o do benef?cio em anos", col="red",pch=1,
#ylim=c(min(TB2[,1]), max(TB2[,1])),cex.axis=1.5,cex.lab=1.5, cex=1.5)
#points(IB2[,1][SB2==2 & MSB1!=6],TB2[,1][SB2==2 & MSB1!=6],col="blue", pch=0,cex.axis=1.5,cex.lab=1.5,cex=1.5)
#legend("topright", c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"),cex=1.5)
#dev.off()


return(TempoBeneficio2)
}




