#' estimaTempoBeneficio1 function
#' @param value
#' @return value
#' @export

estimaTempoBeneficio1 = function(DadosSaiAtivo,DadosServidores,ProbFamilia,TabuasMorte,Rodadas){
#Estima o tempo de dura??o do primeiro benef?cio (se houver)

MotivoSaiAtivo=DadosSaiAtivo[[1]]
SexoBeneficiario1=DadosSaiAtivo[[2]]
IdadeBeneficiario1=DadosSaiAtivo[[3]]
EstadoInicial = DadosServidores$EstadoInicial

##Tempo que benefici?rio1 recebeu benef?cio (s? sai por morte)
TempoBeneficio1=estimaTempoAteMorte(IdadeBeneficiario1,SexoBeneficiario1,TabuasMorte$lxMorte.M,TabuasMorte$lxMorte.F)
####Se benefici?rio era filho menor, benef?cio acaba quando completa 21 anos ou quando morre, o que acontecer primeiro.
TempoBeneficio1[MotivoSaiAtivo==4 & 21-IdadeBeneficiario1< TempoBeneficio1]=21-IdadeBeneficiario1[MotivoSaiAtivo==4 & 21-IdadeBeneficiario1< TempoBeneficio1]

##Gr?ficos
#png(paste("IB1 x TB1 - ", dim(DadosServidores)[1], "Servidores.png"))
#plot(IB1[,1][SB1[,1]==1 & MotivoSai[,1]!=6],TB1[,1][SB1[,1]==1 & MotivoSai[,1]!=6],ylim=c(0, (max(TB1[,1])+1)),xlim=c(0,max(IB1[,1])),xlab="Idade inicial do benefici?rio 1",
#ylab="Dura??o do benef?cio em anos",col="red",pch=1,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
#points(IB1[,1][SB1==2 & MotivoSai[,1]!=6],TB1[,1][SB1==2 & MotivoSai[,1]!=6] ,col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
#legend("topleft", c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"), cex=1.5)
#dev.off()

return (TempoBeneficio1)
}