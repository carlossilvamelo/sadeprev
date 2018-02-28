#' estimaTempoRPS function
#' @param value
#' @return value
#' @export

estimaTempoRPPS = function(DadosServidores,TMD, Rodadas){
###Calcula tempo at? a sa?de por morte ou invalidez pela TMD.

Idade = DadosServidores$Idade
Sexo = DadosServidores$Sexo
IdadeAposentadoria = DadosServidores$IdadeAposentadoria
IdadeAposentadoria[is.na(IdadeAposentadoria)] = -1
EstadoInicial = DadosServidores$EstadoInicial
TempoRPPS = TempoParaAposentadoria = matrix(-1,nrow=length(Idade),ncol=Rodadas)
###Calcula tempo at? a sa?da pelas t?buas definidas.

IndicesAtivo = which(DadosServidores$EstadoInicial==1)

TempoRPPS[IndicesAtivo,]=estimaTempoAteMorte(matrix(rep(Idade[IndicesAtivo],Rodadas), ncol=Rodadas),matrix(rep(Sexo[IndicesAtivo],Rodadas), ncol=Rodadas),TMD$lxTotalTMD.M,TMD$lxTotalTMD.F)


#limite=c(min(TempoRPPS[,1]), max(TempoRPPS[,1]))  ##Garante que os dois gr?ficos ficar?o na mesma escala
#png(paste("Tempo at? morte ou invalidez x Idade - ",dim(DadosServidores)[1], "Servidores .png"))
#plot(Idade[Sexo==1],TempoRPPS[,1][Sexo==1], xlab="Idade atual", ylab="Tempo at? a sa?da por invalidez ou morte", col="red",pch=1, ylim=limite,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
#points(Idade[Sexo==2],TempoRPPS[,1][Sexo==2], xlab="Idade atual", ylab="Tempo at? a sa?da por invalidez ou morte", col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
#legend("topright", c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"), cex=1.5)
#dev.off()

TempoParaAposentadoria[IndicesAtivo,] = rep(IdadeAposentadoria[IndicesAtivo]-Idade[IndicesAtivo], Rodadas)
TempoParaAposentadoria[TempoParaAposentadoria<0] = -1
TempoRPPS[TempoRPPS>TempoParaAposentadoria]=TempoParaAposentadoria[TempoRPPS>TempoParaAposentadoria]

#TempoRPPS[IndicesAtivo,]=TempoRPPS[IndicesAtivo,]+1 #Corre??o para satisfazer a condi??o de que as contribui??es s?o feitas no in?cio de cada per?odo, e n?o no final
TempoRPPS[IndicesAtivo,] = TempoRPPS[IndicesAtivo,]+(Idade[IndicesAtivo]-DadosServidores$IdadeEntradaRPPS[IndicesAtivo])
TempoRPPS[TempoRPPS<0] = -1


#png(paste("TempoRPPS x Idade - ",dim(DadosServidores)[1], "Servidores .png"))
#plot(Idade[Sexo==1],TempoRPPS[,1][Sexo==1], xlab="Idade atual", ylab="Tempo como ativo", col="red",pch=1, ylim=limite,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
#points(Idade[Sexo==2],TempoRPPS[,1][Sexo==2], col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
#legend(x=50, y = limite[2], c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"), cex=1.5)
#dev.off()

return(TempoRPPS)
}



