#' estimaIdadeAposentadoria function
#' @param value
#' @return value
#' @export

estimaIdadeAposentadoria = function (DadosServidores){
##Estima idade m?nima de aposentadoria programada.

TamanhoPopulacao=length(DadosServidores[,1])   #declara o tamanho da popula??o inicial
DadosServidores[is.na(DadosServidores)] = 0
Sexo = DadosServidores$Sexo
Idade = DadosServidores$Idade
EstadoInicial = DadosServidores$EstadoInicial
IdadeEntradaRPPS = DadosServidores$IdadeEntradaRPPS
TempoRGPS = DadosServidores$TempoRGPS
IdadeAposentadoria=AposentadoriaPorTempo=AposentadoriaPorIdade=replicate(TamanhoPopulacao,70)
TipoAposentadoria = vector (length=TamanhoPopulacao)
## Idade de aposentadoria por idade: AposentadoriaPorIdade
#Idade de Aposentadoria por idade e tempo de contribui??o: AposentadoriaPorTempo
## Menor idade em que ? eleg?vel ? aposentadoria: IdadeAposentadoria


  ##Elegibilidade ? aposentadoria por idade (m?nimo de 10 anos no servi?o p?blico)
  AposentadoriaPorIdade[Sexo==1 & IdadeEntradaRPPS>=35 & IdadeEntradaRPPS<=50]=60
  AposentadoriaPorIdade[Sexo==1 & IdadeEntradaRPPS>50]=IdadeEntradaRPPS[Sexo==1 & IdadeEntradaRPPS>50]+10
  AposentadoriaPorIdade[Sexo==2 & IdadeEntradaRPPS>=40 & IdadeEntradaRPPS<=55]=65
  AposentadoriaPorIdade[Sexo==2 & IdadeEntradaRPPS>55]=IdadeEntradaRPPS[Sexo==2 & IdadeEntradaRPPS>55]+10


  #Elegibilidade ? aposentadoria por tempo e idade
  ##Se entrou antes dos 25 anos de idade, aposenta por tempo de contribui??o aos 60, se homem, e aos 55, se mulher.
  AposentadoriaPorTempo[(Sexo==1) & ((IdadeEntradaRPPS-TempoRGPS)<=25)]=55
  AposentadoriaPorTempo[(Sexo==1) & ((IdadeEntradaRPPS-TempoRGPS)>25)]= 30+IdadeEntradaRPPS[(Sexo==1)&((IdadeEntradaRPPS-TempoRGPS)>25)]-TempoRGPS[(Sexo==1)&((IdadeEntradaRPPS-TempoRGPS)>25)]
  AposentadoriaPorTempo[Sexo==1 & IdadeEntradaRPPS>50]=IdadeEntradaRPPS[Sexo==1 & IdadeEntradaRPPS>50]+10
  AposentadoriaPorTempo[Sexo==2 & (IdadeEntradaRPPS-TempoRGPS)<=25]=60
  AposentadoriaPorTempo[(Sexo==2) & ((IdadeEntradaRPPS-TempoRGPS)>25)]= 35+IdadeEntradaRPPS[(Sexo==2)&((IdadeEntradaRPPS-TempoRGPS)>25)]-TempoRGPS[(Sexo==2)&((IdadeEntradaRPPS-TempoRGPS)>25)]
  AposentadoriaPorTempo[Sexo==2 & IdadeEntradaRPPS>55]=IdadeEntradaRPPS[Sexo==2 & IdadeEntradaRPPS>55]+10

  for (i in 1:TamanhoPopulacao) {
    if (AposentadoriaPorTempo[i]<=AposentadoriaPorIdade[i] & AposentadoriaPorTempo[i]<=70 & (AposentadoriaPorTempo[i]-IdadeEntradaRPPS[i]+TempoRGPS[i])>=30) {
      IdadeAposentadoria[i] = AposentadoriaPorTempo[i]
      TipoAposentadoria[i] = 1
    } else if (AposentadoriaPorIdade[i]<=AposentadoriaPorTempo[i] & AposentadoriaPorIdade[i]<=70) {
      IdadeAposentadoria[i] = AposentadoriaPorIdade[i]
      TipoAposentadoria[i] = 2
    } else {
      IdadeAposentadoria[i] = 70
      TipoAposentadoria[i] = 3
    }
  }
  #IdadeAposentadoria=apply(rbind(AposentadoriaPorIdade,AposentadoriaPorTempo,replicate(TamanhoPopulacao,70)),MARGIN=2,FUN=min)
  IdadeAposentadoria[EstadoInicial!=1] = NA
  TipoAposentadoria[EstadoInicial!=1] = NA



#png(paste("Idade de aposentadoria por sexo - ", pop, "Servidores.png"))
#ymin=min(r)
#ymax=max(r)
#plot(IdadeEntradaRPPS[Sexo==1],r[Sexo==1],xlab="Idade de entrada", ylab="Idade Aposentadoria Programada" , col="red",pch=1, ylim=c(ymin, ymax),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
#points(IdadeEntradaRPPS[Sexo==2],r[Sexo==2],col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
#legend(x=18, y = 70, c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"), cex=1.5)
#dev.off()

return(cbind(IdadeAposentadoria,TipoAposentadoria))
}