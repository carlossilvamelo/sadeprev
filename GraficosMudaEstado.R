#' GraficosMudaEstado function
#' @param value
#' @return value
#' @export

GraficosMudaEstado = function(DadosServidores,EstadosServidor){

  attach(DadosServidores)
  attach(EstadosServidor)
  At = which(EstadoInicial==1) #Indices somente com ativos
  Idade = Idade[At]
  Sexo = Sexo[At]
  TempoRGPS = TempoRGPS[At]
  IdadeEntradaRPPS = IdadeEntradaRPPS[At]
  IdadeAposentadoria = IdadeAposentadoria[At]
  TempoRPPS = TempoRPPS[At]
  MotivoSaiAtivo = MotivoSaiAtivo[At]
  TamanhoPopulacao = length(At)

  ##Idade de entrada
  png(width = 800,height = 800,paste("Idade de entrada no RPPS - ", TamanhoPopulacao,"Servidores.png"))
  ymax=max(IdadeEntradaRPPS)
  plot(Idade[Sexo==1],IdadeEntradaRPPS[Sexo==1],main = paste("Idade de entrada no RPPS - ", TamanhoPopulacao,"Servidores"),xlab="Idade atual", ylab="Idade de entrada", col="red",pch=1, ylim=c(18,ymax),cex.axis=1.5,cex.lab=1.5, cex=1,cex.main=2)
  points(Idade[Sexo==2],IdadeEntradaRPPS[Sexo==2],col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1,cex.main=2)
  legend("topleft", c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"),cex =1.5)
  dev.off()

  png(width = 800,height = 800,paste("Piramide Idade de Entrada ",TamanhoPopulacao,"servidores.png"))
  ParaPiramide=data.frame(Idade=Idade,Sexo=Sexo)
  ParaPiramide$Idade=IdadeEntradaRPPS
  piramide(ParaPiramide,titulo=(paste0(TamanhoPopulacao," Servidores")))
  dev.off()


  ##Idade de Aposentadoria
  png(width = 800,height = 800,paste("Idade de aposentadoria por sexo - ", TamanhoPopulacao, "Servidores.png"))
  ymin=min(IdadeAposentadoria)
  ymax=max(IdadeAposentadoria)
  plot(IdadeEntradaRPPS[Sexo==1],IdadeAposentadoria[Sexo==1],main = paste("Idade de aposentadoria por sexo - ", TamanhoPopulacao, "Servidores"),xlab="Idade de entrada", ylab="Idade Aposentadoria Programada" , col="red",pch=1, ylim=c(ymin, ymax),cex.axis=1.5,cex.lab=1.5, cex=1,cex.main=2,xlim=c(min(IdadeEntradaRPPS),max(IdadeEntradaRPPS)))
  points(IdadeEntradaRPPS[Sexo==2],IdadeAposentadoria[Sexo==2],col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1,cex.main=2)
  legend("topleft", c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"), cex=1)
  dev.off()


  ##Tempo Ativo
  TempoAtivo = TempoRPPS+TempoRGPS
  png(width = 800,height = 800,paste("Tempo Ativo Total x Idade - ",TamanhoPopulacao, "Servidores .png"))
  plot(Idade[Sexo==1],TempoAtivo[Sexo==1],main= paste("Tempo Ativo Total x Idade - ",TamanhoPopulacao, "Servidores"),xlab="Idade atual",ylab="Tempo como ativo", col="red",pch=1,  ylim=c(min(TempoAtivo), max(TempoAtivo)),cex.axis=1.5,cex.lab=1.5, cex=1,cex.main=2)
  points(Idade[Sexo==2],TempoAtivo[Sexo==2], col="blue", pch=0,cex.axis=1.5,cex.lab=1.5, cex=1,cex.main=2)
  legend("topright", c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"), cex=1)
  dev.off()

  ##Motivo Sai da Atividade
  if (length(table(MotivoSaiAtivo[MotivoSaiAtivo!=6])/sum(MotivoSaiAtivo!=6))==2) legenda=c('Inv.', 'Apos.')
  if (length(table(MotivoSaiAtivo[MotivoSaiAtivo!=6])/sum(MotivoSaiAtivo!=6))==3) legenda=c('Inv.', 'Apos.', 'Conj.')
  if (length(table(MotivoSaiAtivo[MotivoSaiAtivo!=6])/sum(MotivoSaiAtivo!=6))==4) legenda=c('Inv.', 'Apos.', 'Filho', 'Conj.')

  png(width = 800,height = 800,paste("Motivo que levou o servidor a deixar de ser Ativo - ", TamanhoPopulacao, "Servidores .png") )
  barplot(table(MotivoSaiAtivo[MotivoSaiAtivo!=6])/sum(MotivoSaiAtivo!=6), main =paste("MotivoBeneficiario 1 - ", TamanhoPopulacao, "Servidores"),xlab="Estado",ylab="Frequencia" , names=legenda,cex.axis=1.5,cex.lab=1.5, cex=1,cex.main=1.5)
  dev.off()

  ##Dependentes dos ativos mortos
  png(width = 800,height = 800,paste("Dependentes dos ativos mortos - ", TamanhoPopulacao, "Servidores .png") )
  M=table(MotivoSaiAtivo[MotivoSaiAtivo>3])/sum(MotivoSaiAtivo>3)
  if (length(M)==3) nomes=c("Filhos", "Conjuges", "Sem Dep.")
  if (length(M)==2) nomes=c("Conjuges", "Sem Dep.")
  barplot(M,names=nomes, ylab="Frequencia",main=paste("Dependentes dos ativos mortos - ", TamanhoPopulacao, "Servidores"), xlab="Dependentes dos ativos mortos",cex.axis=1.5,cex.lab=1.5, cex=1,cex.main=1.5)
  dev.off()

  detach(DadosServidores)
  detach(EstadosServidor)
}