#' calculaValoresPagamento function
#' @param value
#' @return value
#' @export

#Fun??o principal, que retorna duas matrizes, uma com os so
calculaValoresPagamento = function(DadosServidores,MatrizEstadosServidor,TabelaEsperancaDeVida,TaxaAumentoSalarial,Tempo,Rodadas){
  ##Calcula benef?cio pelas regras do RPPS e pelas regras do RGPS.
  #Estima a compensa??o financeira. Calcula o benef?cio pago sem a compensa??o financeira.
  #Cria fun??es auxiliares
  {
    calculaFatorPrevidenciario = function() {
      ##Calcula fator previdenci?rio para cada indiv?duo
      FatorPrevidenciario= FatorPrevidenciarioFem = FatorPrevidenciarioMasc = matrix(NA,nrow=TamanhoPopulacao,ncol=Rodadas) #Apenas criando uma matriz com as dimens?es Popula??o x Rodadas
      FatorPrevidenciarioFem[At,] = (TempoContribuicaoTotal[At,]+5)*0.31*(1+(IdadeAposentadoria[At]+(TempoContribuicaoTotal[At,]+5)*0.31)/100)/TabelaEsperancaDeVida[IdadeAposentadoria[At],]
      FatorPrevidenciarioMasc[At,]=(TempoContribuicaoTotal[At,])  *0.31*(1+(IdadeAposentadoria[At]+(TempoContribuicaoTotal[At,])*0.31)/100)/TabelaEsperancaDeVida[IdadeAposentadoria[At],]
      #Aqui est? comparando o sexo existente na DadosServidores, e aplicando os fatores previd?nciarios na mesma quantidade e ordem de cada sexo
      FatorPrevidenciario[Sexo==1]=FatorPrevidenciarioFem[Sexo==1]
      FatorPrevidenciario[Sexo==2]=FatorPrevidenciarioMasc[Sexo==2]
      return(FatorPrevidenciario)
    }

    calculaBeneficioAposentadoria=function() {
      ##Calcula o valor que receberia por aposentadoria programada pelas regras do RPPS. Depende do tipo de aposentadoria.

      Beneficio=matrix(NA,nrow=TamanhoPopulacao,ncol=Rodadas)  ##dimen??es: pop e Rodadas
      #Se aposentadoria por idade
      Beneficio[At,][(Sexo[At]==1 & TipoAposentadoria[At]==2 & IdadeEntradaRGPS[At]>30),]= BeneficioIntegral[At][Sexo[At]==1 & TipoAposentadoria[At]==2 & IdadeEntradaRGPS[At]>30]*(TempoContribuicaoTotal[At,][(Sexo[At]==1 & TipoAposentadoria[At]==2 & IdadeEntradaRPPS[At]>30),])/30
      Beneficio[At,][(Sexo[At]==2 & TipoAposentadoria[At]==2 & IdadeEntradaRGPS[At]>30),]= BeneficioIntegral[At][Sexo[At]==2 & TipoAposentadoria[At]==2 & IdadeEntradaRGPS[At]>30]*(TempoContribuicaoTotal[At,][(Sexo[At]==2 & TipoAposentadoria[At]==2 & IdadeEntradaRPPS[At]>30),])/35
      Beneficio[At,][(Sexo[At]==1 & TipoAposentadoria[At]==2 & IdadeEntradaRGPS[At]<=30),]=BeneficioIntegral[At][(Sexo[At]==1 & TipoAposentadoria[At]==2 & IdadeEntradaRGPS[At]<=30)]
      Beneficio[At,][(Sexo[At]==2 & TipoAposentadoria[At]==2 & IdadeEntradaRGPS[At]<=30),]=BeneficioIntegral[At][(Sexo[At]==2 & TipoAposentadoria[At]==2 & IdadeEntradaRGPS[At]<=30)]

      #Se aposentadoria  por idade e tempo
      Beneficio[At,][(Sexo[At]==1 & TipoAposentadoria[At]==1),]= BeneficioIntegral[At][Sexo[At]==1 & TipoAposentadoria[At]==1]
      Beneficio[At,][(Sexo[At]==2 & TipoAposentadoria[At]==1),]= BeneficioIntegral[At][Sexo[At]==2 & TipoAposentadoria[At]==1]

      #Se aposentadoria  compuls?ria
      Beneficio[At,][(Sexo[At]==1 & TipoAposentadoria[At]==3 & IdadeEntradaRGPS[At]<=40),]= BeneficioIntegral[At][(Sexo[At]==1 & TipoAposentadoria[At]==3 & IdadeEntradaRGPS[At]<=40)]
      Beneficio[At,][(Sexo[At]==1 & TipoAposentadoria[At]==3 & IdadeEntradaRGPS[At]>40),]= BeneficioIntegral[At][(Sexo[At]==1 & TipoAposentadoria[At]==3 & IdadeEntradaRGPS[At]>40)]*(IdadeAposentadoria[At][(Sexo[At]==1 & TipoAposentadoria[At]==3 & IdadeEntradaRGPS[At]>40)]-IdadeEntradaRGPS[At][(Sexo[At]==1 & TipoAposentadoria[At]==3 & IdadeEntradaRGPS[At]>40)])/30
      Beneficio[At,][(Sexo[At]==2 & TipoAposentadoria[At]==3 & IdadeEntradaRGPS[At]<=35),]= BeneficioIntegral[At][(Sexo[At]==2 & TipoAposentadoria[At]==3 & IdadeEntradaRGPS[At]<=35)]
      Beneficio[At,][(Sexo[At]==2 & TipoAposentadoria[At]==3 & IdadeEntradaRGPS[At]>35),]= BeneficioIntegral[At][Sexo[At]==2 & TipoAposentadoria[At]==3 & IdadeEntradaRGPS[At]>35]*(IdadeAposentadoria[At][Sexo[At]==2 & TipoAposentadoria[At]==3 & IdadeEntradaRGPS[At]>35]-IdadeEntradaRGPS[At][Sexo[At]==2 & TipoAposentadoria[At]==3 & IdadeEntradaRGPS[At]>35])/35

      #matplot(r,Br)

      return(Beneficio)
    }
  }

  attach(MatrizEstadosServidor)
  attach(DadosServidores)
  TamanhoPopulacao=dim(TempoRPPS)[1] #Quantidade de linhas da TabelaDadosSevidor. Para testes, estou atribuindo o valor 5
  Rodadas = dim(TempoRPPS)[2]
  SalarioAnualServidor=Salario*12  ##Transforma Salario mensal em anula
  IdadeEntradaRGPS = IdadeEntradaRPPS-TempoRGPS #Idade "fict?cia" estimada para o tempo de entrada no RGPS caso ele tivesse contribu?do ininterruptamente desde que come?ou a trabalhar
  TempoContribuicaoTotal = TempoRPPS + TempoRGPS
  TempoAtivo = TempoRPPS + IdadeEntradaRPPS - Idade  #Tempo total de atividade do servidor a partir do momento atual
  At = which(!is.na(IdadeAposentadoria)) #Indices somente dos servidores ativos (contrubuintes)
  Inat = which(is.na(IdadeAposentadoria)) #Indices somente dos servidores inativos e pensionistas (benefici?rios)
  TempoAtivo[Inat,] = 0
  FatorPrevidenciario = calculaFatorPrevidenciario()

  Beneficios1RPPS=Beneficios2RPPS=Beneficios1RGPS=Beneficios2RGPS=array(data = 0,dim=c(TamanhoPopulacao,Rodadas))  ## Valor do benefc?cio pelas normas do RGPS
  BeneficioIntegral = Salario*((1+TaxaAumentoSalarial)^(IdadeAposentadoria-Idade)-(1+TaxaAumentoSalarial)^(0.2*IdadeAposentadoria+0.8*IdadeEntradaRGPS-Idade))/(0.8*(IdadeAposentadoria-IdadeEntradaRGPS)*TaxaAumentoSalarial)
  BeneficioAposentadoriaAnual=12*calculaBeneficioAposentadoria()
  BeneficioIntegralAnual = matrix(BeneficioIntegral,nrow=TamanhoPopulacao, ncol=Rodadas)*12
  #Benef?cio 1 RGPS:
  ## Se ? inv?lido, recebe benef?cio integral calculado ? idade de invalidez
  ##Se ? pensionista, benef?cio = integral, se ativo, ou ?ltimo benef?cio de aposentadoria

  Beneficios1RGPS[(MotivoSaiAtivo==2 | MotivoSaiAtivo==4 | MotivoSaiAtivo==5)]=BeneficioIntegralAnual[(MotivoSaiAtivo==2 | MotivoSaiAtivo==4 | MotivoSaiAtivo==5)]
  ## Se se aposentou, benef?cio = m?dia 80% maiores remunera??o * fator previdenci?rio
  Beneficios1RGPS[MotivoSaiAtivo==3]=BeneficioIntegralAnual[MotivoSaiAtivo==3]*FatorPrevidenciario[MotivoSaiAtivo==3]

  #Benef?cio 2 RGPS
  ##Se ? pensionista, benef?cio = integral, se ativo, ou ?ltimo benef?cio de aposentadoria
  Beneficios2RGPS[MotivoSaiBeneficio1==4 | MotivoSaiBeneficio1==5]=Beneficios1RGPS[MotivoSaiBeneficio1==4 | MotivoSaiBeneficio1==5]

  #Benef?cio RPPS 1:
  ## Se ? inv?lido, recebe benef?cio integral calculado ? idade de invalidez
  ##Se ? pensionista, benef?cio = integral, se ativo, ou ?ltimo benef?cio de aposentadoria
  Beneficios1RPPS[MotivoSaiAtivo==2]=BeneficioIntegralAnual[MotivoSaiAtivo==2]
  ## Se se aposentou, benef?cio = m?dia 80% maiores remunera??o * fator previdenci?rio
  Beneficios1RPPS[MotivoSaiAtivo==3]=BeneficioAposentadoriaAnual[MotivoSaiAtivo==3]
  #Benef?cio RPPS 2
  ##Se ? pensionista, benef?cio = ?ltimo Salario da ativa, se ativo, ou ?ltimo benef?cio de aposentadoria

  SalarioAnualServidorAjustado=SalarioAnualServidor*(1+TaxaAumentoSalarial)^TempoContribuicaoTotal
  Beneficios1RPPS[MotivoSaiAtivo==4 | MotivoSaiAtivo==5] =SalarioAnualServidorAjustado[MotivoSaiAtivo==4 | MotivoSaiAtivo==5]
  Beneficios2RPPS[MotivoSaiBeneficio1==4 | MotivoSaiBeneficio1==5] =Beneficios1RPPS[MotivoSaiBeneficio1==4 | MotivoSaiBeneficio1==5]

  Beneficio1Minimo = pmin(Beneficios1RGPS,Beneficios1RPPS)
  Beneficio2Minimo = pmin(Beneficios2RGPS,Beneficios2RPPS)

  SalarioAnualServidorAjustado=(1+TaxaAumentoSalarial)^seq(0,Tempo-1,1)
  RecebendoBeneficio1 = RecebendoBeneficio2 = rep(0,TamanhoPopulacao)
  #d?vida: os benef?cios recebidos pelos os que j? come?aram inativos ser?o sempre constantes, tanto para eles quanto para o segundo benefici?rio?
  SalarioContribuicao = MatrizBeneficio1RGPS = MatrizBeneficio1RPPS = MatrizBeneficio2RGPS = MatrizBeneficio2RPPS = BeneficioInativos = MatrizBeneficio1Compensacao = MatrizBeneficio2Compensacao = MatrizCompensacaoTotal = array(data=0,dim=c(Tempo,Rodadas))
  for (i in 1:Rodadas) {
    for (j in 1:Tempo) {
      RecebendoBeneficio1 = ((TempoAtivo[,i]<j)&((TempoAtivo[,i]+TempoBeneficio1[,i])>=j)) #Se TempoAtivo < TempoAtual <= TempoAtivo+TempoBeneficio1, ent?o est? recebendo beneficio no tempo atual
      RecebendoBeneficio2 = (((TempoAtivo[,i]+TempoBeneficio1[,i])<j)&((TempoAtivo[,i]+TempoBeneficio1[,i]+TempoBeneficio2[,i])>=j))
      SalarioContribuicao[j,i] = sum(((TempoAtivo[At,i]>=j)*12*Salario[At])*SalarioAnualServidorAjustado[j])
      #MatrizBeneficio1RGPS[j,i] = sum(RecebendoBeneficio1[At]*Beneficios1RGPS[At,i])
      MatrizBeneficio1RPPS[j,i] = sum(RecebendoBeneficio1[At]*Beneficios1RPPS[At,i]) #+ sum(RecebendoBeneficio1[Inat]*Salario[Inat])
      #MatrizBeneficio2RGPS[j,i] = sum(RecebendoBeneficio2[At]*Beneficios2RGPS[At,i])
      MatrizBeneficio2RPPS[j,i] = sum(RecebendoBeneficio2[At]*Beneficios2RPPS[At,i]) #+ sum(RecebendoBeneficio2[Inat]*Salario[Inat])
      MatrizBeneficio1Compensacao[j,i] = sum(Beneficio1Minimo[At,i]*(RecebendoBeneficio1[At]*(TempoRGPS[At])/(TempoContribuicaoTotal[At,i])))
      MatrizBeneficio2Compensacao[j,i] = sum(Beneficio2Minimo[At,i]*(RecebendoBeneficio2[At]*(TempoRGPS[At])/(TempoContribuicaoTotal[At,i])))
      BeneficioInativos[j,i] = sum(RecebendoBeneficio1[Inat]*Salario[Inat])*12 + sum(RecebendoBeneficio2[Inat]*Salario[Inat]*12) #Beneficios pago aos indiv?duos que no in?cio da simula??o encontravam-se aposentados ou pensionistas
    }
  }

  #RGPS = MatrizBeneficio1RGPS+MatrizBeneficio2RGPS
  RPPS = MatrizBeneficio1RPPS+MatrizBeneficio2RPPS
  MatrizCompensacaoTotal = MatrizBeneficio1Compensacao+MatrizBeneficio2Compensacao

  BeneficioPagoPlano = (MatrizBeneficio1RPPS+MatrizBeneficio2RPPS+BeneficioInativos)-MatrizCompensacaoTotal


  ##Gr?ficos
  #ymax=max(BeneficioPagoPlano,RGPS,RPPS,MatrizCompensacaoTotal)
  #png(paste("Benef?cios RGPS - ", pop,"Servidores.png"))
  #matplot(MatrizBeneficioRGPSTotal/1000000, type = "l",ylab="R$ Milh?es", main="Beneficios pela regra do RGPS", xlab="Tempo" , ylim=c(0,ymax/1000000),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
  #dev.off()
  #png(paste("Benef?cios RPPS - ", pop,"Servidores.png"))
  #matplot(MatrizBeneficioRPPSTotal/1000000, type = "l",ylab="R$ Milh?es", main="Beneficios pela regra do RPPS", xlab="Tempo" , ylim=c(0,ymax/1000000),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
  #dev.off()
  #png(paste("Compensa??o Financeira - ", pop,"Servidores.png"))
  #matplot(MatrizCompensacaoTotal/1000000, type = "l",ylab="R$ Milh?es", main="Compensa??o Financeira", xlab="Tempo" , ylim=c(0,ymax/1000000),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
  #dev.off()
  #png(paste("Benef?cios pagos pelo plano ", pop,"Servidores.png"))
  #matplot(BeneficioPagoPlano/1000000, type = "l",ylab="R$ Milh?es", main="Beneficios pagos pelo plano", xlab="Tempo" , ylim=c(0,ymax/1000000),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
  #dev.off()
  #png(paste("Salario de Contribui??o - ", pop, "Servidores.png"))
  #matplot(SalarioContribuicao, type = "l",ylab="R$ Milh?es", main="Salario de Contribui??o Total", xlab="Tempo",cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
  #dev.off()

  ##png(paste("Percentis Benef?cios RGPS - ", TamanhoPopulacao,"Servidores.png"))
  ##geraGraficoBeneficiosPagos(RGPS/1000000, 0.75,0, ymax/1000000, paste("Benef?cios RGPS -", TamanhoPopulacao, "Servidores"), "R$ Milh?es","topleft")
  ##dev.off()
  ##png(paste("Percentis Benef?cios RPPS - ", TamanhoPopulacao,"Servidores.png"))
  ##geraGraficoBeneficiosPagos(RPPS/1000000, 0.75, 0, ymax/1000000, paste("Benef?cios RPPS -", TamanhoPopulacao, "Servidores"), "R$ Milh?es","topleft")
  ##dev.off()
  ##png(paste("Percentis Compensa??o Financeira - ", TamanhoPopulacao,"Servidores.png"))
  ##geraGraficoBeneficiosPagos(MatrizCompensacaoTotal/1000000,  0.75,0, ymax/1000000, paste("Compensa??o Financeira -", TamanhoPopulacao, "Servidores"), "R$ Milh?es","topleft")
  ##dev.off()
  ##png(paste("Percentis Benef?cios pagos pelo plano ", TamanhoPopulacao,"Servidores.png"))
  ##geraGraficoBeneficiosPagos(BeneficioPagoPlano/1000000,  0.75,0, ymax/1000000, paste("Benef?cios Pagos -", TamanhoPopulacao, "Servidores"), "R$ Milh?es","topleft")
  ##dev.off()
  ##png(paste("Percentis Salarios de Contribui??o ", TamanhoPopulacao,"Servidores.png"))
  ##geraGraficoBeneficiosPagos(SalarioContribuicao/1000000,  0.75,0, max(SalarioContribuicao)/1000000, paste("Salario de Contribui??o -", TamanhoPopulacao, "Servidores"), "R$ Milh?es","topright")
  ##dev.off()
  detach(MatrizEstadosServidor)
  detach(DadosServidores)
  return(list(BeneficioPagoPlano,SalarioContribuicao))
}


