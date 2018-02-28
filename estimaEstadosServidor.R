#' estimaEstadosServidor function
#' @param value
#' @return value
#' @export

estimaEstadosServidor = function(DadosServidores,TMD,TabuasMorte,ProbFamilia,Rodadas){
  ###Estima tempo at? sa?da de cada status e indica status para o qual mudou

  ###Estados observados no decorrer do tempo
  ##1 = Ativo; 2 = Inv?lido, 3 - aposentado, 4=Filho benefici?rio, 5=Conjuge benefici?rio, 6=Morto sem sependentes,

  ## Estima idade em que poderia se aposentar.
  #IdadeAposentadoria = estimaIdadeAposentadoria(DadosServidores)    ##Guarda idades de aposentadoria para cada rodada
  ##Estima tempo na atividade
  TempoRPPS = estimaTempoRPPS(DadosServidores,TMD, Rodadas)
  DadosSaiAtivo =  estimaDadosSaiAtivo(TempoRPPS,DadosServidores,TMD,ProbFamilia,Rodadas)

  MotivoSaiAtivo=DadosSaiAtivo[[1]]
  SexoBeneficiario1=DadosSaiAtivo[[2]]
  IdadeBeneficiario1=DadosSaiAtivo[[3]]

  #Dura??o do benef?cio do primeiro benefici?rio
  TempoBeneficiario1=estimaTempoBeneficio1(DadosSaiAtivo,DadosServidores,ProbFamilia,TabuasMorte, Rodadas)
  DadosFimBeneficio1=estimaDadosFimBeneficio1(TempoBeneficiario1,IdadeBeneficiario1,SexoBeneficiario1,MotivoSaiAtivo,ProbFamilia,Rodadas)
  MotivoSaiBeneficiario1=DadosFimBeneficio1[[1]]
  IdadeBeneficiario2=DadosFimBeneficio1[[2]]
  SexoBeneficiario2=DadosFimBeneficio1[[3]]

  #Dura??o do benef?cio do segundo benefici?rio
  TempoBeneficiario2=estimaTempoBeneficio2(IdadeBeneficiario2, SexoBeneficiario2,MotivoSaiBeneficiario1,TabuasMorte, Rodadas)

  #IdadeEntrada = matrix(rep(DadosServidores$IdadeEntradaRPPS,Rodadas), ncol=Rodadas)
  #IdadeAposentadoria = matrix(rep(IdadeAposentadoria,Rodadas), ncol=Rodadas)

  #Ajuste para preencher com NA onde tiver -1, significando que ? um benefici?rio desde o in?cio da simula??o
  #IdadeEntrada[IdadeEntrada==-1] = NA
  #IdadeAposentadoria[IdadeAposentadoria==-1] = NA
  TempoRPPS[TempoRPPS==-1] = NA

  MatrizEstadosServidor=list(TempoRPPS,MotivoSaiAtivo,TempoBeneficiario1,TempoBeneficiario2,MotivoSaiBeneficiario1)
  names(MatrizEstadosServidor)=c('TempoRPPS','MotivoSaiAtivo','TempoBeneficio1','TempoBeneficio2','MotivoSaiBeneficio1')
  return (MatrizEstadosServidor)

}