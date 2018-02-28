#' organizaDadosRPPS function
#' @param value
#' @return value
#' @export

organizaDadosRPPS = function (dadosEntrada) {
  #Calcula a idade de entrada no RPPS
  TempoRPPS = dadosEntrada[[1]]$TempoServicoPublico
  TempoRPPS[TempoRPPS>dadosEntrada[[2]]$TempoCriacaoRPPS] = dadosEntrada[[2]]$TempoCriacaoRPPS
  TempoRGPSServicoPublico = dadosEntrada[[1]]$TempoServicoPublico - TempoRPPS
  IdadeEntradaRPPS = dadosEntrada[[1]]$Idade-TempoRPPS

  #Anula dados do tempo de rgps e entrada do rpps para benefÃ­ciarios
  TempoRGPS = dadosEntrada[[1]]$TempoRGPS
  TempoRPPS = TempoRGPS + TempoRGPSServicoPublico
  TempoRGPS[dadosEntrada[[1]]$EstadoInicial !=1 ] = IdadeEntradaRPPS[dadosEntrada[[1]]$EstadoInicial != 1] = NA

  DadosServidores = data.frame(dadosEntrada[[1]]$Idade,dadosEntrada[[1]]$Sexo,dadosEntrada[[1]]$Salario,
                               dadosEntrada[[1]]$EstadoInicial,TempoRGPS,IdadeEntradaRPPS)
  names(DadosServidores) = c("Idade", "Sexo", "Salario", "EstadoInicial", "TempoRGPS", "IdadeEntradaRPPS")

  #Soma a aliquota do servidor com a aliquota do ente federativo
  Aliquota = dadosEntrada[[2]]$AliquotaServidor+dadosEntrada[[2]]$AliquotaEnte

  #Fundo atual do RPPS
  FundoAtual = dadosEntrada[[2]]$FundoAtual



  return(list(DadosServidores=DadosServidores,Aliquota=Aliquota,FundoAtual = FundoAtual))
}