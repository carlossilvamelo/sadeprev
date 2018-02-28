#' resumeEstadoServidor function
#' @param value
#' @return value
#' @export

resumeEstadoServidor = function (MatrizEstadosServidor,DadosServidores,Tempo){ #Faz a contagem de estados de servidores para cada ano

  attach(MatrizEstadosServidor)
  attach(DadosServidores)
  NumeroPopulacao = nrow(MotivoSaiAtivo)
  Rodadas = ncol(MotivoSaiAtivo)
  TempoAtivo = TempoRPPS + IdadeEntradaRPPS - Idade #tempo de atividade a partir do momento atual
  Ativo = Beneficiario1 = Beneficiario2 = 0
  TempoAtivo[which(is.na(IdadeAposentadoria)),] = 0

  #Cada elemento da lista representa, respectivamente, o total de 1=ativos, 2=inv?lidos, 3=aposentados, 4=filhos recebendo benef?cio, 5=c?njuges recebendo benef?cios, 6=mortos sem deixar benefici?rio
  MatrizResumo = replicate(6,list(matrix(0,Tempo,Rodadas)))
  for(r in 1:Rodadas) {
    for (t in 1:Tempo) {
      Ativo = TempoAtivo[,r]>=t #Se o tempo de atividade do servidor for maior ou igual ao tempo atual, significa que ele ainda est? ativo
      Beneficiario1 = ((TempoAtivo[,r]+TempoBeneficio1[,r])>=t)&TempoAtivo[,r]<t
      Beneficiario2 = ((TempoAtivo[,r]+TempoBeneficio1[,r]+TempoBeneficio2[,r])>=t)&((TempoAtivo[,r]+TempoBeneficio1[,r])<t)
      for (k in 2:5) {
        MatrizResumo[[k]][t,r] = sum(MotivoSaiAtivo[Beneficiario1,r]==k)
        MatrizResumo[[k]][t,r] = MatrizResumo[[k]][t,r] + sum(MotivoSaiBeneficio1[Beneficiario2,r]==k)
      }
      MatrizResumo[[1]][t,r] = sum(Ativo)
      MatrizResumo[[6]][t,r] = sum((Ativo+Beneficiario1+Beneficiario2)==0)
      #Se no tempo atual n?o h? mais ativo nem qualquer benefici?rio, a partir desse ano em diante todos os mortos n?o deixaram benefici?rios
      if (MatrizResumo[[6]][t,r]==NumeroPopulacao) {
        MatrizResumo[[6]][t:Tempo,r] = NumeroPopulacao
        break
      }
    }
  }

  names(MatrizResumo) = c('Ativos','Invalidos','Aposentados','FilhosBeneficiarios','ConjugesBeneficiarios','MortosSemBeneficario')
  detach(MatrizEstadosServidor)
  detach(DadosServidores)
  return(MatrizResumo)
}