#' estimaDadosSaiAtivo function
#' @param value
#' @return value
#' @export

estimaDadosSaiAtivo=function(TempoRPPS,DadosServidores,TMD,ProbFamilia,Rodadas) {
##Motivo pelo qual saiu da situa??o de ativo.
  TamanhoPopulacao=length(DadosServidores[[1]])

  Idade = DadosServidores$Idade
  Sexo = DadosServidores$Sexo
  IdadeAposentadoria = DadosServidores$IdadeAposentadoria
  EstadoInicial = DadosServidores$EstadoInicial
  IndicesAtivo = which(DadosServidores$EstadoInicial==1)
  IndicesInativo = (1:TamanhoPopulacao)[-IndicesAtivo]
  nIndicesAtivo = length(IndicesAtivo)

  IdadeAtivos = Idade[IndicesAtivo]
  SexoAtivos = Sexo[IndicesAtivo]
  IdadeAposentadoriaAtivos = IdadeAposentadoria[IndicesAtivo]
  TempoRPPSAtivos = TempoRPPS[IndicesAtivo,]
  MotivoSaiAtivo = IdadeBeneficiario1 = SexoBeneficiario1 = matrix(-1,nrow=TamanhoPopulacao,ncol=Rodadas)
  Limite = matrix(-1, nrow=nIndicesAtivo,ncol=Rodadas)
  MotivoSaiAtivo[IndicesAtivo,] = 2

  Aleatorio=matrix(runif(nIndicesAtivo*Rodadas, min = 0, max = 1), ncol=Rodadas) ##Gera n?meros aleat?rios
  IdadeSaiAtivo=IdadeAtivos+TempoRPPSAtivos


##O limite ? a raz?o entre o n?mero de sa?das por mortes em rela??o ?s sa?das totais naquela idade
#Mulheres
a=(TMD$lxMorteTMD.F[(IdadeSaiAtivo)]-TMD$lxMorteTMD.F[IdadeSaiAtivo+1])/(TMD$lxTotalTMD.F[IdadeSaiAtivo]-TMD$lxTotalTMD.F[IdadeSaiAtivo+1])
Limite[SexoAtivos==1]=a[SexoAtivos==1]
#Homens
b=(TMD$lxMorteTMD.M[(IdadeSaiAtivo)]-TMD$lxMorteTMD.M[IdadeSaiAtivo+1])/(TMD$lxTotalTMD.M[IdadeSaiAtivo]-TMD$lxTotalTMD.M[IdadeSaiAtivo+1])
Limite[SexoAtivos==2]=b[SexoAtivos==2]

MotivoSaiAtivo[IndicesAtivo,][Aleatorio<=Limite]=7   ##Morreu
MotivoSaiAtivo[IndicesAtivo,][IdadeAtivos+TempoRPPSAtivos-IdadeAposentadoriaAtivos>=0]=3  ##Se idade que saiu ? maior que idade de aposentadoria, assume que aposentou




  IdadeSaiAtivo=IdadeAtivos+TempoRPPSAtivos

  SexoSaiAtivo=matrix(rep(SexoAtivos, Rodadas), ncol=Rodadas)

####Avalia se deixou c?njuge ou filho menor de 21 anos
Beneficiario1 = estimaSeDeixouDependente(MotivoSaiAtivo[IndicesAtivo,],SexoSaiAtivo,IdadeSaiAtivo,ProbFamilia,Rodadas)
MotivoSaiAtivo[IndicesAtivo,]=Beneficiario1[[1]]
SexoBeneficiario1[IndicesAtivo,]=Beneficiario1[[2]]
IdadeBeneficiario1[IndicesAtivo,]=Beneficiario1[[3]]

MotivoSaiAtivo[IndicesInativo,] = matrix(rep(EstadoInicial[IndicesInativo],Rodadas),ncol=Rodadas)
SexoBeneficiario1[IndicesInativo,] = matrix(rep(Sexo[IndicesInativo],Rodadas),ncol=Rodadas)
IdadeBeneficiario1[IndicesInativo,] = matrix(rep(Idade[IndicesInativo],Rodadas),ncol=Rodadas)

return(list(MotivoSaiAtivo,SexoBeneficiario1,IdadeBeneficiario1))
}

