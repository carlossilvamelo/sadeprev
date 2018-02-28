#' estimaSeDeixouDependente function
#' @param value
#' @return value
#' @export

estimaSeDeixouDependente=function (MotivoSaida,SexoQuemMorreu,IdadeQuemMorreu,ProbFamilia,Rodadas) {
####Avalia se deixou c?njuge ou filho menor de 21 anos


TamanhoPopulacao=dim(SexoQuemMorreu)[1]
IdadeBeneficiario=IdadeQuemMorreu
SexoBeneficiario=SexoQuemMorreu

ProbTerConjuge = matrix(NA,nrow=TamanhoPopulacao,ncol=Rodadas)
ProbTerConjuge[SexoQuemMorreu==1] = ProbFamilia$P.TerConj.F[IdadeQuemMorreu[SexoQuemMorreu==1]]
ProbTerConjuge[SexoQuemMorreu==2] = ProbFamilia$P.TerConj.M[IdadeQuemMorreu[SexoQuemMorreu==2]]
TemConjuge = matrix(rbinom(length(ProbTerConjuge),size=1,prob=ProbTerConjuge),nrow=TamanhoPopulacao,ncol=Rodadas)
rm(ProbTerConjuge)

ProbTerFilho = matrix(NA,nrow=TamanhoPopulacao,ncol=Rodadas)
ProbTerFilho[SexoQuemMorreu==1] = ProbFamilia$P.TerFilho.F[IdadeQuemMorreu[SexoQuemMorreu==1]]
ProbTerFilho[SexoQuemMorreu==2] = ProbFamilia$P.TerFilho.M[IdadeQuemMorreu[SexoQuemMorreu==2]]
TemFilho = matrix(rbinom(length(ProbTerFilho),size=1,prob=ProbTerFilho),nrow=TamanhoPopulacao,ncol=Rodadas)
rm(ProbTerFilho)


#########PARA MULHER
#### Testa se deixa c?njuge
IndicesTemConjuge = which(MotivoSaida==7 & SexoQuemMorreu==1 & TemConjuge)
MotivoSaida[IndicesTemConjuge]=5

nIndicesTemFilho = length(IndicesTemConjuge) ##N?mero de idades de conjuges a serem calculadas
EstimacaoIdade = rnorm(nIndicesTemFilho,0, sd =6.9) ##Estima idade do c?njuge, com idade m?nima de 18 anos
Media = 3.45+0.84*IdadeQuemMorreu[IndicesTemConjuge]+3.61+0.07*IdadeQuemMorreu[IndicesTemConjuge]
IdadeBeneficiario[IndicesTemConjuge] =floor(EstimacaoIdade+Media)  ##arredonda para baixo, indicando idade completa
SexoBeneficiario[IndicesTemConjuge]=2

#### Testa se deixa filho
IndicesTemFilho = which(SexoQuemMorreu==1 & MotivoSaida==7 & TemFilho)
MotivoSaida[IndicesTemFilho]=4  ##Morreu e deixou filho menor de 21 anos

nIndicesTemFilho = length(IndicesTemFilho)
EstimacaoIdade = rnorm(nIndicesTemFilho,0,4.773)
Media=-9.314+0.443*IdadeQuemMorreu[IndicesTemFilho]+1.782
IdadeBeneficiario[IndicesTemFilho]=floor(EstimacaoIdade+Media)  ##Estima idade do filho. Idade m?nima=0.
SexoBeneficiario[IndicesTemFilho]=1  ##Assume tabela de vida de menor mortalidade
MotivoSaida[IndicesTemFilho * (IdadeBeneficiario[IndicesTemFilho]>=21)] =6 ##Se filho tem 21 anos ou mais, n?o ? benefici?rio.

#########PARA HOMEM
## Verifico deixou conjuge
IndicesTemConjuge = which(MotivoSaida==7 & SexoQuemMorreu==2 & TemConjuge)
MotivoSaida[IndicesTemConjuge]=5

nIndicesTemFilho = length(IndicesTemConjuge) ##N?mero de idades de conjuges a serem calculadas
EstimacaoIdade=rnorm(nIndicesTemFilho,0, sd =6.9) ##Estima idade do c?njuge, com idade m?nima de 18 anos
Media= 3.45+0.84*IdadeQuemMorreu[IndicesTemConjuge]+3.61+0.07*IdadeQuemMorreu[IndicesTemConjuge]
IdadeBeneficiario[IndicesTemConjuge] =floor(EstimacaoIdade+Media)  ##arredonda para baixo, indicando idade completa
SexoBeneficiario[IndicesTemConjuge]=1



##Verifico deixou filho
IndicesTemFilho = which(SexoQuemMorreu==2 & MotivoSaida==7 & TemFilho)
MotivoSaida[IndicesTemFilho]=4  ##Morreu e deixou filho menor de 21 anos

nIndicesTemFilho=length(IndicesTemFilho)
EstimacaoIdade= rnorm(nIndicesTemFilho,0,4.773)
Media=-9.314+0.443*IdadeQuemMorreu[IndicesTemFilho]+1.782
IdadeBeneficiario[IndicesTemFilho]=floor(EstimacaoIdade+Media)  ##Estima idade do filho. Idade m?nima=0.
SexoBeneficiario[IndicesTemFilho]=1  ##Assume tabela de vida de menor mortalidade
MotivoSaida[IndicesTemFilho * (IdadeBeneficiario[IndicesTemFilho]>=21)] =6 ##Se filho tem 21 anos ou mais, n?o ? benefici?rio.


##AJUSTES FINAIS
IdadeBeneficiario[MotivoSaida==5 & IdadeBeneficiario<18]=18
IdadeBeneficiario[MotivoSaida==4 & IdadeBeneficiario<0]=0
IdadeBeneficiario[IdadeBeneficiario==0] = 1
MotivoSaida[MotivoSaida==4 & IdadeBeneficiario>=21]=6 ##Se filho tem 21 anos ou mais, n?o ? benefici?rio.
MotivoSaida[MotivoSaida==7]=6  ##Se n?o tem filho nem c?njuge, n?o deixou dependente
IdadeBeneficiario[IdadeBeneficiario>=110]=109    ##Ajustes idades dos benefici?rios
IdadeBeneficiario[MotivoSaida==6]=109        ##Se n?o existe benefici?rio, n?o existe idade do benefici?rio


return(list(MotivoSaida,SexoBeneficiario,IdadeBeneficiario))

}