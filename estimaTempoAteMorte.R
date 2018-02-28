#' estimaTempoAteMorte function
#' @param value
#' @return value
#' @export

####Fun??es Fidel
###FUN??O PRINCIPAL

estimaTempoAteMorte=function(MatIdade,MatSexo,lxM,lxF){


  Idades=as.vector(MatIdade)     #l? como vetor
        Sexo=as.vector(MatSexo)   #l? como vetor
        VetorTx=rep(NA,length(Idades))              ##Vetor onde ser?o armazenados os resultados
        IndSexoF = which(Sexo == 1) #Identifica as posi??es na matriz em que h? Mulheres
  IndSexoM = which(Sexo == 2) #Identifica as posi??es na matriz em que h? Homens

  #Para sexo masculino
        IdadesHomens=Idades[IndSexoM]# Idade dos homens Novo vetor
        IdadesHomensUnicas=unique(IdadesHomens)      ##Identifica valores diferentes de idades - fun??o elaborada pelo autor
        nIdadesHomensUnicas=length(IdadesHomensUnicas)        ##Identifica quantos s?o os valores diferentes
        ResultadoHomens=as.vector(array(NA,dim=c(length(IdadesHomens),1))) ##Cria vetor para guardar resultados

        for(i in 1:nIdadesHomensUnicas){
        IndiceIdadesUnicas = which(IdadesHomens == IdadesHomensUnicas[i])      ##identifica posi??es do vetor que t?m a mesma idade
        nIndiceIdadesUnicas=length(IndiceIdadesUnicas)                   ##Identifica o n?mero de posi??es do vetor que t?m a mesma idade
    if(IdadesHomensUnicas[i]>=(length(lxM)-1)){       ##S? estima para idades abaixo da idade limite da tabela de vida. Diminui em 1 porque a tabela de vida come?a na idade 0.
                        ResultadoHomens[IndiceIdadesUnicas]=rep(0,nIndiceIdadesUnicas)         ##Se for maior que o limite da t?bua de vida, atribui 0.
                }else{                           ##Se for menor, calcula.
                        lxNovo=lxM[(IdadesHomensUnicas[i]+1):(length(lxM))]   ##Cria novo vetor de lx apenas com valores acima da idade i
                        ResultadoHomens[IndiceIdadesUnicas]=EstimaTx(nIndiceIdadesUnicas,lxNovo)          ##fun??o elaborada pelo autor
                }
        }

#Para sexo feminino
        IdadesMulheres=Idades[IndSexoF]# Idade das mulheres Novo vetor
  IdadesUnicasMulheres=unique(IdadesMulheres)      ##Identifica valores diferentes de idades
        nIdadesUnicasMulheres=length(IdadesUnicasMulheres)        ##Identifica quantos s?o os valores diferentes
        ResultadoMulheres=as.vector(array(NA,dim=c(length(IdadesMulheres),1))) ##Cria vetor para guardar resultados

        for(i in 1:nIdadesUnicasMulheres){
        IndiceIdadesUnicas = which(IdadesMulheres %in% IdadesUnicasMulheres[i])      ##identifica posi??es do vetor que t?m a mesma idade
        nIndiceIdadesUnicas=length(IndiceIdadesUnicas)                   ##Identifica o n?mero de posi??es do vetor que t?m a mesma idade
    if(IdadesUnicasMulheres[i]>=(length(lxF)-1)){       ##S? estima para idades abaixo da idade limite da tabela de vida. Diminui em 1 porque a tabela de vida come?a na idade 0.
                        ResultadoMulheres[IndiceIdadesUnicas]=rep(0,nIndiceIdadesUnicas)         ##Se for maior que o limite da t?bua de vida, atribui 0.
                }else{                           ##Se for menor, calcula.
                        lxNovo=lxF[(IdadesUnicasMulheres[i]+1):(length(lxF))]   ##Cria novo vetor de lx apenas com valores acima da idade i
                        ResultadoMulheres[IndiceIdadesUnicas]=EstimaTx(nIndiceIdadesUnicas,lxNovo)          ##fun??o elaborada pelo autor
                }
        }

  VetorTx[IndSexoM]=ResultadoHomens
        VetorTx[IndSexoF]=ResultadoMulheres
        TempoAteMorte=array(VetorTx,dim=c(nrow(MatIdade),ncol(MatIdade)))  ##Transforma vetor em matriz
        TempoAteMorte

}