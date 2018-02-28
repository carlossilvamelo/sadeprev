#' EstimaTx function
#' @param value
#' @return value
#' @export


EstimaTx=function(n,lx){  #Estima Tx
  ##Estima tempo de vida futuro aleat?rio para n indiv?duos com lx a partir da idade desses indiv?duos dado pelo vetor lx.
  lxMais1=c((lx[2:length(lx)]),0)
  qy=(lx-lxMais1)/lx[1]    ###Estima a probabilidade de morte entre as idades x e x+1, dado que tem a idade atual y.
  qy=qy[1:(length(lx))]
  TxPossivel=0:(length(lx)-1)              ##Tira duas possibilidades de valores porque come?a em zero e porque a ?ltima idade n?o ? poss?vel (todos mortos)
  Tx=sample(TxPossivel,n,replace = T,qy)       ##Tira amostra de tempo de vida futuro. Assume que tempos de vida futuros podem assumir valores dados por xx, estima n valores diferentes, valores podem se repetir, e t?m distribui??o de probabilidade dada por prob)
  Tx

}
