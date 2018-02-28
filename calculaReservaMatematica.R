#' calculaReservaMatematica function
#' @param value
#' @return value
#' @export

#####Estima??o da reserva matem?tica prospectiva.
calculaReservaMatematica=function(DadosServidores,SalarioContribuicao, Beneficio, Juros, Aliquota,Tfuturo,Rodadas){

ReservaProspectiva=array(data = 0,  dim=c(Tfuturo,Rodadas))
#Reserva Matem?tica Prospectiva.
ReservaProspectiva[Tfuturo,]=Beneficio[Tfuturo,]-SalarioContribuicao[Tfuturo,]*Aliquota
for (t in (Tfuturo-1):1) ReservaProspectiva[t,]=ReservaProspectiva[t+1,]*(1/(1+Juros))+Beneficio[t,]-SalarioContribuicao[t,]*Aliquota

return(ReservaProspectiva)
}
