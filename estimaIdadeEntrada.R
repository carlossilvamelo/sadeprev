#' estimaIdadeEntrada function
#' @param value
#' @return value
#' @export

estimaIdadeEntrada = function (DadosServidores){
### estima idade em que o servidor entrou no servi?o p?blico.

Idade=DadosServidores$Idade
Sexo=DadosServidores$Sexo
EstadoInicial = DadosServidores$EstadoInicial
TamanhoPopulacao = length(DadosServidores[,1])
IdadeEntrada=vector(length=TamanhoPopulacao)

#Estima m?dia da distribui??o.
Media=vector(length=TamanhoPopulacao)
#Homem
Media[Sexo==2]=(6.94+0.62*Idade[Sexo==2])
#Mulher
Media[Sexo==1]=6.94+0.62*Idade[Sexo==1]+1.99-0.07*Idade[Sexo==1]

#estima desvio padr?o
DesvioPadrao=vector(length=TamanhoPopulacao)
DesvioPadrao[Sexo==2 & Idade<=19] = 0.51
DesvioPadrao[Sexo==2 & (Idade>=20 & Idade<=24) ] = 1.63
DesvioPadrao[Sexo==2 & (Idade>=25 & Idade<=29) ] = 2.57
DesvioPadrao[Sexo==2 & (Idade>=30 & Idade<=34) ] = 3.97
DesvioPadrao[Sexo==2 & (Idade>=35 & Idade<=39) ] = 5.29
DesvioPadrao[Sexo==2 & (Idade>=40 & Idade<=44) ] = 6.95
DesvioPadrao[Sexo==2 & (Idade>=45 & Idade<=49) ] = 9.10
DesvioPadrao[Sexo==2 & (Idade>=50)] = 10.89
DesvioPadrao[Sexo==1 & Idade<=19] = 0.50
DesvioPadrao[Sexo==1 & (Idade>=20 & Idade<=24) ] = 1.60
DesvioPadrao[Sexo==1 & (Idade>=25 & Idade<=29) ] = 2.75
DesvioPadrao[Sexo==1 & (Idade>=30 & Idade<=34) ] = 4.04
DesvioPadrao[Sexo==1 & (Idade>=35 & Idade<=39) ] = 5.50
DesvioPadrao[Sexo==1 & (Idade>=40 & Idade<=44) ] = 7.06
DesvioPadrao[Sexo==1 & (Idade>=45 & Idade<=49) ] = 8.74
DesvioPadrao[Sexo==1 & (Idade>=50)] = 10.10

Aleatorio=rnorm(TamanhoPopulacao, mean = 0, sd = 1)    ##Gera n?mero aleat?rio

for (j in 1:TamanhoPopulacao) IdadeEntrada[j]= min((Idade[j]-1),floor(Aleatorio[j]*DesvioPadrao[j]+Media[j]))    #Arredonda para baixo o valor da idade, indicando idade completa. Assume que idade de entrada m?nima ? a idade anterior ? atual. (N?o d? para ser a atual porque pode geral tempo de contribui??o igual a zero na fun??o de benef?cios).
IdadeEntrada[IdadeEntrada<18]=18

IdadeEntrada[EstadoInicial!=1] = -1

#png(paste("Idade de entrada - ", pop,"Servidores.png"))
#ymax=max(IdadeEntrada)
#plot(DadosServidores$x[DadosServidores$Sexo==1],IdadeEntrada[DadosServidores$Sexo==1],xlab="Idade atual", ylab="Idade de entrada",
# col="red",pch=1, ylim=c(18,max(IdadeEntrada)),cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=2)
#points(DadosServidores$x[DadosServidores$Sexo==2],IdadeEntrada[DadosServidores$Sexo==2],col="blue",
#pch=0,cex.axis=1.5,cex.lab=1.5, cex=1.5)
#legend("topleft", c("Mulheres", "Homens"),pch= c(1,0), col=c("red","blue"),cex =1.5)
#dev.off()

#png(paste("Pir?mide Idade de Entrada ",N[i],"servidores.png"))
#ParaPiramide=DadosServidores;ParaPiramide$x=IdadeEntrada
#piramide(ParaPiramide,titulo=(paste0(pop," Servidores")))
#dev.off()

return (IdadeEntrada)

}


