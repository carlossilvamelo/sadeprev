#' geraGraficoPercentil function
#' @param value
#' @return value
#' @export

geraGraficoPercentil=function(MatrizValores,IntervaloConfianca,Ymin, Ymax, Titulo, NomeY, PosicaoLegenda){
###Gr?ficos por percentil
Tempo=dim(MatrizValores[])[1]
QuartilInferior=QuartilSuperior=Media=Minimo=Maximo=0
for (t in 1:Tempo) {
    QuartilInferior[t]=quantile(MatrizValores[][t,],(1-IntervaloConfianca)/2)
    QuartilSuperior[t]=quantile(MatrizValores[][t,],IntervaloConfianca+(1-IntervaloConfianca)/2)
    Media[t]=mean(MatrizValores[][t,])
    Maximo[t]=max(MatrizValores[][t,])
    Minimo[t]=min(MatrizValores[][t,])
}

plot(Media, type="l", ylim=c(Ymin,Ymax), xlab="Tempo", ylab=NomeY,lwd=2,main=Titulo,cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
lines (QuartilInferior, type="l",lty=2, col=4,lwd=2,cex.axis=1.5,cex.lab=1.5, cex=1.5)
lines (QuartilSuperior, type="l",lty=2, col=4,lwd=2,cex.axis=1.5,cex.lab=1.5, cex=1.5)
lines (Maximo, type="l", lty=3, col="orange", lwd=2,cex.axis=1.5,cex.lab=1.5, cex=1.5)
lines (Minimo, type="l", lty=3, col="orange", lwd=2,cex.axis=1.5,cex.lab=1.5, cex=1.5)
legend (PosicaoLegenda,lty = c(1,2,3),lwd=2,col=c(1,4,"orange"),c("Media",paste0("IC ", IntervaloConfianca,"%"), "Extremos"),bty="n",cex=1.5)
abline(h=0)
}

