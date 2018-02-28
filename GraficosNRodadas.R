#' GraficosNRodadas function
#' @param value
#' @return value
#' @export

GraficosNRodadas <- function (MatrizResumoServidor,TamanhoPopulacao,Rodadas,Tempo){ #faz gr?ficos para v?rias Rodadas
  estados=c("Ativos","Invalidos", "Aposentados", "Filhos beneficiarios", "Conjuge beneficiario", "Morto sem beneficiario")
  status = 6
  MatrizResumo = array(dim = c(6,Tempo,Rodadas))
  MatrizResumo[1,,] = MatrizResumoServidor[[1]]
  MatrizResumo[2,,] = MatrizResumoServidor[[2]]
  MatrizResumo[3,,] = MatrizResumoServidor[[3]]
  MatrizResumo[4,,] = MatrizResumoServidor[[4]]
  MatrizResumo[5,,] = MatrizResumoServidor[[5]]
  MatrizResumo[6,,] = MatrizResumoServidor[[6]]

  MatrizMedia=Minimo=Maximo=DP=VARIA=array(data = NA,  dim=c(status, Tempo))  ## Vetor de Media de cada status no tempo
  yMin=yMax= vector(length=status)

  for (t in 1:Tempo){
    for (j in 1:status){
      MatrizMedia[j,t]=mean(MatrizResumo[j,t,])
      Minimo[j,t]=min(MatrizResumo[j,t,])
      Maximo[j,t]=max(MatrizResumo[j,t,])
      DP[j,t]=sd(MatrizResumo[j,t,])
      VARIA[j,t]=var(MatrizResumo[j,t,])
      yMax[j]=max(MatrizResumo[j,,])
      yMin[j]=min(MatrizResumo[j,,])
    }
  }
  #yMaxD=max(MatrizDiferenca)
  #yMinD=min(MatrizDiferenca)
  dif=Maximo-Minimo

  #for (j in 1:status){
  ###Gr?ficos dos valores observados
  #    png(width = 800,height = 800,width = 800,height = 800,paste("Varia??oEstados",j," ", TamanhoPopulacao, "serv.png"))
  #    plot(MatrizResumo[j,,1], type="l", xlab="Tempo", ylab="Frequencia", ylim=c(yMin[j],yMax[j]), main=(estados[j]),cex.axis=1.5,cex.lab=1.5, cex=1.5, cex.main=1.5)
  #    for (k in 2:Rodadas) lines(MatrizResumo[j,,k],cex.axis=1.5,cex.lab=1.5, cex=1.5)
  #    lines(MatrizMedia[j,], col="red",lwd=2,cex.axis=1.5,cex.lab=1.5, cex=1.5)
  #    dev.off()
  ###Gr?ficos das Diferencas observadas
  #    png(width = 800,height = 800,width = 800,height = 800,paste("DiferencaEstados",j," ", TamanhoPopulacao, "serv.png"))
  #    plot(MatrizDiferenca[j,,1], type="l", xlab="Tempo", ylab="Diferenca em rela??o ? Media", ylim=c(yMinD,yMaxD),main=estados[j],cex.axis=1.5,cex.lab=1.5, cex=1.5, cex.main=1.5)
  #    for (k in 2:Rodadas){
  #        lines(MatrizDiferenca[j,,k],cex.axis=1.5,cex.lab=1.5, cex=1.5)
  #    }
  #    abline(h=0, col="red", lwd=2)
  #    dev.off()
  #}
  ###Gr?ficos do Maximo e Minimo
  png(width = 800,height = 800,paste("- Extremos",estados[1]," ", TamanhoPopulacao, "serv.png"))
  plot(Maximo[1,], type="l" , lty =2,lwd=2, ylab="Frequencia", xlab="Tempo",main=paste((estados[1])),cex.axis=1.5,cex.lab=1.5, cex=1.5, cex.main=1.5)
  lines(Minimo[1,],lty =3,lwd=2, cex=1.5)
  lines(MatrizMedia[1,],col="red",lwd=2, cex=1.5)
  legend("topright",legend=c("Maximo","Media","Minimo"), col=c("black","red","black"), lty = c(2,1,3), cex=1.5, bty="n",lwd=2)
  dev.off()

  png(width = 800,height = 800,paste("- Extremos",estados[2]," ", TamanhoPopulacao, "serv.png"))
  plot(Maximo[2,], type="l" , lty =2, ylab="Frequencia", xlab="Tempo",main=paste((estados[2])),lwd=2, cex=1.5,cex.axis=1.5,cex.lab=1.5, cex.main=1.5)
  lines(Minimo[2,],lty =3,lwd=2, cex=1.5)
  lines(MatrizMedia[2,],lwd=2,col="red", cex=1.5)
  legend("topright",legend=c("Maximo","Media","Minimo"), col=c("black","red","black"), lty = c(2,1,3), cex=1.5, bty="n",lwd=2)
  dev.off()

  png(width = 800,height = 800,paste("- Extremos",estados[3]," ", TamanhoPopulacao, "serv.png"))
  plot(Maximo[3,], type="l" , lty =2, ylab="Frequencia", xlab="Tempo",main=paste((estados[3])),cex.axis=1.5,cex.lab=1.5, cex=1.5, cex.main=1.5,lwd=2)
  lines(Minimo[3,],lty =3, cex=1.5,lwd=2)
  lines(MatrizMedia[3,],col="red", cex=1.5,lwd=2)
  legend("topright",legend=c("Maximo","Media","Minimo"), col=c("black","red","black"), lty = c(2,1,3), cex=1.5, bty="n",lwd=2)
  dev.off()

  png(width = 800,height = 800,paste("- Extremos",estados[4]," ", TamanhoPopulacao, "serv.png"))
  plot(Maximo[4,], type="l" , lty =2, ylab="Frequencia", xlab="Tempo",main=paste((estados[4])),cex.axis=1.5,cex.lab=1.5, cex=1.5, cex.main=1.5,lwd=2)
  lines(Minimo[4,],lty =3, cex=1.5,lwd=2)
  lines(MatrizMedia[4,],col="red", cex=1.5,lwd=2)
  legend("topright",legend=c("Maximo","Media","Minimo"), col=c("black","red","black"), lty = c(2,1,3), cex=1.5, bty="n",lwd=2)
  dev.off()

  png(width = 800,height = 800,paste("- Extremos",estados[5]," ", TamanhoPopulacao, "serv.png"))
  plot(Maximo[5,], type="l" , lty =2, ylab="Frequencia", xlab="Tempo",main=paste(estados[5]),cex.axis=1.5,cex.lab=1.5, cex=1.5, cex.main=1.5,lwd=2)
  lines(Minimo[5,],lty =3, cex=1.5,lwd=2)
  lines(MatrizMedia[5,],col="red", cex=1.5,lwd=2)
  legend("topright",legend=c("Maximo","Media","Minimo"), col=c("black","red","black"), lty = c(2,1,3), cex=1.5, bty="n",lwd=2)
  dev.off()

  png(width = 800,height = 800,paste("- Extremos",estados[6]," ", TamanhoPopulacao, "serv.png"))
  plot(Maximo[6,], type="l" , lty =2, ylab="Frequencia", xlab="Tempo",main=paste((estados[j])),cex.axis=1.5,cex.lab=1.5, cex=1.5, cex.main=1.5,lwd=2)
  lines(Minimo[6,],lty =3, cex=1.5,lwd=2)
  lines(MatrizMedia[6,],col="red", cex=1.5,lwd=2)
  legend("topleft",legend=c("Maximo","Media","Minimo"), col=c("black","red","black"), lty = c(2,1,3), cex=1.5, bty="n",lwd=2)
  dev.off()

  png(width = 800,height = 800,paste("- Desvio Padrao dos estados", TamanhoPopulacao, "serv.png"))
  plot(DP[1,], main="Desvio Padrao dos estados",type="l",lty = 1,lwd=2,col="red", ylim=c(min(DP), 1.3*max(DP)), ylab="Desvio Padrao", xlab="Tempo",cex.axis=1.5,cex.lab=1.5, cex=1.5)
  lines(DP[2,], col="blue",lty =2,lwd=2 , cex=1.5)
  lines(DP[3,], col="dark green",lty = 3,lwd=2, cex=1.5)
  lines(DP[4,], col="dark orange",lty = 4,lwd=2, cex=1.5)
  lines(DP[5,], col="brown",lty = 5,lwd=2, cex=1.5)
  lines(DP[6,], col="green",lty =6 ,lwd=2, cex=1.5)
  legend ("topleft",legend=estados[1:3],lty = seq(1:3),lwd=2,col = c("red", "blue","dark green"), cex=1.2, bty="n")
  legend ("topright",legend=estados[4:6],lty = seq(4:6),lwd=2,col = c("dark orange","green","green"), cex=1.2, bty="n")
  dev.off()

  png(width = 800,height = 800,paste("- Diferenca Maxima", TamanhoPopulacao, "serv.png"))
  plot(dif[1,], main="Diferenca Maxima dos Estados",type="l", ylim=c(min(dif), 1.3*max(dif)),lwd=2,col="red",ylab="Diferenca Maxima", xlab="Tempo",cex.axis=1.5,cex.lab=1.5, cex=1.5)
  lines(dif[2,], col="blue",lty =2 ,lwd=2, cex=1.5)
  lines(dif[3,], col="dark green",lty = 3,lwd=2, cex=1.5)
  lines(dif[4,], col="dark orange",lty = 4,lwd=2, cex=1.5)
  lines(dif[5,], col="brown",lty = 5,lwd=2, cex=1.5)
  lines(dif[6,], col="green",lty =6 ,lwd=2, cex=1.5)
  legend ("topleft",legend=estados[1:3],lty = seq(1:3),lwd=2,col = c("red", "blue","dark green"), cex=1.2, bty="n")
  legend ("topright",legend=estados[4:6],lty = seq(4:6),lwd=2,col = c("dark orange","brown","green"), cex=1.2, bty="n")
  dev.off()

  png(width = 800,height = 800,paste("Media", TamanhoPopulacao, "serv.png"))
  plot(MatrizMedia[1,],main="- Media dos Estados",type="l", ylim=c(min(MatrizMedia), 1.3*max(MatrizMedia)),lwd=2,col="red",ylab="Media", xlab="Tempo",cex.axis=1.5,cex.lab=1.5, cex=1.5)
  lines(MatrizMedia[2,], col="blue",lty =2 ,lwd=2, cex=1.5)
  lines(MatrizMedia[3,], col="dark green",lty = 3,lwd=2, cex=1.5)
  lines(MatrizMedia[4,], col="dark orange",lty = 4,lwd=2, cex=1.5)
  lines(MatrizMedia[5,], col="brown",lty = 5,lwd=2, cex=1.5)
  lines(MatrizMedia[6,], col="green",lty =6 ,lwd=2, cex=1.5)
  legend ("topleft",legend=estados[1:3],lty = seq(1:3),lwd=2,col = c("red", "blue","dark green"), cex=1.2, bty="n")
  legend ("topright",legend=estados[4:6],lty = seq(4:6),lwd=2,col = c("dark orange","brown","green"), cex=1.2, bty="n")
  dev.off()

}