#' piramide function
#' @param value
#' @return value
#' @export

piramide = function(DadosServidores,titulo){

  Feminino=Masculino=vector(length=(70-17))
  for (i in 18:70){
    Feminino[(i-17)]=sum(DadosServidores$Idade[DadosServidores$Idade==i & DadosServidores$Sexo==1])/i
    Masculino[(i-17)]=sum(DadosServidores$Idade[DadosServidores$Idade==i & DadosServidores$Sexo==2])/i
  }

  amplitude=1
  edadmax=70

  escalax<-length(DadosServidores$Idade)/200
  max1<-max(c(Feminino,Masculino));n<-length(Masculino)
  min.x<--(max1%/%escalax+1)*escalax;max.x<-(max1%/%escalax+1)*escalax

  pir<-plot(0,0,type="n",xaxt='n',yaxt='n',ylim=c(18,edadmax+5),xlim=c(min.x,max.x),xlab="",ylab="", cex=1.5)

  ejex1<-seq(0,max1,by=escalax);ejex2<--ejex1[order(-ejex1)]
  ejex<-c(ejex2,ejex1)
  axis(1,at=ejex,labels=as.character(abs(ejex)),cex.axis=1.5)
  ejey<-c(seq(18,edadmax,by=4))
  axis(2,at=ejey,labels=as.character(ejey),cex.axis=1.5,las=2)
  for(i in 1:n){
    x1<-0;x2<-Masculino[i]; x3<--Feminino[i]
    y1<-(i-1)*amplitude+18
    y2<-y1+amplitude
    rect(x1,y1,x2,y2,col='blue')
    rect(x1,y1,x3,y2,col='orange')
  }

  x.l1<--max1/16-1.5*escalax;x.l2<-max1/16+escalax
  title(main=paste("\n",titulo),ylab="Idade", xlab="Frequ?ncia",cex.main=1.5,cex.lab=1.5)
  legend(x.l1,edadmax+7.5,"Feminino",bty="n",xjust=1,cex=1.5)
  legend(x.l2,edadmax+7.5,"Masculino",bty="n", cex=1.5)
}