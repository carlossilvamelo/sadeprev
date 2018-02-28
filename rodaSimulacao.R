#' rodaSimulacao function
#' @param value
#' @return value
#' @export

rodaSimulacao <- function (DadosRPPS, Diretorio, TaxaAumentoSalarial, JurosAnual, Rodadas, Tempo,TipoEntrada){

    result = tryCatch({
      TMD = read.table(paste(Diretorio, "/TMD.txt", sep = ""),header = TRUE, sep = "", dec = ",", )
      ProbFamilia = read.table(paste(Diretorio, "/Probab de Familia.txt",sep = ""), header = TRUE, sep = "", dec = ",", )
      TabuasMorte = read.table(paste(Diretorio, "/Tabuas de Morte.txt",sep = ""), header = TRUE, sep = "", dec = ",", )
      TabelaEsperancaDeVida = read.table(paste(Diretorio, "/ex.txt", sep = ""), header = TRUE, sep = "", dec = ",", )
      DadosRPPS = organizaDadosRPPS(DadosRPPS)

      if(TipoEntrada=="Usar dados reais") {
        DadosServidores = DadosRPPS[[1]]
      } else DadosServidores = Gera1PopInicial(500,3)
      DadosServidores = cbind(DadosServidores, IdadeAposentadoria = estimaIdadeAposentadoria(DadosServidores)[,1], TipoAposentadoria = estimaIdadeAposentadoria(DadosServidores)[,2])

      Aliquota = DadosRPPS$Aliquota
      FundoAtual = DadosRPPS$FundoAtual
      TamanhoPopulacao = nrow(DadosServidores)

      nCores <- detectCores()
      cl <- makeCluster(nCores)
      registerDoParallel(nCores)
      ListaBeneficio = ListaSalarioContribuicao = ListaMatrizResumoServidor = 0
      MatrizEstadosServidor = BeneficioeContribuicao = 0
      MatrizResumoServidor = replicate(6, list(matrix(0, Tempo,0)))
      Blocos = 20
      ListaResultados = foreach(i = 1:Blocos, .packages = "SADEPrev") %dopar%
      {
        MatrizEstadosServidor = estimaEstadosServidor(DadosServidores,TMD, TabuasMorte, ProbFamilia, Rodadas/Blocos)
        BeneficioeContribuicao = calculaValoresPagamento(DadosServidores,MatrizEstadosServidor, TabelaEsperancaDeVida,TaxaAumentoSalarial, Tempo, Rodadas/Blocos)
        ListaBeneficio = BeneficioeContribuicao[[1]]
        ListaSalarioContribuicao = BeneficioeContribuicao[[2]]
        ListaMatrizResumoServidor = resumeEstadoServidor(MatrizEstadosServidor,DadosServidores, Tempo)
        return(c(list(Beneficio = ListaBeneficio),list(SalarioContribuicao = ListaSalarioContribuicao),list(Resumo = ListaMatrizResumoServidor)))
      }
      stopCluster(cl)
      Beneficio = SalarioContribuicao = matrix(NA, nrow = Tempo,ncol = 0)
      for (i in 1:Blocos) {
        Beneficio = cbind(Beneficio, ListaResultados[[i]]$Beneficio)
        SalarioContribuicao = cbind(SalarioContribuicao, ListaResultados[[i]]$SalarioContribuicao)
        MatrizResumoServidor = mapply(cbind, MatrizResumoServidor,ListaResultados[[i]]$Resumo, SIMPLIFY = FALSE)
      }
      rm(ListaResultados)
      invisible(gc())
      while(is.nan(sum(Beneficio))) {
        Beneficio[which(is.nan(Beneficio))] = Beneficio[(which(is.nan(Beneficio))+75)]
      }
      Tfuturo = 200
      Contribuicao = RM = RM0 = FundoPadronizado = RProspecFechada = DiferencaReservaFechada = DifPadronizadaFechada = list(length = 1)
      AliquotaSugerida = sugereAliquota(SalarioContribuicao, Beneficio,FundoAtual, JurosAnual)
      FundoAtualIdeal = sugereFundoAtual(SalarioContribuicao, Beneficio,Aliquota, JurosAnual, FundoAtual)
      Contribuicao = SalarioContribuicao * AliquotaSugerida
      Fundo = calculaFundo(SalarioContribuicao, Beneficio, FundoAtual,JurosAnual, AliquotaSugerida, Tempo, Rodadas * Blocos)
      FundoPadronizado = Fundo/TamanhoPopulacao
      SC = rbind(SalarioContribuicao, matrix(rep(0, (Tfuturo-Tempo) * Rodadas * Blocos), ncol = Rodadas))
      B = rbind(Beneficio, matrix(rep(0, (Tfuturo - Tempo) * Rodadas*Blocos), ncol = Rodadas))
      Pop = cbind(DadosServidores, matrix(rep(0, TamanhoPopulacao*Rodadas), ncol = Rodadas))
      ReservaMatematica = calculaReservaMatematica(DadosServidores,SC, B, JurosAnual, AliquotaSugerida, 80, Rodadas)
      DiferencaReservaFechada = (Fundo - ReservaMatematica[1:75,])

      DifPadronizadaFechada = DiferencaReservaFechada/TamanhoPopulacao
      DataHora = gsub(":", "-", as.character(Sys.time()))
      DiretorioSaida = paste(Diretorio, "/Resultados/", DataHora,sep = "")

      dir.create(file.path(paste(Diretorio, "/Resultados/", DataHora,sep = "")), showWarnings = FALSE)
      write.csv(Beneficio, paste(DiretorioSaida, "/Beneficios futuros com Aliquota ou Fundo sugerido.csv",sep = ""))
      write.csv(SalarioContribuicao, paste(DiretorioSaida, "/Salarios de contribuicao futuros.csv",sep = ""))
      write.csv(Contribuicao, paste(DiretorioSaida, "/Contribuicoes futuras com Aliquota ou Fundo sugerido.csv",sep = ""))
      write.csv(Fundo, paste(DiretorioSaida, "/Fundos futuros com Aliquota ou Fundo sugerido.csv",sep = ""))
      write.csv(FundoPadronizado, paste(DiretorioSaida, "/Fundos padronizados futuros com Aliquota ou Fundo sugerido.csv",sep = ""))
      write.csv(ReservaMatematica, paste(DiretorioSaida, "/Reserva matematica futuros com Aliquota ou Fundo sugerido.csv",sep = ""))
      write.csv(DiferencaReservaFechada, paste(DiretorioSaida,"/DiferencaReservaFechada futura com Aliquota ou Fundo sugerido.csv",sep = ""))
      write.csv(DifPadronizadaFechada, paste(DiretorioSaida, "/DifPadronizadaFechada futura com Aliquota ou Fundo sugerido.csv",sep = ""))
      write.csv(AliquotaSugerida, paste(DiretorioSaida, "/AliquotaSugerida.csv",sep = ""))
      write.csv(FundoAtualIdeal, paste(DiretorioSaida, "/FundoAtualIdeal",sep = ""))


      png(width = 800,height = 800,filename = paste(DiretorioSaida, "/Fundos futuros com Aliquota ou Fundo sugerido.png",sep = ""))
      geraGraficoPercentil(Fundo/1e+06, 0.95, min(Fundo/1e+06),max(Fundo/1e+06), "Fundos futuros com Aliquota ou Fundo sugerido","Milhoes R$", "topleft")
      dev.off()
      png(width = 800,height = 800,filename = paste(DiretorioSaida, "/ReservaMatematica futura com Aliquota ou Fundo sugerido.png",sep = ""))
      geraGraficoPercentil(ReservaMatematica/1e+06, 0.95, min(ReservaMatematica/1e+06), max(ReservaMatematica/1e+06), "Reserva Matematica futura com Aliquota ou Fundo sugerido","Milhoes R$", "topright")
      dev.off()
      png(width = 800,height = 800,filename = paste(DiretorioSaida, "/DiferencaReservaFechada futura com Aliquota ou Fundo sugerido.png",sep = ""))
      geraGraficoPercentil(DiferencaReservaFechada/1e+06, 0.95,min(DiferencaReservaFechada/1e+06), max(DiferencaReservaFechada/1e+06),"Diferenca Reserva Padronizada futura com Aliquota ou Fundo sugerido","Milhoes R$", "topleft")
      dev.off()
      Contribuicao = RM = RM0 = FundoPadronizado = RProspecFechada = DiferencaReservaFechada = DifPadronizadaFechada = list(length = 1)

      Contribuicao = SalarioContribuicao * Aliquota
      Fundo = calculaFundo(SalarioContribuicao, Beneficio, FundoAtual,JurosAnual, Aliquota, Tempo, Rodadas * Blocos)
      FundoPadronizado = Fundo/TamanhoPopulacao
      SC = rbind(SalarioContribuicao, matrix(rep(0, (Tfuturo-Tempo) * Rodadas * Blocos), ncol = Rodadas))
      B = rbind(Beneficio, matrix(rep(0, (Tfuturo - Tempo) * Rodadas*Blocos), ncol = Rodadas))
      Pop = cbind(DadosServidores, matrix(rep(0, TamanhoPopulacao * Rodadas), ncol = Rodadas))
      ReservaMatematica = calculaReservaMatematica(DadosServidores, SC, B, JurosAnual, Aliquota, 80, Rodadas)
      DiferencaReservaFechada = (Fundo - ReservaMatematica[1:75,])
      DifPadronizadaFechada = DiferencaReservaFechada/TamanhoPopulacao
      write.csv(Beneficio, paste(DiretorioSaida, "/Beneficio.csv",sep = ""))
      write.csv(Contribuicao, paste(DiretorioSaida, "/Contribuicao.csv",sep = ""))
      write.csv(Fundo, paste(DiretorioSaida, "/Fundo.csv", sep = ""))
      write.csv(FundoPadronizado, paste(DiretorioSaida, "/FundoPadronizado.csv",sep = ""))
      write.csv(ReservaMatematica, paste(DiretorioSaida, "/Reserva.csv",sep = ""))
      write.csv(DiferencaReservaFechada, paste(DiretorioSaida,"/DiferencaReservaFechada.csv", sep = ""))
      write.csv(DifPadronizadaFechada, paste(DiretorioSaida, "/DifPadronizadaFechada.csv", sep = ""))

      png(width = 800,height = 800,filename = paste(DiretorioSaida, "/Fundo.png", sep = ""))
      geraGraficoPercentil(Fundo/1e+06, 0.95, min(Fundo/1e+06),max(Fundo/1e+06), "Fundo RPPS", "Milhoes R$", "topleft")
      dev.off()
      png(width = 800,height = 800,filename = paste(DiretorioSaida, "/ReservaMatematica.png",sep = ""))
      geraGraficoPercentil(ReservaMatematica/1e+06, 0.95, min(ReservaMatematica/1e+06),max(ReservaMatematica/1e+06), "Reserva Matematica RPPS","Milhoes R$", "topright")
      dev.off()
      png(width = 800,height = 800,filename = paste(DiretorioSaida, "/DifPadronizadaFechada.png",sep = ""))
      geraGraficoPercentil(DifPadronizadaFechada/1e+06, 0.95, min(DifPadronizadaFechada/1e+06),max(DifPadronizadaFechada/1e+06), "Diferenca Reserva Padronizada RPPS","Milhoes R$", "topleft")
      dev.off()

      png(width = 800,height = 800,filename = paste(DiretorioSaida, "/Beneficio.png",sep = ""))
      geraGraficoPercentil(Beneficio/1e+06, 0.95, min(Beneficio/1e+06),max(Beneficio/1e+06), "Beneficios RPPS","Milhoes R$", "topright")
      dev.off()
      png(width = 800,height = 800,filename = paste(DiretorioSaida, "/Contribuicao.png",sep = ""))
      geraGraficoPercentil(Contribuicao/1e+06, 0.95, min(Contribuicao/1e+06),max(Contribuicao/1e+06), "Contribuicoes RPPS","Milhoes R$", "topright")
      dev.off()
      png(width = 800,height = 800,filename = paste(DiretorioSaida, "/IndiceCoberturaTotal.png",sep = ""))
      geraGraficoPercentil((Fundo/ReservaMatematica[1:75,]), 0.95, min((Fundo/ReservaMatematica[1:75,])),max((Fundo/ReservaMatematica[1:75,])), "Indice de Cobertura Total do Plano","Indice", "topleft")
      dev.off()
      Beneficio1 = Beneficio
      Beneficio1[Beneficio1==0] = 1
      png(width = 800,height = 800,filename = paste(DiretorioSaida, "/IndiceMaturidadeFinanceiraRestrita.png",sep = ""))
      geraGraficoPercentil((SalarioContribuicao/Beneficio1), 0.95, min((SalarioContribuicao/Beneficio1)),max((SalarioContribuicao/Beneficio1)), "Indice de Maturidade Financeira Restrita","Indice", "topright")
      dev.off()

      {
        VarFundoPad = VarDifPad = dpFundoPad = dpDifPad = ProbRuinaFundoPad = ProbRuinaDifPad = TamRuinaFundoPad = TamRuinaDifPad = vector(length = Tempo)
        for (t in 1:Tempo) {
          VarFundoPad[t] = var(FundoPadronizado[t, ])
          VarDifPad[t] = var(DifPadronizadaFechada[t, ])
          dpFundoPad[t] = sd(FundoPadronizado[t, ])
          dpDifPad[t] = sd(DifPadronizadaFechada[t, ])
          ProbRuinaFundoPad[t] = mean(FundoPadronizado[t, ]<0)
          ProbRuinaDifPad[t] = mean(DifPadronizadaFechada[t,] < 0)
          if (sum(FundoPadronizado[t, ] < 0) > 0)TamRuinaFundoPad[t] = -mean(FundoPadronizado[t,][FundoPadronizado[t, ] < 0])
          if (sum(DifPadronizadaFechada[t, ] < 0) > 0) TamRuinaDifPad[t] = -mean(DifPadronizadaFechada[t,][DifPadronizadaFechada[t, ] < 0])
        }

        write.csv(TamRuinaFundoPad, paste(DiretorioSaida, "/TamRuinaFundoPad.csv",sep = ""))
        write.csv(TamRuinaDifPad, paste(DiretorioSaida, "/TamRuinaDifPad.csv", sep = ""))
        TempoAteRuinaFundoPad = TempoAteRuinaDifPad = vector(length = Rodadas)
        for (k in 1:Rodadas) {
          TempoAteRuinaFundoPad[k] = length(FundoPadronizado[,k]) - length(FundoPadronizado[, k][FundoPadronizado[,k] > 0]) + 1
          TempoAteRuinaDifPad[k] = length(DifPadronizadaFechada[, k]) - length(DifPadronizadaFechada[, k][DifPadronizadaFechada[, k] > 0]) + 1
        }
        TempoAteRuinaFundoPad[TempoAteRuinaFundoPad == 76] = NA
        TempoAteRuinaDifPad[TempoAteRuinaDifPad == 76] = NA
        write.csv(TempoAteRuinaFundoPad, paste(DiretorioSaida,"/TempoAteRuinaFundoPad.csv", sep = ""))
        write.csv(TempoAteRuinaDifPad, paste(DiretorioSaida,"/TempoAteRuinaDifPad.csv", sep = ""))
        p = 0.9
        VaR = CVaR = TVaR = ES = VaRDif = CVaRDif = TVaRDif = ESDif = vector(length = Tempo)
        for (t in 1:Tempo) {
          Perda = -FundoPadronizado
          Perda[FundoPadronizado > 0] = 0
          VaR[t] = quantile(Perda[t, ], p)
          ES[t] = sum(Perda[t, ][Perda[t, ] > VaR[t]])/Rodadas
          TVaR[t] = VaR[t] + ES[t]/(1 - p)
          CVaR[t] = mean(Perda[t, ][Perda[t, ] > VaR[t]])
          Perda = -DifPadronizadaFechada
          Perda[DifPadronizadaFechada > 0] = 0
          VaRDif[t] = quantile(Perda[t, ], p)
          ESDif[t] = sum(Perda[t, ][Perda[t, ] > VaRDif[t]])/Rodadas
          TVaRDif[t] = VaRDif[t] + ESDif[t]/(1 - p)
          CVaRDif[t] = mean(Perda[t, ][Perda[t, ] > VaRDif[t]])
        }
        CVaR[which(is.nan(CVaR))] = 0
        CVaRDif[which(is.nan(CVaRDif))] = 0

        write.csv(Perda, paste(DiretorioSaida, "/Perda.csv",sep = ""))
        write.csv(VaR, paste(DiretorioSaida, "/VaR.csv", sep = ""))
        write.csv(TVaR, paste(DiretorioSaida, "/TVaR.csv", sep = ""))
        write.csv(CVaR, paste(DiretorioSaida, "/CVaR.csv", sep = ""))
        write.csv(VaRDif, paste(DiretorioSaida, "/VaRDif.csv",sep = ""))
        write.csv(ESDif, paste(DiretorioSaida, "/ESDif.csv", sep = ""))
        write.csv(TVaRDif, paste(DiretorioSaida, "/TVaRDif.csv",sep = ""))
        write.csv(CVaRDif, paste(DiretorioSaida, "/CVaRDif.csv", sep = ""))

        png(width = 800,height = 800,filename = paste(DiretorioSaida, "/VaR.png", sep = ""))
        plot(VaR/1e+06, main = "VaR", xlab = "Tempo", ylab = "Milhoes R$",type = "l", lwd = 2, cex.axis = 1.5, cex.lab = 1.5, cex = 1.5, cex.main = 1.5)
        dev.off()
        png(width = 800,height = 800,filename = paste(DiretorioSaida, "/TVaR.png", sep = ""))
        plot(TVaR/1e+06, main = "TVaR", xlab = "Tempo", ylab = "Milhoes R$", type = "l", lwd = 2, cex.axis = 1.5, cex.lab = 1.5,cex = 1.5, cex.main = 1.5)
        dev.off()
        png(width = 800,height = 800,filename = paste(DiretorioSaida, "/CVaR.png", sep = ""))
        plot(CVaR/1e+06, main = "CVaR", xlab = "Tempo", ylab = "Milhoes R$",type = "l", lwd = 2, cex.axis = 1.5, cex.lab = 1.5, cex = 1.5, cex.main = 1.5)
        dev.off()
        png(width = 800,height = 800,filename = paste(DiretorioSaida, "/VaRDif.png", sep = ""))
        plot(VaRDif/1e+06, main = "VaRDif", xlab = "Tempo", ylab = "Milhoes R$",type = "l", lwd = 2, cex.axis = 1.5, cex.lab = 1.5,cex = 1.5, cex.main = 1.5)
        dev.off()
        png(width = 800,height = 800,filename = paste(DiretorioSaida, "/ESDif.png", sep = ""))
        plot(ESDif/1e+06, main = "ESDif", xlab = "Tempo", ylab = "Milhoes R$",type = "l", lwd = 2, cex.axis = 1.5, cex.lab = 1.5, cex = 1.5, cex.main = 1.5)
        dev.off()
        png(width = 800,height = 800,filename = paste(DiretorioSaida, "/TVaRDif.png",sep = ""))
        plot(TVaRDif/1e+06, main = "TVaRDif", xlab = "Tempo",ylab = "Milhoes R$", type = "l", lwd = 2, cex.axis = 1.5,cex.lab = 1.5, cex = 1.5, cex.main = 1.5)
        dev.off()
        png(width = 800,height = 800,filename = paste(DiretorioSaida, "/CVaRDif.png",sep = ""))
        plot(CVaRDif/1e+06, main = "CVaRDif", xlab = "Tempo",ylab = "Milhoes R$", type = "l", lwd = 2, cex.axis = 1.5,cex.lab = 1.5, cex = 1.5, cex.main = 1.5)
        dev.off()
      }

      {
        setwd(DiretorioSaida)
        EstadosServidor = estimaEstadosServidor(DadosServidores,TMD, TabuasMorte, ProbFamilia,1)
        GraficosMudaEstado(DadosServidores,EstadosServidor)
        GraficosNRodadas(MatrizResumoServidor,TamanhoPopulacao,Rodadas,Tempo)

        #Mais graficos da funcao FazGraficosSimulacao.R
        DadosServidoresAtivos = DadosServidores
        DadosServidoresAtivos = DadosServidoresAtivos[DadosServidoresAtivos$EstadoInicial==1,]
        png(width = 800,height = 800,paste("Piramide Etaria ",TamanhoPopulacao,"servidores.png"))
        piramide(DadosServidoresAtivos,titulo=(paste0(nrow(DadosServidoresAtivos)," Servidores")))
        dev.off()
        png(width = 800,height = 800,paste("Remuneracao ",TamanhoPopulacao,"servidores.png"))
        boxplot(DadosServidoresAtivos$Salario~DadosServidoresAtivos$Sexo, main=paste0(nrow(DadosServidoresAtivos)," Servidores"), names=c("Mulheres", "Homens"), ylab="Remuneracao (R$)",cex.axis=1.5,cex.lab=1.5, cex=1.5,cex.main=1.5)
        dev.off()

        ##Grafico Probabilidade de Ruina
        ymaxPR=max(ProbRuinaFundoPad);yminPR=min(ProbRuinaFundoPad)
        png(width = 800,height = 800,"Probabilidade de Ruina.png")
        matplot(ProbRuinaFundoPad, main="Probabilidade de Ruina",type="l",lwd=2, ylab="Probabilidade de Ruina", xlab="Tempo", ylim=c(0,1),cex.axis=1.5,cex.lab=1.5, cex=1.5)
        dev.off()

        ##Grafico Tamanho da Ruina
        png(width = 800,height = 800,"Tamanho da Ruina.png")
        matplot(TamRuinaFundoPad,main="Tamanho da Ruina", type="l",lwd=2, ylab="Tamanho do deficit", xlab="Tempo", ylim=c(min(TamRuinaFundoPad),max(TamRuinaFundoPad)),cex.axis=1.5,cex.lab=1.5, cex=1.5)
        dev.off()


        ##Grafico Risco
        ymaxR=max(TamRuinaFundoPad*ProbRuinaFundoPad);yminR=min(TamRuinaFundoPad*ProbRuinaFundoPad)
        png(width = 800,height = 800,"Risco.png")
        matplot(ProbRuinaFundoPad*TamRuinaFundoPad,main="Risco - Prob. da Ruina x Tamanho da Ruina", type="l",lwd=2, ylab="Risco", xlab="Tempo", ylim=c(yminR,ymaxR),cex.axis=1.5,cex.lab=1.5, cex=1.5)
        dev.off()

        ##Grafico Probabilidade de Ruina da diferenca
        ymaxPR=max(ProbRuinaDifPad)
        yminPR=min(ProbRuinaDifPad)
        png(width = 800,height = 800,"Probabilidade de Ruina da Diferenca.png")
        matplot(ProbRuinaDifPad, type="l",main="Probabilidade de Ruina (diferenca)",lwd=2, ylab="Probabilidade de Ruina", xlab="Tempo", ylim=c(0,1),cex.axis=1.5,cex.lab=1.5, cex=1.5)
        dev.off()

        ##Grafico Tamanho da Ruina da diferenca
        png(width = 800,height = 800,"Tamanho da Ruina da Diferenca.png")
        matplot(TamRuinaDifPad, type="l",lwd=2,main="Tamanho da Ruina (diferenca)", ylab="Tamanho do deficit", xlab="Tempo", ylim=c(min(TamRuinaFundoPad),max(TamRuinaFundoPad)),cex.axis=1.5,cex.lab=1.5, cex=1.5)
        dev.off()

        ##Grafico Risco da diferenca
        ymaxR=max(TamRuinaDifPad*ProbRuinaDifPad);yminR=min(TamRuinaDifPad*ProbRuinaDifPad)
        png(width = 800,height = 800,"Risco da Diferenca.png")
        matplot(ProbRuinaDifPad*TamRuinaDifPad,main="Risco (diferenca) - Prob. da Ruina x Tamanho da Ruina", type="l",lwd=2, ylab="Risco", xlab="Tempo", ylim=c(yminR,ymaxR),cex.axis=1.5,cex.lab=1.5, cex=1.5)
        dev.off()

      }
      return(TRUE)
    }, error = function(e) {
      print(e)
      return(FALSE)
    })

}


