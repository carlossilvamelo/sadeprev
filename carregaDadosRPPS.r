#' carregaDadosRPPS function
#' @param value
#' @return value
#' @export
carregaDadosRPPS = function(caminhoDirEntrada){



  #funcÃµes auxiliares internas
  {
    pegarCelula<- function(tabela, coluna, index){
      return(tabela[[coluna]][index])
    }
    #funcao para tratar entrada de datas
    separarData<- function(Data){

      novaData = strsplit(as.character(Data), "-")
      dia = as.numeric(novaData[[1]][3])
      mes = as.numeric(novaData[[1]][2])
      ano = as.numeric(novaData[[1]][1])
      return(c(dia,mes,ano))
    }
    #funcao para converter string de situacao em numero
    converteSituacao <- function(situacao){
      situacoes <- c("Ativo", "Aposentado", "Invalido", "Pensionista conjuge", "Pensionista filho")

      for(i in 1:5){
        if(situacoes[i] == situacao)
          return(i)
      }

      return(1)
    }
  }

  #verificando entradas configuradas
  if(caminhoDirEntrada == "")
    caminhoDirEntrada = "C:/SADEPrev"


  {#posicoes das colunas tabela SERVIDOR
    serv_dataNascimento = 1
    serv_sexo = 2
    serv_situacao = 3
    serv_tempoContribuicaoAnterior = 4
    serv_dataIngressoEnte = 5
    serv_salarioContribuicao =6

  }
  {#posicoes das colunas tabela RPPS
    rpps_dataCriacao = 1
    rpps_fundoReservaAtual = 2
    rpps_aliquotaContribuicaoServidor = 3
    rpps_aliquotaContribuicaoEnte = 4


  }


  #tratamento de excessao de caminho de arquivo
  result = tryCatch({
    #importando as planilhas para dataframes

    tabelaServidor <- read_excel(paste(caminhoDirEntrada,"/servidor.xlsx",sep = ""))
    tabelaRpps <- read_excel(paste(caminhoDirEntrada,"/rpps.xlsx",sep = ""))
    {#excluindo as colunas desnecessarias ao programa
      tabelaServidor = tabelaServidor[c(-1,-2)]
      tabelaRpps =tabelaRpps[c(-1,-2)]
    }


    #pegando data atual data atual
    dataAtual = strsplit(as.character(Sys.Date()) , "-")
    diaAtual = as.numeric(dataAtual[[1]][3])
    mesAtual = as.numeric(dataAtual[[1]][2])
    anoAtual = as.numeric(dataAtual[[1]][1])


    #Vetor com os dados das idades que seram inseridos na tabela como coluna - #tabelaServidor
    idadesServidores <- c()
    sexoServidor <- c()
    situacaoNoRpps <- c()
    #Vetor com os dados os tempos de ingresso que seram inseridos na tabela como coluna - #tabelaServidor
    ingressoEnteFederativo <- c()


    #vetor com os dados do tempo de criacao do rpps - #tabelaRpps
    tempoCriacaoRpps <- c()

    #constantes
    masculino = 1
    feminino = 2
    #constantes de situacao
    ativo = 1
    aposentado = 2
    invalido = 3
    pensionistaConjuge = 4
    pensionistaFilho = 5

    numColunasTabelaRpps = dim(tabelaRpps)[1]
    numColunasTabelaServidor = dim(tabelaServidor)[1]

    numMaxColuna =  sort(c(numColunasTabelaRpps, numColunasTabelaServidor))[2]
    for (index in 1:numMaxColuna){

      if(index <= numColunasTabelaServidor){

        #convertendo stiring de situacao em numero
        situacaoNoRpps[index] = converteSituacao(pegarCelula(tabelaServidor,serv_situacao,index))
        #convertendo string sexo em numero
        if(pegarCelula(tabelaServidor, serv_sexo, index) == "Masculino")
          sexoServidor[index] = masculino
        else
          sexoServidor[index] = feminino

        dataNascimentoServidor = separarData(pegarCelula(tabelaServidor,serv_dataNascimento, index))
        #converte data de nascimento em idade
        idadesServidores[index] = anoAtual - dataNascimentoServidor[3]#ano
        #correcao para menos

        if(dataNascimentoServidor[2] >= mesAtual){
          if(dataNascimentoServidor[2] == mesAtual){

            #verifica dias
            if(dataNascimentoServidor[1] > diaAtual)
              idadesServidores[index] = idadesServidores[index] - 1

          }else
            idadesServidores[index] = idadesServidores[index] - 1
        }


        dataIngressoEnteFederativo = separarData(pegarCelula(tabelaServidor,serv_dataIngressoEnte, index))
        #converter data de ingresso ente federativo em tempo de ingresso
        ingressoEnteFederativo[index] = anoAtual - dataIngressoEnteFederativo[3]#ano
        #correcao
        #if(dataIngressoEnteFederativo[2] >= mesAtual){
        #
        #  if(dataIngressoEnteFederativo[2] == mesAtual){
        #    #verifica dias
        #    if(dataIngressoEnteFederativo[1] > diaAtual)
        #      ingressoEnteFederativo[index] = ingressoEnteFederativo[index] - 1
        #
        #  }else
        #    ingressoEnteFederativo[index] = ingressoEnteFederativo[index] - 1
        #
        #}

      }


      #converter data de criacao do rpps em tempo de criacao - tabelaRPPS
      if(index <= 1){
        dataCriacaoRPPS = separarData(pegarCelula(tabelaRpps,rpps_dataCriacao,index))
        tempoCriacaoRpps[index] = anoAtual - dataCriacaoRPPS[3]#ano
        #correcao
        if(dataCriacaoRPPS[2] >= mesAtual){

          if(dataCriacaoRPPS[2] == mesAtual){
            #verifica dias
            if(dataCriacaoRPPS[1] > diaAtual)
              tempoCriacaoRpps[index] = tempoCriacaoRpps[index] - 1

          }else
            tempoCriacaoRpps[index] = tempoCriacaoRpps[index] - 1

        }
      }

    }#lac

    ################ Inclus?es nas tabelas
    tabelaServidor[[serv_sexo]] <- NULL
    ############## exclusÃµes nas tabelas
    #excluir colunas de data de nascimento e colunas data de ingresso do ente federativo - tabelaServidor
    tabelaServidor[[serv_dataNascimento]] <- NULL
    tabelaServidor[[serv_dataIngressoEnte]] <- NULL
    tabelaServidor[[serv_situacao]] <-NULL


    #inserindo novas colunas na tabelaServidor
    tabelaServidor$idades.servidores <- idadesServidores #add coluna no data frame
    tabelaServidor$Sexo.do.servidor <- sexoServidor
    tabelaServidor$Situacao.no.RPPS <- situacaoNoRpps
    tabelaServidor$tempo.ingresso.ente.federativo<- ingressoEnteFederativo
    #inserindo nova coluna na tabelaRpps
    #excluir colunas data de criacao do rpps - tabelaRpps
    tabelaRpps[[rpps_dataCriacao]] <- NULL
    tabelaRpps$tempo.criacao <- tempoCriacaoRpps

    print("Dados carregados com sucesso.")
    tabelaServidor = tabelaServidor[-1]
    names(tabelaServidor) = c("TempoRGPS","Salario","Idade","Sexo","EstadoInicial","TempoServicoPublico")
    names(tabelaRpps) = c("FundoAtual","AliquotaServidor","AliquotaEnte","TempoCriacaoRPPS")
    return(list(tabelaServidor, tabelaRpps))

  }, error = function(e) {
    print("Nao foi poss?vel carregar algum arquivo de entrada, verifique os nomes de caminho do diretor?o e do arquivo e tente novamente.")
    print(e)
    return(NULL)
  })
}