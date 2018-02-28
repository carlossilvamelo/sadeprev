#' SADEPrev function
#' @param value
#' @return value
#' @export

SADEPrev <- function() {
  library(readxl)
  library(doParallel)
  library(shiny)
  library(stats)
  shinyUI <- fluidPage(
    titlePanel("SADEPrev", "Projeto SADEPrev"),
    helpText(
      "Bem vindo ao SADEPrev, Simulador Atuarial-Demografico de regimes proprios  de previdencia  social.."
    ),
    hr(),
    helpText("ATENCAO:"),
    helpText(
      "1 - Todos os arquivos dos dados iniciais devem estar nomeados com os nomes padrao indicados no tutorial"
    ),
    helpText(
      "2 - Este formulario esta configurado com as configuracoes padrao, voce pode modificar cada opcao ou mante-las."
    ),
    hr(),
    selectInput(inputId = "TipoEntrada", "Selecione se deseja utilizar dados reais ou simular com dados aleatorios",
                choices = c("Usar dados reais","Usar dados ficticios"),
                width = 600),

    hr(),
    textInput(
      inputId = "caminhoDirEntrada",
      label = "Digite o caminho da pasta onde se encontram os dados:",
      value = "C:/sadeprev",
      width = 600
    ),
    sliderInput(
      "numeroRodadas",
      "Selecione o numero de rodadas/repeticoes a serem realizadas na simulacao:",
      min = 200,
      max = 20000,
      width = 600,
      value = 2000,
      step = 200
    ),
    sliderInput(
      "valorJuros",
      "Selecione a taxa dos juros anual: (em %)",
      width = 600,
      min = 0,
      max = 10,
      value = 6,
      step = 0.5
    ),
    textInput(
      inputId = "aumentoAnualSalarios",
      label = "Digite a taxa de aumento salarial anual\n                dos salarios dos servidores: (em %)",
      value = "1",
      width = 600
    ),
    h4(
      helpText(
        "Clique apenas uma vez no botao abaixo EXECUTAR, e aguarde o processamento do programa. Isto deve levar cerca de 40 segundos, nas configuracoes padrao de 2000 rodadas."
      )
    ),
    h4(textOutput("status")),
    hr(),
    actionButton(inputId = "btEnviar",
                 label = "Executar simulacao")
  )


  shinyServer <- function(input, output, session) {


    observeEvent(input$btEnviar, {
      {

        if (input$caminhoDirEntrada == "")
          caminhoEntrada = "C:/sadeprev"
        else
          caminhoEntrada = input$caminhoDirEntrada
        if (input$aumentoAnualSalarios == "")
          aumentoAnualSalarios = 0.06
        else
          aumentoAnualSalarios = as.numeric(input$aumentoAnualSalarios) / 100
        valorJuros = input$valorJuros / 100
      }
      dadosEntrada = carregaDadosRPPS(caminhoEntrada)
      if (!is.null(dadosEntrada)) {
        if (rodaSimulacao(
          dadosEntrada,
          caminhoEntrada,
          aumentoAnualSalarios,
          valorJuros,
          input$numeroRodadas,
          75,
          input$TipoEntrada #OPCAO DE DADOS FICTICIOS OU REAIS
        )) {
          # SE A SIMULAÃƒâ€¡ÃƒÆ’O RODOU MOSTRA ISSO
          output$status <- renderText({
            paste0(
              "SIMULACAO CONCLUIDA! OS RESULTADOS ESTAO SALVOS NA PASTA ",
              caminhoEntrada,
              "/Resultados EM UMA SUBPASTA COM A HORA ATUAL. VOCE JA PODE FECHAR O PROGRAMA.",
              sep = ""
            )
          })
          print(
            "Simulacao finalizada. Para simular outra vez, digite novamente o comando SADEPrev()"
          )

        } else{
          #SE NÃƒÆ’O RODOU MOSTRA ISSO
          output$status <- renderText({
            paste0(
              "Algum erro ocorreu durante a simulacao. Por favor, entre em contato com o suporte tecnico do SADEPrev."
            )
          })

        }


      } else{
        output$status <- renderText({
          paste0(
            "Ocorreu um erro durante o carregamento das planilhas dos dados. Por favor, verifique se as 4 planilhas estao na pasta padrao e se estao nomeados corretamente."
          )
        })

      }
    })
  }
  shinyApp(ui = shinyUI, server = shinyServer)
}